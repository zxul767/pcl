(in-package :dev.zxul767.mp3-database)

;; TODO: this is duplicated now in two packages, consolidate!
(defmacro fn-and (&rest functions)
  `#'(lambda (&rest args)
       (and ,@(loop for fn in functions collect `(apply #',fn args)))))

(defclass table ()
  ((rows :accessor rows :initarg :rows :initform (make-rows))
   (schema :accessor schema :initarg :schema)))

(defparameter *default-table-size* 1000)

(defun make-rows (&optional (size *default-table-size*))
  (make-array size :adjustable t :fill-pointer 0))

(defclass column ()
  ((name
    :reader name
    :initarg :name)

   (equality-predicate
    :reader equality-predicate
    :initarg :equality-predicate)

   (comparator
    :reader comparator
    :initarg :comparator)

   (default-value
    :reader default-value
    :initarg :default-value
    :initform nil)

   (value-normalizer
    :reader value-normalizer
    :initarg :value-normalizer
    :initform #'(lambda (value column) (declare (ignore column)) value))))

(defgeneric make-column (name type &optional default-value))

(defmethod make-column (name (type (eql 'string)) &optional default-value)
  (make-instance
   'column
   :name name
   :comparator #'string<
   :equality-predicate #'string=
   :default-value default-value
   :value-normalizer #'normalize-string))

(defmethod make-column (name (type (eql 'number)) &optional default-value)
  (make-instance
   'column
   :name name
   :comparator #'<
   :equality-predicate #'=
   :default-value default-value))

(defun ensure-non-nullable (value column)
  (or value (error "Column ~a can't be null" (name column))))

(defun normalize-string (value column)
  (string-downcase
   (string-trim '(#\Space)
                (ensure-non-nullable value column))))

(defclass interned-values-column (column)
  ((interned-values
    :reader interned-values
    :initform (make-hash-table :test #'equal))
   (equality-predicate :initform #'eql)
   (value-normalizer :initform #'intern-for-column)))

(defun intern-for-column (value column)
  (let ((hash (interned-values column))
        (value (normalize-string value column)))
    (or (gethash value hash)
        (setf (gethash value hash) value))))

(defmethod make-column (name (type (eql 'interned-string)) &optional default-value)
  (make-instance
   'interned-values-column
   :name name
   :comparator #'string<
   :default-value default-value))

(defun make-schema (spec)
  (mapcar #'(lambda (column-spec) (apply #'make-column column-spec)) spec))

(defun create-mp3-schema ()
  (make-schema
   '((:file string)
     (:genre interned-string "Unknown")
     (:artist interned-string "Unknown")
     (:album interned-string "Unknown")
     (:song string)
     (:track number 0)
     (:year number 0)
     (:id3-size number))))

(defparameter *mp3s* (create-mp3-table))

(defun create-mp3-table ()
  (make-instance 'table :schema (create-mp3-schema)))

(defun insert-row (names-and-values table)
  (when names-and-values
    (vector-push-extend (normalize-row names-and-values (schema table))
                        (rows table))))

(defun normalize-row (names-and-values schema)
  (loop
    for column in schema
    for name = (name column)
    for value = (or (getf names-and-values name)
                    (default-value column))
    collect name
    collect (normalize-for-column value column)))

(defun normalize-for-column (value column)
  (funcall (value-normalizer column) value column))

(defun file->row (file)
  (when-bind ((id3 (read-id3 file)))
    (list
     :file (namestring (truename file))
     :genre (translated-genre id3)
     :artist (artist id3)
     :album (album id3)
     :song (or (song id3) "")
     :track (parse-track (track id3))
     :year (parse-year (year id3))
     :id3-size (size id3))))

(defun try-parse-integer (text &rest keywords &key &allow-other-keys)
  (when-bind ((text (and text (string-trim '(#\Space) text))))
    (apply #'parse-integer text :junk-allowed t keywords)))

(defun parse-track (track)
  (try-parse-integer track :end (position #\/ track)))

(defun parse-year (year)
  (try-parse-integer year))

(defun load-database (directory database)
  (delete-all-rows database)
  ;; the interned values in some of the columns are populated dynamically
  ;; so we need to recreate the schema to get rid of the old values
  (setf (schema database) (create-mp3-schema))
  (let ((count 0))
    (walk-directory
     directory
     #'(lambda (file)
         (princ #\.)
         (when (insert-row (file->row file) database)
           (incf count)))
     :recursively t
     :file-condition (fn-and mp3-p id3-p))
    (format t "~&Loaded ~d files into database." count)))

(defun select (&key (columns t) from where distinct order-by)
  (let ((rows (rows from))
        (schema (schema from)))
    (when where
      (setf rows (restrict-rows rows where)))
    (unless (eql columns 't)
      (setf schema (extract-schema (ensure-list columns) schema))
      (setf rows (project-columns rows schema)))
    (when order-by
      (setf rows (sorted-rows rows schema (ensure-list order-by))))
    (when distinct
      (setf rows (distinct-rows rows schema)))
    (make-instance 'table :rows rows :schema schema)))

(defun sorted-rows (rows schema order-by)
  (sort (copy-seq rows) (row-comparator order-by schema)))

(defun row-equality-tester (schema)
  (let ((names (mapcar #'name schema))
        (tests (mapcar #'equality-predicate schema)))
    #'(lambda (a b)
        (loop for name in names and test in tests
              always (funcall test (getf a name) (getf b name))))))

(defun row-comparator (column-names schema)
  (let ((comparators (mapcar #'comparator (extract-schema column-names schema))))
    #'(lambda (a b)
        (loop
          for name in column-names
          for comparator in comparators
          for a-value = (getf a name)
          for b-value = (getf b name)
          when (funcall comparator a-value b-value)
            return t
          when (funcall comparator b-value a-value)
            return nil
          finally (return nil)))))

(defun restrict-rows (rows where)
  (remove-if-not where rows))

(defun matching (table &rest names-and-values)
  "Build a 'where' function that matches rows with the given column values."
  (let ((matchers (column-matchers (schema table) names-and-values)))
    #'(lambda (row)
        (every #'(lambda (matcher) (funcall matcher row)) matchers))))

(defun column-matchers (schema names-and-values)
  (loop for (name value) on names-and-values by #'cddr
        when value collect (column-matcher (find-column name schema) value)))

(defun column-matcher (column value)
  (let ((name (name column))
        (predicate (equality-predicate column))
        (normalized-value (normalize-for-column value column)))
    #'(lambda (row) (funcall predicate (getf row name) normalized-value))))

(defun extract-schema (column-names schema)
  (loop for name in column-names collect (find-column name schema)))

(defun find-column (name schema)
  (or (find name schema :key #'name)
      (error "No column: ~a in schema: ~a" name schema)))

(defun ensure-list (thing)
  (if (listp thing) thing (list thing)))

(defun project-columns (rows schema)
  (map 'vector (extractor schema) rows))

(defun distinct-rows (rows schema)
  (remove-duplicates rows :test (row-equality-tester schema)))

(defun extractor (schema)
  (let ((names (mapcar #'name schema)))
    #'(lambda (row)
        (loop for name in names collect name collect (getf row name)))))

(defmacro do-rows ((row table) &body body)
  `(loop for ,row across (rows ,table) do ,@body))

(defmacro with-column-values ((&rest vars) row &body body)
  (once-only (row)
    `(let ,(column-bindings vars row) ,@body)))

(defun column-bindings (vars row)
  (loop for var in vars collect `(,var (column-value ,row ,(as-keyword var)))))

(defun as-keyword (symbol)
  (intern (symbol-name symbol) :keyword))

(defun column-value (row column-name)
  (getf row column-name))

(defun delete-all-rows (table)
  (setf (rows table) (make-rows *default-table-size*)))

(defun table-size (table)
  (length (rows table)))

;; ----------------------------------------------------------------------------
;; testing ground
;; ----------------------------------------------------------------------------
(defun enumerate-column-values (column-name schema)
  (when-bind ((column (find-column column-name schema)))
    (unless (slot-exists-p column 'interned-values)
      (error "Column ~a does not have a fixed set of values!" column))
    (with-slots (interned-values) column
      (loop for key being each hash-key of interned-values collect key))))

(defun enumerate-artists (schema)
  (enumerate-column-values :artist schema))

(defun enumerate-genres (schema)
  (enumerate-column-values :genre schema))

(defun run-query (predicate)
  (print-rows (select :columns '(:song :artist :album)
                      :from *mp3s*
                      :where predicate
                      :distinct t
                      :order-by '(:album :song))))

;; FIXME: generalize to print all columns in the schema associated with `rows'
;;        (which happens to be a table, so it always has a schema object attached)
(defun print-rows (rows)
  (let ((output-format "~40a|~20a|~20a~%"))
    (format t output-format "SONG" "ARTIST" "ALBUM")
    (format t "~80,,,'-@a~%" "")
    (do-rows (row rows)
      (with-column-values (song artist album) row
        (format t output-format song artist album)))
    (when (> (table-size rows) 10)
      (format t "~80,,,'-@a~%" "")
      (format t output-format "SONG" "ARTIST" "ALBUM"))))
