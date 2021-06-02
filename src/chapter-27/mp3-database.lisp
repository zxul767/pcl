(in-package :dev.zxul767.mp3-database)

(defparameter *default-table-size* 1000)

(defclass table ()
  ((rows :accessor rows :initarg :rows :initform (make-rows))
   (schema :accessor schema :initarg :schema)))

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

(defclass interned-values-column (column)
  ((interned-values
    :reader interned-values
    :initform (make-hash-table :test #'equal))
   (equality-predicate :initform #'eql)
   (value-normalizer :initform #'intern-for-column)))

(defun make-rows (&optional (size *default-table-size*))
  (make-array size :adjustable t :fill-pointer 0))

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

(defmethod make-column (name (type (eql 'interned-string)) &optional default-value)
  (make-instance
   'interned-values-column
   :name name
   :comparator #'string<
   :default-value default-value))

(defun make-schema (spec)
  (mapcar #'(lambda (column-spec) (apply #'make-column column-spec)) spec))

(defun create-mp3-table ()
  (make-instance 'table :schema (create-mp3-schema)))

(defun create-mp3-schema ()
  (make-schema
   '((:file string)
     (:genre interned-string "Unknown")
     (:artist interned-string "Unknown")
     (:album interned-string "Unknown")
     (:song string "")
     (:track number 0)
     (:year number 0)
     (:id3-size number))))

(defun file->row (file)
  (when-bind ((id3 (read-id3 file)))
    (list
     :file (namestring (truename file))
     :genre (translated-genre id3)
     :artist (artist id3)
     :album (album id3)
     :song (song id3)
     :track (try-parse-track (track id3))
     :year (try-parse-year (year id3))
     :id3-size (size id3))))

(defun load-database (directory database)
  (delete-all-rows database)
  ;; the interned values in some of the columns are populated dynamically
  ;; so we need to recreate the schema to get rid of the old values
  (setf *mp3-schema* (create-mp3-schema))
  (setf (schema database) *mp3-schema*)
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

(defun remove-columns (table columns)
  (if (not columns)
      table
      (let ((rows (rows table))
            (schema (schema table)))
        (setf schema (remove-columns-from-schema (ensure-list columns) schema))
        (setf rows (project-columns rows schema))
        (make-instance 'table :rows rows :schema schema))))

(defun remove-columns-from-schema (column-names schema)
  (remove-if #'(lambda (column) (member (name column) column-names))
             schema))

(defun in (column-name table)
  (let ((test (equality-predicate (find-column column-name (schema table))))
        (values (map 'list #'(lambda (row) (column-value row column-name)) (rows table))))
    #'(lambda (row)
        (member (column-value row column-name) values :test test))))

(defun matching (table &rest names-and-values)
  (let ((matchers (column-matchers (schema table) names-and-values)))
    #'(lambda (row)
        (every #'(lambda (matcher) (funcall matcher row)) matchers))))

(defun delete-rows (&key from where)
  (loop
    with rows = (rows from)
    with store-index = 0
    for read-index from 0
    for row across rows
    do (setf (aref rows read-index) nil)
    unless (funcall where row) do
      (setf (aref rows store-index) row)
      (incf store-index)
    finally (setf (fill-pointer rows) store-index)))

(defun sort-rows (table &rest column-names)
  (setf (rows table)
        (sort (rows table) (row-comparator column-names (schema table))))
  table)

(defun shuffle-table (table)
  (nshuffle-vector (rows table))
  table)

;; FIXME: this is now duplicated in the `spam' package as well; move to common
;; utilities package
(defun nshuffle-vector (vector)
  (loop for i downfrom (1- (length vector)) to 1
        for j = (random (1+ i))
        do (unless (= i j)
             (rotatef (aref vector i) (aref vector j))))
  vector)

(defun shuffle-vector (vector)
  (nshuffle-vector (copy-seq vector)))

(defun random-selection (table n)
  (make-instance
   'table
   :schema (schema table)
   :rows (nshuffle-vector (random-sample (rows table) n))))

(defun random-sample (vector n)
  "Based on Algorithm S from Knuth. TAOCP, vol. 2. p. 142"
  (loop with selected = (make-array n :fill-pointer 0)
        for index from 0
        do
           (loop
             with to-select = (- n (length selected))
             for remaining = (- (length vector) index)
             while (>= (* remaining (random 1.0)) to-select)
             do (incf index))
           (vector-push (aref vector index) selected)
        when (= (length selected) n) return selected))

(defun delete-all-rows (table)
  (setf (rows table) (make-rows *default-table-size*)))

(defun table-size (table)
  (length (rows table)))

(defun nth-row (n table)
  (aref (rows table) n))

(defun column-names (table)
  (mapcar #'name (schema table)))

(defun ensure-non-nullable (value column)
  (or value (error "Column ~a can't be null" (name column))))

(defun normalize-string (value column)
  (string-downcase
   (trim-whitespace
    (ensure-non-nullable value column))))

(defun intern-for-column (value column)
  (let ((hash (interned-values column))
        (value (normalize-string value column)))
    (or (gethash value hash)
        (setf (gethash value hash) value))))

(defun insert-row (names-and-values table)
  (when names-and-values
    (vector-push-extend (normalize-row names-and-values (schema table))
                        (rows table))))

(defun normalize-row (names-and-values schema)
  (loop
    for column in schema
    for name = (name column)
    for value = (or (column-value names-and-values name)
                    (default-value column))
    collect name
    collect (normalize-for-column value column)))

(defun normalize-for-column (value column)
  (funcall (value-normalizer column) value column))

(defun try-parse-integer (text &rest keywords &key &allow-other-keys)
  (when-bind ((text (and text (trim-whitespace text))))
    (apply #'parse-integer text :junk-allowed t keywords)))

(defun try-parse-track (track)
  (try-parse-integer track :end (position #\/ track)))

(defun try-parse-year (year)
  (try-parse-integer year))

(defun trim-whitespace (string)
  (string-trim '(#\Space) string))

(defun sorted-rows (rows schema order-by)
  (sort (copy-seq rows) (row-comparator order-by schema)))

(defun row-equality-tester (schema)
  (let ((column-names (mapcar #'name schema))
        (equality-tests (mapcar #'equality-predicate schema)))
    #'(lambda (row-a row-b)
        (loop for column-name in column-names and equals in equality-tests
              always (funcall equals
                              (column-value row-a column-name)
                              (column-value row-b column-name))))))

(defun row-comparator (column-names schema)
  (let ((column-comparators (mapcar #'comparator (extract-schema column-names schema))))
    #'(lambda (row-a row-b)
        (loop
          for column-name in column-names
          for column-comparator in column-comparators
          for a-value = (column-value row-a column-name)
          for b-value = (column-value row-b column-name)
          when (funcall column-comparator a-value b-value)
            return t
          when (funcall column-comparator b-value a-value)
            return nil
          finally (return nil)))))

(defun restrict-rows (rows where)
  (remove-if-not where rows))

(defun column-matchers (schema names-and-values)
  (loop for (name value) on names-and-values by #'cddr
        when value collect (column-matcher (find-column name schema) value)))

(defun column-matcher (column value)
  (let ((name (name column))
        (predicate (equality-predicate column))
        (normalized-value (normalize-for-column value column)))
    #'(lambda (row) (funcall predicate (column-value row name) normalized-value))))

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
        (loop for name in names collect name collect (column-value row name)))))

(defmacro do-rows ((row table &key max-rows) &body body)
  (once-only (max-rows table)
    `(loop for ,row across (subseq (rows ,table) 0 ,max-rows) do ,@body)))

(defun map-rows (fn table)
  (loop for row across (rows table) collect (funcall fn row)))

(defmacro with-column-values ((&rest vars) row &body body)
  (with-labels
      (once-only (row)
        `(let ,(column-bindings vars row) ,@body))

    (column-bindings (vars row)
      (loop for var in vars collect `(,var (column-value ,row ,(as-keyword var)))))))

(defun column-value (row column-name)
  (getf row column-name))

(defun is-categorical (column)
  (slot-exists-p column 'interned-values))

(defun is-numerical (column)
  (and (eq (comparator column) #'<)
       (eq (equality-predicate column) #'=)))

;; ----------------------------------------------------------------------------
;; testing ground
;; ----------------------------------------------------------------------------
(defun enumerate-column-values (column-name schema)
  (when-bind ((column (find-column column-name schema)))
    (unless (is-categorical column)
      (error "Column ~a does not have a fixed set of values!" column))
    (with-slots (interned-values) column
      (loop for key being each hash-key of interned-values collect key))))

(defun enumerate-artists (table)
  (enumerate-column-values :artist (schema table)))

(defun enumerate-genres (table)
  (enumerate-column-values :genre (schema table)))

(defun run-query (table predicate)
  (print-table (select :columns '(:song :artist :album)
                      :from table
                      :where predicate
                      :distinct t
                      :order-by '(:album :song))))

(defparameter *default-rows-for-sample-display* 5)

(defun print-table (table &key exclude-columns include-all-rows)
  (unless (= (table-size table) 0)
    (let* ((table (remove-columns table exclude-columns))
           (max-rows (calculate-rows-count table include-all-rows))
           (column-widths (estimate-column-widths table :max-rows max-rows))
           (row-layout (create-row-layout column-widths)))
      (format t "Showing ~d of ~d rows~2%" max-rows (table-size table))
      (apply #'format t row-layout (plist-keys column-widths))
      (format t "~a~%" (str:repeat (total-width column-widths) "-"))
      (do-rows (row table :max-rows max-rows)
        (apply #'format t row-layout (plist-values row)))
      (if (null include-all-rows)
          (format t "...~%")))))

(defun calculate-rows-count (table show-all-rows)
  (let ((max-rows (table-size table)))
    (if show-all-rows
        max-rows
        (min max-rows *default-rows-for-sample-display*))))

(defun estimate-column-widths (table &key max-rows)
  (loop for column-name in (column-names table)
        collect column-name
        collect (estimate-column-width column-name table :max-rows max-rows)))

;; TODO: consider optimizing this by doing a single pass over the rows;
;;       the current implementation traverses all rows for each 'string'
;;       column present in `table'
(defun estimate-column-width (column-name table &key max-rows)
  (when-bind ((column (find-column column-name (schema table))))
    (1+
     (cond
       ((is-categorical column)
        (max
         (length (symbol-name column-name))
         (longest-string-length (hash-keys (interned-values column)))))
       ((is-numerical column)
        (max
         (length (symbol-name column-name))
         (ceiling (log (largest-number column-name table) 10))))
       (t
        (max
         (length (symbol-name column-name))
         (longest-length column-name table :max-rows max-rows)))))))

(defun create-row-layout (column-widths)
  (loop for (_ value) on column-widths by #'cddr
        collecting (format nil "~~~aa" value) into results
        finally (return (format nil "~a~~%" (str:join "|" results)))))

(defun total-width (widths)
  (loop for (column width) on widths by #'cddr
        summing width into total-width
        finally (return (+ total-width (/ (length widths) 2)))))

(defun plist-values (plist)
  (loop for (_ value) on plist by #'cddr collect value))

(defun plist-keys (plist)
  (loop for (key _) on plist by #'cddr collect key))

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defun largest-number (column-name table)
  (reduce #'max (rows table)
          :key #'(lambda (row) (column-value row column-name))))

(defun longest-length (column-name table &key max-rows)
  (reduce #'max (subseq (rows table) 0 max-rows)
          :key #'(lambda (row) (length (column-value row column-name)))))

(defun longest-string-length (strings)
  (reduce #'max strings :key #'length))

(defparameter *mp3-table* (create-mp3-table))

(defparameter *mp3-schema* (create-mp3-schema))
