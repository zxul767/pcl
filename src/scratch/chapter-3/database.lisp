(defvar *records* nil)

(defun make-record (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defun add-record (record)
  (push record *records*))

(defun dump-records ()
  (dolist (record *records*)
    (format t "~{~a:~10t~a~%~}~%" record)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-record ()
  (make-record
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped:")))

(defun add-records ()
  (loop (add-record (prompt-for-record))
        (if (not (y-or-n-p "Another?")) (return))))

(defun save-records (filename)
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (print *records* out))))

(defun load-records (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *records* (read in)))))

(defun select (condition)
  (remove-if-not condition *records*))

;; this version is not very flexible, as it hard-codes the names of the fields
;; for this particular application:
;;
;; (defun where (&key title artist rating (ripped nil ripped-p))
;;   #'(lambda (record)
;;       (and
;;        (if title (equal (getf record :title) title) t)
;;        (if artist (equal (getf record :artist) artist) t)
;;        (if rating (equal (getf record :rating) rating) t)
;;        (if ripped-p (equal (getf record :ripped) ripped) t))))

;; this version is more flexible, as it expands at compilation time into the
;; requested fields only:
;;
;; (defmacro where (&rest clauses)
;;   (labels ((generate-comparisons (fields)
;;              (loop while fields collect (generate-comparison (pop fields) (pop fields))))
;;            (generate-comparison (field value)
;;              `(equal (getf record ,field) ,value)))
;;     `#'(lambda (record) (and ,@(generate-comparisons clauses)))))

;; though i'm a fan of decomposing functions with a lot of detail into smaller functions
;; to improve readability--and this particular function seems to do that just fine--i think
;; that the following version may actually be better, as it keeps the code terse without
;; becoming cryptic:

(defmacro where (&rest clauses)
  (labels ((generate-comparisons (fields)
             (loop while fields collect `(equal (getf record ,(pop fields)) ,(pop fields)))))
    `#'(lambda (record) (and ,@(generate-comparisons clauses)))))

;; this is the original version in the book, which has the same problem as the original
;; version of `where':
;;
;; (defun update (condition &key title artist rating (ripped nil ripped-p))
;;   (setf *records*
;;         (mapcar
;;          #'(lambda (record)
;;              (when (funcall condition record)
;;                (if title (setf (getf record :title) title))
;;                (if artist (setf (getf record :artist) artist))
;;                (if rating (setf (getf record :rating) rating))
;;                (if ripped-p (setf (getf record :ripped) ripped)))
;;              record)
;;          *records*)))

;; and here's the corresponding macro to make it more flexible:
(defmacro update (condition &rest clauses)
  (labels ((generate-assignments (fields)
             (loop while fields collect `(setf (getf record ,(pop fields)) ,(pop fields)))))
    `(setf *records*
           (mapcar #'(lambda (record)
                       (when (funcall ,condition record)
                         ,@(generate-assignments clauses)))
                   *records*))))

(defun delete-records (condition)
  (setf *records* (remove-if condition *records*)))
