(in-package :dev.zxul767.spam)

(defparameter *max-ham-score* .4)
(defparameter *min-spam-score* .6)
(defparameter *corpus* (make-array 1000 :adjustable t :fill-pointer 0))
(defparameter *corpus-encoding* :iso-8859-1)
(defparameter *max-file-chars* (* 10 1024)) ;; 10Kb

(defvar *feature-database* (make-hash-table :test #'equal))
(defvar *total-spams* 0)
(defvar *total-hams* 0)

(defclass word-feature ()
  ((word
    :initarg :word
    :accessor word
    :initform (error "Missing mandatory argument :word")
    :documentation "The word this feature represents.")
   (spam-count
    :initarg :spam-count
    :accessor spam-count
    :initform 0
    :documentation "Number of spams we have seen this feature in.")
   (ham-count
    :initarg :ham-count
    :accessor ham-count
    :initform 0
    :documentation "Number of hams we have seen this feature in.")))

(defun classify (text)
  (let ((score (score (extract-features text))))
    (values (classification score) score)))

(defun classification (score)
  (cond
    ((<= score *max-ham-score*) 'ham)
    ((>= score *min-spam-score*) 'spam)
    (t 'unsure)))

(defun intern-feature (word)
  (or (gethash word *feature-database*)
      (setf (gethash word *feature-database*)
            (make-instance 'word-feature :word word))))

(defun extract-features (text)
  (mapcar #'intern-feature (extract-words text)))

(defun extract-words (text)
  (delete-duplicates
   (cl-ppcre:all-matches-as-strings "[a-zA-Z]{3,}" text)
   :test #'string=))

(defun increment-total-count (type)
  (ecase type
    (ham (incf *total-hams*))
    (spam (incf *total-spams*))))

(defun train (text type)
  (dolist (feature (extract-features text))
    (increment-count feature type))
  (increment-total-count type))

(defun increment-count (feature type)
  (ecase type
    (ham (incf (ham-count feature)))
    (spam (incf (spam-count feature)))))

(defun spam-probability (feature)
  (with-slots (spam-count ham-count) feature
    (let ((spam-frequency (/ spam-count (max 1 *total-spams*)))
          (ham-frequency (/ ham-count (max 1 *total-hams*))))
      (/ spam-frequency (+ spam-frequency ham-frequency)))))

(defun bayesian-spam-probability (feature
                                  &optional (assumed-probability 1/2) (weight 1))
  (let ((basic-probability (spam-probability feature))
        (data-points (+ (spam-count feature) (ham-count feature))))
    (/ (+ (* weight assumed-probability)
          (* data-points basic-probability))
       (+ weight data-points))))

(defun score (features)
  (let ((spam-probs ()) (ham-probs ()))
    (dolist (feature features)
      (if (trained-p feature)
          (let ((spam-prob (float (bayesian-spam-probability feature) 0.0d0)))
            (push spam-prob spam-probs)
            (push (- 1.0d0 spam-prob) ham-probs))))
    (let ((h (- 1 (fisher spam-probs)))
          (s (- 1 (fisher ham-probs))))
      (/ (+ (- 1 h) s)
         2.0d0))))

(defun fisher (probs)
  (inverse-chi-square
   (* -2 (reduce #'+ probs :key #'log))
   (* 2 (length probs))))

(defun inverse-chi-square (value degrees-of-freedom)
  (assert (evenp degrees-of-freedom))
  (min
   1.0
   (loop with m = (/ value 2)
         for i below (/ degrees-of-freedom 2)
         for prob = (exp (- m)) then (* prob (/ m i))
         summing prob)))

(defun trained-p (feature)
  (not (untrained-p feature)))

(defun untrained-p (feature)
  (with-slots (spam-count ham-count) feature
    (and (zerop spam-count) (zerop ham-count))))

(defun add-file-to-corpus (filename type corpus)
  (vector-push-extend `(,filename ,type ,encoding) corpus))

(defun add-directory-to-corpus (directory type corpus &key recursively)
  (walk-directory directory
                  #'(lambda (filename) (add-file-to-corpus filename type corpus))
                  :recursively recursively))

(defun test-classifier (corpus &key (testing-fraction 0.2))
  (clear-database)
  (let* ((shuffled (shuffle-vector corpus))
         (size (length corpus))
         (train-on (floor (* size (- 1 testing-fraction)))))
    (train-from-corpus shuffled :start 0 :end train-on)
    (test-from-corpus shuffled :start train-on)))

(defun train-from-corpus (corpus &key (start 0) end)
  (loop for i from start below (or end (length corpus)) do
    (destructuring-bind (file type) (aref corpus i)
      (train (start-of-file file *max-file-chars* :encoding *corpus-encoding*)
             type))))

(defun test-from-corpus (corpus &key (start 0) end)
  (loop for i from start below (or end (length corpus))
        collect (destructuring-bind (file type) (aref corpus i)
                  (multiple-value-bind (classification score)
                      (classify (start-of-file file
                                               *max-file-chars*
                                               :encoding *corpus-encoding*))
                    (list
                     :file file
                     :type type
                     :classification classification
                     :score score)))))

(defun nshuffle-vector (vector)
  (loop for i downfrom (1- (length vector)) to 1
        for j = (random (1+ i))
        do (unless (= i j)
             (rotatef (aref vector i) (aref vector j))))
  vector)

(defun shuffle-vector (vector)
  (nshuffle-vector (copy-seq vector)))

(defun start-of-file (filename max-chars &key (encoding :utf-8))
  (with-open-file (in filename :external-format encoding)
    (let* ((length (min (file-length in) max-chars))
           (text (make-string length))
           (read (read-sequence text in)))
      (if (< read length)
          (subseq text 0 read)
          text))))

(defun result-type (result)
  (destructuring-bind (&key type classification &allow-other-keys) result
    (ecase type
      (ham
       (ecase classification
         (ham 'correct)
         (spam 'false-positive)
         (unsure 'missed-ham)))
      (spam
       (ecase classification
         (ham 'false-negative)
         (spam 'correct)
         (unsure 'missed-spam))))))

(defun false-positive-p (result)
  (eql (result-type result) 'false-positive))

(defun false-negative-p (result)
  (eql (result-type result) 'false-negative))

(defun missed-ham-p (result)
  (eql (result-type result) 'missed-ham))

(defun missed-spam-p (result)
  (eql (result-type result) 'missed-spam))

(defun correct-p (result)
  (eql (result-type result) 'correct))

(defun analyze-results (results)
  (let* ((keys '(total correct false-positive false-negative missed-ham missed-spam))
         (counts (loop for key in keys collect (cons key 0))))
    (dolist (result results)
      (incf (cdr (assoc 'total counts)))
      (incf (cdr (assoc (result-type result) counts))))
    (loop with total = (cdr (assoc 'total counts))
          for (label . count) in counts
          do (format t "~&~@(~a~):~20t~5d~,5t: ~6,2f%~%"
                     label count (* 100 (/ count total))))))

(defun explain-classification (filepath &key (encoding :utf-8))
  (let* ((text (start-of-file filepath *max-file-chars* :encoding encoding))
         (features (extract-features text))
         (score (score features))
         (classification (classification score)))
    (show-summary filepath text classification score)
    (dolist (feature (sorted-interesting features))
      (show-feature feature))))

(defun show-summary (filepath text classification score)
  (format t "~&~a" filepath)
  (format t "~2%~a~2%" text)
  (format t "Classified as ~a with score of ~,5f~%" classification score))

(defun show-feature (feature)
  (with-slots (word ham-count spam-count) feature
    (format
     t "~&~2t~a~30t hams: ~5d; spam: ~5d;~,10t probability: ~,f~%"
     word ham-count spam-count (bayesian-spam-probability feature))))

(defun sorted-interesting (features)
  (sort (remove-if #'untrained-p features) #'< :key #'bayesian-spam-probability))

;; customize the string representation of `word-feature'
(defmethod print-object ((object word-feature) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (word ham-count spam-count) object
      (format stream "~s :hams ~d :spams ~d" word ham-count spam-count))))

(defun clear-database ()
  (setf
   *feature-database* (make-hash-table :test #'equal)
   *total-spams* 0
   *total-hams* 0))
