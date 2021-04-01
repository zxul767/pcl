(in-package :dev.zxul767.binary)

(defconstant +null+ (code-char 0))
(defvar *in-progress-objects* nil)

;; -----------------------------------------------------------------------------
;; Interfaces
;; -----------------------------------------------------------------------------
(defgeneric read-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Fill in the slots of object from stream."))


(defgeneric write-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Write out the slots of object from stream."))

(defgeneric read-value (type stream &key)
  (:documentation "Read a value of the given type from the stream."))

(defgeneric write-value (type stream value &key)
  (:documentation "Write a value as the given type to the stream."))

;; -----------------------------------------------------------------------------
;; Current Object Stack Support
;; -----------------------------------------------------------------------------
(defmethod read-object :around (object stream)
  (declare (ignore stream))
  (let ((*in-progress-objects* (cons object *in-progress-objects*)))
    (call-next-method)))

(defmethod write-object :around (object stream)
  (declare (ignore stream))
  (let ((*in-progress-objects* (cons object *in-progress-objects*)))
    (call-next-method)))

(defun current-binary-object () (first *in-progress-objects*))

(defun parent-of-type (type)
  (find-if #'(lambda (x) (typep x type)) *in-progress-objects*))

;; -----------------------------------------------------------------------------
;; Default Implementations
;; -----------------------------------------------------------------------------
(defmethod read-value ((type symbol) stream &key)
  (let ((object (make-instance type)))
    (read-object object stream)
    object))

(defmethod write-value ((type symbol) stream value &key)
  (assert (typep value type))
  (write-object value stream))
;; -----------------------------------------------------------------------------
;; Macro Helper Functions
;; -----------------------------------------------------------------------------
(defun slot->defclass-slot (spec)
  (let ((name (first spec)))
    `(,name :initarg ,(as-keyword name) :accessor ,name)))

(defun as-keyword (symbol) (intern (string symbol) :keyword))

(defun slot->read-value-expression (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(setf ,name (read-value ',type ,stream ,@args))))

(defun slot->write-value-expression (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(write-value ',type ,stream ,name ,@args)))

(defun normalize-slot-spec (spec)
  (list (first spec) (ensure-list (second spec))))

(defun ensure-list (x)
  (if (listp x) x (list x)))

(defun direct-slots (class-name)
  (copy-list (get class-name 'slots)))

(defun inherited-slots (class-name)
  (loop for super in (get class-name 'superclasses)
        nconc (direct-slots super)
        nconc (inherited-slots super)))

(defun all-slots (class-name)
  (nconc (direct-slots class-name) (inherited-slots class-name)))

(defun new-class-all-slots (slots superclasses)
  (nconc (mapcan #'all-slots superclasses) (mapcar #'first slots)))

(defun slot->binding (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(,name (read-value ',type ,stream ,@args))))

(defun slot->keyword-arg (spec)
  (let ((name (first spec)))
    `(,(as-keyword name) ,name)))

;; -----------------------------------------------------------------------------
;; General Utility Macros
;; -----------------------------------------------------------------------------
(defmacro let-guard ((var condition) error &body body)
  (assert (symbolp var))
  `(let ((,var ,condition))
     (if ,var (progn ,@body) ,error)))

;; -----------------------------------------------------------------------------
;; Binary Data Structure Definition Macros
;; -----------------------------------------------------------------------------

;; NOTE: all macros intended to be called by users have `slots' as the last argument
;; this macro is not really meant to be used by users, so it is allowed to break
;; that convention
(defmacro define-generic-binary-class (name (&rest superclasses) slots &body read-method)
  (with-gensyms ((objectvar "object") (streamvar "stream"))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (get ',name 'slots) ',(mapcar #'first slots))
         (setf (get ',name 'superclasses) ',superclasses))

       (defclass ,name ,superclasses
         ,(mapcar #'slot->defclass-slot slots))

       ,@read-method

       (defmethod write-object progn ((,objectvar ,name) ,streamvar)
         (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
           ,@(mapcar #'(lambda (s) (slot->write-value-expression s streamvar)) slots))))))


(defmacro define-binary-class (name (&rest superclasses) &body slots)
  (with-gensyms ((objectvar "object") (streamvar "stream"))
    `(define-generic-binary-class ,name ,superclasses ,slots
       (defmethod read-object progn ((,objectvar ,name) ,streamvar)
         (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
           ,@(mapcar #'(lambda (s) (slot->read-value-expression s streamvar)) slots))))))
;; Usage:
;; (define-binary-class frame ()
;;   (id (iso-8859-1-string :length 3))
;;   (size u3))
;;
;; (define-binary-class generic-frame (frame)
;;   (data (raw-bytes :bytes size)))

(defmacro define-tagged-binary-class (name (&rest superclasses) (&rest options) &body slots)
  (let-guard (class-resolver (second (assoc :class-resolver options)))
             (error ":class-resolver option is mandatory!")
    (with-gensyms ((typevar "type") (objectvar "object") (streamvar "stream"))
      `(define-generic-binary-class ,name ,superclasses ,slots
         (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key)
           (let* ,(mapcar #'(lambda (s) (slot->binding s streamvar)) slots)
             (let ((,objectvar (make-instance ,class-resolver ,@(mapcan #'slot->keyword-arg slots))))
               (read-object ,objectvar ,streamvar)
               ,objectvar)))))))
;; Usage:
;; (define-tagged-binary-class id3-frame ()
;;     ((:class-resolver (find-frame-class id)))
;;   (id (iso-8859-1-string :length 3))
;;   (size u3))

(defmacro define-binary-type (name (&rest args) &body spec)
  (ecase (length spec)
    (1
     (with-gensyms (type stream value)
       (destructuring-bind (derived-from &rest derived-args) (ensure-list (first spec))
         `(progn
            (defmethod read-value ((,type (eql ',name)) ,stream &key ,@args)
              (read-value ',derived-from ,stream ,@derived-args))
            (defmethod write-value ((,type (eql ',name)) ,stream ,value &key ,@args)
              (write-value ',derived-from ,stream ,value ,@derived-args))))))
    (2
     (with-gensyms (type)
       `(progn
          ,(destructuring-bind ((in) &body body) (rest (assoc :reader spec))
             `(defmethod read-value ((,type (eql ',name)) ,in &key ,@args)
                ,@body))
          ,(destructuring-bind ((out value) &body body) (rest (assoc :writer spec))
             `(defmethod write-value ((,type (eql ',name)) ,out ,value &key ,@args)
                ,@body)))))))
;; Usage:
;; (define-binary-type iso-8859-1-string (length)
;;   (:reader (in)
;;            (let ((string (make-string length)))
;;              (dotimes (i length)
;;                (setf (char string i) (code-char (read-byte in))))
;;              string))
;;   (:writer (out string)
;;            (dotimes (i length)
;;              (write-byte (char-code (char string i)) out))))
;;
;; (define-binary-type u1 () (unsigned-integer :bytes 1))
