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

(defun current-binary-object ()
  (first *in-progress-objects*))

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
;; General Helper Functions
;; -----------------------------------------------------------------------------
(defun as-keyword (symbol) (intern (string symbol) :keyword))

(defun normalize-slot-spec (spec)
  (list (first spec) (ensure-list (second spec))))

(defun ensure-list (x)
  (if (listp x) x (list x)))

;; -----------------------------------------------------------------------------
;; General Utility Macros
;; -----------------------------------------------------------------------------
(defmacro let-guard ((var condition) error &body body)
  (assert (symbolp var))
  `(let ((,var ,condition))
     (if ,var (progn ,@body) ,error)))

;; -----------------------------------------------------------------------------
;; Binary Data Structures Definition Macros
;; -----------------------------------------------------------------------------
;; NOTE: all macros intended to be called by users have `slots' as the last
;; argument, but this macro is not really meant to be used by users, so it is
;; allowed to break that convention
(defmacro define-generic-binary-class (name (&rest superclasses) slots &body read-method)
  (with-gensyms (object stream)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (get ',name 'slots) ',(mapcar #'first slots))
         (setf (get ',name 'superclasses) ',superclasses))

       (defclass ,name ,superclasses
         ,(generate-defclass-slots slots))

       ,@read-method

       (defmethod write-object progn ((,object ,name) ,stream)
         (with-slots ,(new-class-all-slots slots superclasses) ,object
           ,@(generate-write-slots-statements slots stream))))))

(defun generate-defclass-slots (slots)
  (flet ((slot->defclass-slot (spec)
           (let ((name (first spec)))
             `(,name :initarg ,(as-keyword name) :accessor ,name))))
    (mapcar #'slot->defclass-slot slots)))

(defun generate-write-slots-statements (slots stream)
  (flet ((slot->write-value-statement (spec stream)
           (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
             `(write-value ',type ,stream ,name ,@args))))
    (mapcar #'(lambda (s) (slot->write-value-statement s stream)) slots)))

(defun direct-slots (class-symbol)
  (copy-list (get class-symbol 'slots)))

(defun inherited-slots (class-symbol)
  (loop for super in (get class-symbol 'superclasses)
        nconc (direct-slots super)
        nconc (inherited-slots super)))

(defun all-slots (class-symbol)
  (nconc (direct-slots class-symbol) (inherited-slots class-symbol)))

;; this function is necessary because calling (all-slots <class-X>) when we're
;; compiling <class-X> will result in an empty list since the symbol <class-X>
;; has not yet been processed and thus has no slots or superclasses information
;; (it will after the expansion for <class-X> is compiled/loaded at top-level)
(defun new-class-all-slots (slots superclasses)
  (nconc (mapcan #'all-slots superclasses) (mapcar #'first slots)))

;; -----------------------------------------------------------------------------
;; Usage:
;; (define-binary-class frame ()
;;   (id (iso-8859-1-string :length 3))
;;   (size u3))
;;
;; (define-binary-class generic-frame (frame)
;;   (data (raw-bytes :bytes size)))
(defmacro define-binary-class (name (&rest superclasses) &body slots)
  (with-gensyms (object stream)
    `(define-generic-binary-class ,name ,superclasses ,slots
       (defmethod read-object progn ((,object ,name) ,stream)
         (with-slots ,(new-class-all-slots slots superclasses) ,object
           ,@(generate-read-slots-statements slots stream))))))

(defun generate-read-slots-statements (slots stream)
  (flet ((slot->read-value-expression (spec stream)
           (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
             `(setf ,name (read-value ',type ,stream ,@args)))))
    (mapcar #'(lambda (s) (slot->read-value-expression s stream)) slots)))

;; -----------------------------------------------------------------------------
;; Usage:
;; (define-tagged-binary-class id3-frame ()
;;     ((:class-name-finder (find-frame-class id)))
;;   (id (iso-8859-1-string :length 3))
;;   (size u3))
;;
;; A tagged binary class is a structure that has generic data in the first few
;; fields (with a mandatory ID field) but whose remaining structure depends on
;; the value of the ID found, so a mechanism to build the correct binary class
;; and dispatch the reading of its corresponding fields is required, hence the
;; :class-name-finder argument.
(defmacro define-tagged-binary-class (name (&rest superclasses) (&rest options) &body slots)
  (let-guard (class-name-finder (second (assoc :class-name-finder options)))
             (error ":class-name-finder option is mandatory!")
    (with-gensyms (type object stream)
      `(define-generic-binary-class ,name ,superclasses ,slots
         (defmethod read-value ((,type (eql ',name)) ,stream &key)
           (let* ,(generate-slot-read-bindings slots stream)
             (let ((,object (make-instance ,class-name-finder ,@(generate-slot-keywords slots))))
               (read-object ,object ,stream)
               ,object)))))))

(defun generate-slot-read-bindings (slots stream)
  (flet ((slot->binding (spec stream)
           (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
             `(,name (read-value ',type ,stream ,@args)))))
    (mapcar #'(lambda (s) (slot->binding s stream)) slots)))

(defun generate-slot-keywords (slots)
  (flet ((slot->keyword-arg (spec)
           (let ((name (first spec)))
             `(,(as-keyword name) ,name))))
    (mapcan #'slot->keyword-arg slots)))

;; -----------------------------------------------------------------------------
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
