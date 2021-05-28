(in-package :dev.zxul767.binary)

;; Tell SLIME to indent certain macros we'll define later in this file
;; in special ways (basically so they indent as if they were defun bodies)
;;
;; For instance, the following definition would normally indent the `let'
;; body to align with the `(in)' parameter, but in reality we want it to
;; be indented as a body:
;;
;; (define-binary-type iso-8859-1-string (length)
;;   (:reader (in)
;;     (let ((string (make-string length)))
;;       (dotimes (i length)
;;         (setf (char string i) (code-char (read-byte in))))
;;       string))
;;   (:writer (out string)
;;     (dotimes (i length)
;;       (write-byte (char-code (char string i)) out))))
(indent:define-indentation
    define-binary-type (&lambda &lambda &rest (&whole 2 4 &rest 2)))
(indent:initialize-slime)

(defconstant +null+ (code-char 0))

;; -----------------------------------------------------------------------------
;; Interfaces
;; -----------------------------------------------------------------------------
(defgeneric read-value (type stream &key)
  (:documentation "Read a value of the given type from the stream."))

(defgeneric write-value (type stream value &key)
  (:documentation "Write a value as the given type to the stream."))

(defgeneric read-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Fill in the slots of object from stream."))

(defgeneric write-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Write out the slots of object from stream."))

;; -----------------------------------------------------------------------------
;; Binary Objects Processing Stack Support
;;
;; When processing binary objects, sometimes the parsing of a nested object
;; depends on information found in a parent (or more generally an ancestor)
;; object. The solution chosen here involves the creation of a stack of objects
;; currently being processed (the "processing stack") implemented with a dynamic
;; variable (*binary-objects-processing-stack*)
;; -----------------------------------------------------------------------------
(defvar *binary-objects-processing-stack* nil)

(defmethod read-object :around (object stream)
  (declare (ignore stream))
  (let ((*binary-objects-processing-stack*
          (cons object *binary-objects-processing-stack*)))
    (call-next-method)))

(defmethod write-object :around (object stream)
  (declare (ignore stream))
  (let ((*binary-objects-processing-stack*
          (cons object *binary-objects-processing-stack*)))
    (call-next-method)))

(defun first-object-in-processing-stack ()
  (first *binary-objects-processing-stack*))

(defun first-ancestor-in-processing-stack-by-type (type)
  "Return the first object of type `type' (excluding the first one in the processing
stack) currently being read/written."
  (find-if #'(lambda (x) (typep x type)) (rest *binary-objects-processing-stack*)))

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
;; General Helper Functions & Macros
;; -----------------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun assert-all (predicate sequence)
    (dolist (item sequence)
      (assert (funcall predicate item)))))

;; (id (iso-8859-1-string :length 3)) => (id (iso-8859-1-string :length 3))
;; (size u3))                         => (size (u3))
(defun normalize-slot (slot)
  (list (first slot) (ensure-list (second slot))))

(defmacro with-slot-parts ((name type args) slot &body body)
  (assert-all #'symbolp (list name type args))
  `(destructuring-bind (,name (,type &rest ,args)) (normalize-slot ,slot)
     ,@body))

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
         (with-slots ,(all-combined-slots slots superclasses) ,object
           ,@(generate-write-slot-expressions slots stream))))))

(defun generate-defclass-slots (slots)
  (loop for (name _) in slots
        collect `(,name :initarg ,(as-keyword name) :accessor ,name)))

(defun generate-write-slot-expressions (slots stream)
  (flet ((slot->write-value-statement (slot)
           (with-slot-parts (name type args) slot
             `(write-value ',type ,stream ,name ,@args))))
    (mapcar #'slot->write-value-statement slots)))

(defun direct-slots (class-symbol)
  (copy-list (get class-symbol 'slots)))

(defun inherited-slots (class-symbol)
  (loop for super in (get class-symbol 'superclasses)
        nconc (direct-slots super)
        nconc (inherited-slots super)))

(defun all-slots (class-symbol)
  (nconc (direct-slots class-symbol) (inherited-slots class-symbol)))

;; This function is necessary because when we're macro-expanding calls to
;; `define-binary-class', any call to (all-slots <class-symbol>) would result
;; in an empty list, since the code to attach `slots' and `superclasses' to
;; <class-symbol> is yet to be run after the macro expansion.
(defun all-combined-slots (slots superclasses)
  (nconc (mapcan #'all-slots superclasses) (mapcar #'first slots)))

;; -----------------------------------------------------------------------------
;; Usage:
;; (define-binary-class frame ()
;;   (id (iso-8859-1-string :length 3))
;;   (size u3))
;;
;; (define-binary-class generic-frame (frame)
;;   (data (raw-bytes :bytes size)))
;; -----------------------------------------------------------------------------
(defmacro define-binary-class (name (&rest superclasses) &body slots)
  (with-gensyms (object stream)
    `(define-generic-binary-class ,name ,superclasses ,slots
       (defmethod read-object progn ((,object ,name) ,stream)
         (with-slots ,(all-combined-slots slots superclasses) ,object
           ,@(generate-read-slot-expressions slots stream))))))

(defun generate-read-slot-expressions (slots stream)
  (flet ((slot->read-value-expression (slot)
           (with-slot-parts (name type args) slot
             `(setf ,name (read-value ',type ,stream ,@args)))))
    (mapcar #'slot->read-value-expression slots)))

;; -----------------------------------------------------------------------------
;; Usage:
;; (define-tagged-binary-class id3-frame ()
;;     ((:class-finder (find-frame-class id)))
;;   (id (iso-8859-1-string :length 3))
;;   (size u3))
;;
;; A tagged binary class is a structure that has generic data in the first few
;; fields (with a mandatory ID field) but whose remaining structure depends on
;; the value of the ID found, so a mechanism to build the correct binary class
;; and dispatch the reading of its corresponding fields is required, hence the
;; :class-finder argument.
;; -----------------------------------------------------------------------------
(defmacro define-tagged-binary-class (name (&rest superclasses) (&rest options) &body slots)
  (let-guard (class-finder (second (assoc :class-finder options)))
             (error ":class-finder option is mandatory!")
    (with-gensyms (type object stream)
      `(define-generic-binary-class ,name ,superclasses ,slots
         (defmethod read-value ((,type (eql ',name)) ,stream &key)
           (let* ,(generate-read-slot-bindings slots stream)
             (let ((,object (make-instance ,class-finder ,@(generate-slot-keywords slots))))
               (read-object ,object ,stream)
               ,object)))))))

(defun generate-read-slot-bindings (slots stream)
  (flet ((slot->binding (slot)
           (with-slot-parts (name type args) slot
             `(,name (read-value ',type ,stream ,@args)))))
    (mapcar #'slot->binding slots)))

(defun generate-slot-keywords (slots)
  (loop for (name _) in slots append `(,(as-keyword name) ,name)))

;; -----------------------------------------------------------------------------
;; Usage:
;; (define-binary-type u1 () (unsigned-integer :bytes 1))
;;
;; (define-binary-type iso-8859-1-string (length)
;;   (:reader (in)
;;     (let ((string (make-string length)))
;;       (dotimes (i length)
;;         (setf (char string i) (code-char (read-byte in))))
;;       string))
;;   (:writer (out string)
;;     (dotimes (i length)
;;       (write-byte (char-code (char string i)) out))))
;; -----------------------------------------------------------------------------
(defmacro define-binary-type (name (&rest args) &body spec)
  (ecase (length spec)
    ;; derived from an existing type
    (1
     (with-gensyms (type stream value)
       (destructuring-bind (derived-from &rest derived-args) (ensure-list (first spec))
         `(progn
            (defmethod read-value ((,type (eql ',name)) ,stream &key ,@args)
              (read-value ',derived-from ,stream ,@derived-args))
            (defmethod write-value ((,type (eql ',name)) ,stream ,value &key ,@args)
              (write-value ',derived-from ,stream ,value ,@derived-args))))))
    ;; specified with :reader and :writer methods
    (2
     (with-gensyms (type)
       `(progn
          ,(destructuring-bind ((in) &body body) (rest (assoc :reader spec))
             `(defmethod read-value ((,type (eql ',name)) ,in &key ,@args)
                ,@body))
          ,(destructuring-bind ((out value) &body body) (rest (assoc :writer spec))
             `(defmethod write-value ((,type (eql ',name)) ,out ,value &key ,@args)
                ,@body)))))))
