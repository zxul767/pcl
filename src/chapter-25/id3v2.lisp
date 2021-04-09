(in-package :dev.zxul767.id3v2)

;; two-byte encodings require knowing whether the first logical byte is written out first
(defvar *bom-big-endian* #xfeff)
(defvar *bom-little-endian* #xfffe)

;; -----------------------------------------------------------------------------
;; General Utilities & Macros
;; -----------------------------------------------------------------------------
(defmacro fn-and (&rest functions)
  `#'(lambda (&rest args)
       (and ,@(loop for fn in functions collect `(apply #',fn args)))))

(defmacro let-when ((var condition) &body then)
  `(let ((,var ,condition))
     (when ,var ,@then)))

(defmacro with-result-in ((result &optional initform) &body body)
  `(let ((,result ,initform))
     ,@body
     ,result))

;; -----------------------------------------------------------------------------
;; Primitive Types
;; -----------------------------------------------------------------------------
(define-binary-type unsigned-integer (bytes bits-per-byte)
  (:reader (in)
           (loop with value = 0
                 for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte
                 do (setf (ldb (byte bits-per-byte low-bit) value) (read-byte in))
                 finally (return value)))
  (:writer (out value)
           (loop for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte
                 do (write-byte (ldb (byte bits-per-byte low-bit) value) out))))

(define-binary-type u1 () (unsigned-integer :bytes 1 :bits-per-byte 8))
(define-binary-type u2 () (unsigned-integer :bytes 2 :bits-per-byte 8))
(define-binary-type u3 () (unsigned-integer :bytes 3 :bits-per-byte 8))
(define-binary-type u4 () (unsigned-integer :bytes 4 :bits-per-byte 8))

(define-binary-type iso-8859-1-char ()
  (:reader (in)
           (let ((code (read-byte in)))
             (code-char code)
             (or (code-char code)
                 (error "Character code ~d not supported" code))))
  (:writer (out char)
           (let ((code (char-code char)))
             (if (<= 0 code #xff)
                 (write-byte code out)
                 (error "Illegal character for iso-8859-1 encoding. Character: ~c with code: ~d"
                        char code)))))

(define-binary-type ucs-2-char (swap)
  (:reader (in)
           (let ((code (read-value 'u2 in)))
             (if swap (setf code (swap-bytes code)))
             (or (code-char code) (error "Character code ~d not supported" code))))
  (:writer (out char)
           (let ((code (char-code char)))
             (unless (<= 0 code #xffff)
               (error "Illegal character for ucs-2 encoding: ~c with char-code: ~d"
                      char code))
             (if swap (setf code (swap-bytes code)))
             (write-value 'u2 out code))))

(define-binary-type ucs-2-char-big-endian ()
  (ucs-2-char :swap nil))

(define-binary-type ucs-2-char-big-endian ()
  (ucs-2-char :swap t))

(define-binary-type generic-string (length character-type)
  (:reader (in)
           (with-result-in (string (make-string length))
             (dotimes (i length)
               (setf (char string i) (read-value character-type in)))))
  (:writer (out string)
           (dotimes (i length)
             (write-value character-type out (char string i)))))

(define-binary-type generic-terminated-string (terminator character-type)
  (:reader (in)
           (with-output-to-string (s)
             (loop for char = (read-value character-type in)
                   until (char= char terminator)
                   do (write-char char s))))
  (:writer (out string)
           (loop for char across string
                 do (write-value character-type out char)
                 finally (write-value character-type out terminator))))

(define-binary-type iso-8859-1-string (length)
  (generic-string :length length :character-type 'iso-8859-1-char))

(define-binary-type iso-8859-1-terminated-string (terminator)
  (generic-terminated-string :terminator terminator :character-type 'iso-8859-1-char))

(define-binary-type ucs-2-string (length)
  (:reader (in)
           (let ((byte-order-mark (read-value 'u2 in))
                 (characters (1- (/ length 2))))
             (read-value 'generic-string in
                          :length characters
                          :character-type (ucs-2-char-type byte-order-mark))))
  (:writer (out string)
           (declare (ignorable length))
           (assert (= length (length string)))

           (write-value 'u2 out *bom-big-endian*)
           (write-value 'generic-string out string
                         :length (length string)
                         :character-type (ucs-2-char-type *bom-big-endian*))))

(define-binary-type ucs-2-terminated-string (terminator)
  (:reader (in)
           (let ((byte-order-mark (read-value 'u2 in)))
             (read-value 'generic-terminated-string in
                          :terminator terminator
                          :character-type (ucs-2-char-type byte-order-mark))))
  (:writer (out string)
           (write-value 'u2 out *bom-big-endian*)
           (write-value 'generic-terminated-string out string
                         :terminator terminator
                         :character-type (ucs-2-char-type *bom-big-endian*))))

(define-binary-type raw-bytes (size)
  (:reader (in)
           (with-result-in (buffer (make-array size :element-type '(unsigned-byte 8)))
             (read-sequence buffer in)))
  (:writer (out buffer)
           (assert (eql size (length buffer)))
           (write-sequence buffer out)))

(defun ucs-2-char-type (byte-order-mark)
  (ecase byte-order-mark
    (*bom-big-endian* 'ucs-2-char-big-endian)
    (*bom-little-endian* 'ucs-2-char-little-endian)))

(defun swap-bytes (code)
  (assert (<= code #xffff))
  (rotatef (ldb (byte 8 0) code)
           (ldb (byte 8 8) code))
  code)

;; -----------------------------------------------------------------------------
;; ID3 Generic
;; -----------------------------------------------------------------------------

;; At the end of the frames, there is likely to be some padding. We use a signal
;; and the condition system to deal with this.
(define-condition in-padding () ())

(defgeneric frame-header-size (frame))

(define-binary-type id3-tag-size ()
  (unsigned-integer :bytes 4 :bits-per-byte 7))

(define-binary-type optional (type if)
  (:reader (in)
           (when if (read-value type in)))
  (:writer (out value)
           (when if (write-value type out value))))

(define-tagged-binary-class id3-tag ()
    ((:class-finder (ecase major-version
                      (2 'id3v2.2-tag)
                      (3 'id3v2.3-tag))))
  (identifier (iso-8859-1-string :length 3))
  (major-version u1)
  (revision u1)
  (flags u1)
  (size id3-tag-size))

;; frame IDs are just like other regular IDs in the ID3 spec, but they are
;; designed to signal the `in-padding' "exception" to transfer control up
;; to the `read-frame' function when padding is found.
(define-binary-type frame-id (length)
  (:reader (in)
           (let ((first-byte (read-byte in)))
             (if (= first-byte 0) (signal 'in-padding))
             (let ((rest (read-value 'iso-8859-1-string in :length (1- length))))
               (concatenate 'string (string (code-char first-byte)) rest))))
  (:writer (out id)
           (write-value 'iso-8859-1-string id :length length)))

(define-tagged-binary-class id3-frame ()
    ((:class-finder (find-frame-class id)))
  (id (frame-id :length 3))
  (size u3))

;; (define-binary-class generic-id3-frame (id3-frame)
;;   (data (raw-bytes :size size)))

;; The decision to make this a base class (as opposed to subclassing
;; `id3-frame') is done to avoid the concrete frame classes in each
;; version (2.2 and 2.3) duplicate the `data' field. However, the tradeoff
;; is that we lose access to the `size' field, so a workaround is needed
;; to make it work; using `first-object-in-processing-stack' is that workaround.
;; However, it's harder to reason about this because we now have to think about
;; how things look like at runtime, and convince ourselves that the call to
;; `data-bytes' will return the correct value (based on the return value from
;; `first-object-in-processing-stack')
;;
(define-binary-class generic-id3-frame ()
  (data (raw-bytes :size (data-bytes (first-object-in-processing-stack)))))

(defgeneric data-bytes (frame))

(define-binary-type id3-frames (tag-size frame-type)
  (:reader (in)
           (loop with to-read = tag-size
                 while (plusp to-read)
                 for frame = (read-frame frame-type in)
                 while frame
                 do (decf to-read (+ (frame-header-size frame) (size frame)))
                 collect frame
                 ;; skip over null padding
                 finally (loop repeat (1- to-read) do (read-byte in))))
  (:writer (out frames)
           (loop with to-write = tag-size
                 for frame in frames
                 do (write-value frame-type out frame)
                    (decf to-write (+ (frame-header-size frame) (size frame)))
                    ;; write null padding if necessary
                 finally (loop repeat to-write do (write-byte 0 out)))))

(defun read-frame (frame-type in)
  (handler-case (read-value frame-type in)
    (in-padding () nil)))

(defun read-id3 (filepath)
  "Read the ID3 tag of the mp3 at `filepath'"
  (with-open-file (in filepath :element-type '(unsigned-byte 8))
    ;; FIXME: we should replace this with a specific condition to signal that we
    ;; couldn't read a specific version (e.g., v2.4) to avoid conflating other
    ;; legitimate errors
    (handler-case (read-value 'id3-tag in)
      (error () (progn (format t "failed to process ~a~%" (enough-namestring filepath)) nil)))))

(defun show-tag-headers (directory)
  "Show the ID3 tag header for all mp3 files under `directory'"
  (flet
      ((show-tag-header (file)
         (with-slots (identifier major-version revision flags size) (read-id3 file)
           (format t "~a ~d.~d ~8,'0b ~d bytes -- ~a~%"
                   identifier major-version revision flags size (enough-namestring file)))))
    (walk-directory directory #'show-tag-header
                    :recursively t
                    :file-condition (fn-and mp3-p id3-p))))

(defun count-versions (directory)
  "Count the total number of mp3 files grouped by the version of their ID3 tags"
  (let ((version-counts (mapcar #'(lambda (version) (cons version 0)) '(2 3 4))))
    (flet ((count-version (file)
             (let-when (major-version (assoc (major-version (read-id3 file)) version-counts))
               (incf (cdr major-version)))))
      (walk-directory directory #'count-version
                      :recursively t
                      :file-condition #'mp3-p))
    (loop for (version . count) in version-counts
          collect `(:version ,version :count ,count))))

(defun mp3-p (filepath)
  "Does `filepath' point to what seems to be an mp3 file?"
  (and (file-pathname-p filepath)
       (string-equal "mp3" (pathname-type filepath))))

(defun id3-p (filepath)
  "Is `filepath' an mp3 file with a valid ID3 tag?"
  (with-open-file (in filepath :element-type '(unsigned-byte 8))
    (string= "ID3" (read-value 'iso-8859-1-string in :length 3))))

(defun find-frame-class (id)
  (ecase (length id)
    (3 'generic-id3-frame-v2.2)
    (4 'generic-id3-frame-v2.3)))

(defun frame-id (plist)
  (getf plist :id))

(defun frame-types (filepath)
  (let-when (id3 (read-id3 filepath))
    (delete-duplicates
     (mapcar #'(lambda (frame) (list :id (id frame) :version (major-version id3)))
             (frames id3))
     :key #'frame-id
     :test #'string=)))

(defun frame-types-in-directory (directory)
  (with-result-in (ids)
    (flet
        ((collect (filepath)
           (setf ids (nunion ids (frame-types filepath) :key #'frame-id :test #'string=))))
      (walk-directory directory #'collect
                      :recursively t
                      :file-condition (fn-and mp3-p id3-p)))))

;; -----------------------------------------------------------------------------
;; ID3 V2.2
;; -----------------------------------------------------------------------------
(define-binary-class id3v2.2-tag (id3-tag)
  (frames (id3-frames :tag-size size :frame-type 'id3v2.2-frame)))

(define-tagged-binary-class id3v2.2-frame ()
    ((:class-finder (find-frame-class id)))
  (id (frame-id :length 3))
  (size u3))

(define-binary-class generic-id3-frame-v2.2 (id3v2.2-frame generic-id3-frame))

(defmethod frame-header-size ((frame id3v2.2-frame)) 6)

(defmethod data-bytes ((frame id3v2.2-frame))
  (size frame))

;; -----------------------------------------------------------------------------
;; ID3 V2.3
;; -----------------------------------------------------------------------------
(define-binary-class id3v2.3-tag (id3-tag)
  (extended-header-size (optional :type 'u4 :if (extended-p flags)))
  (extra-flags (optional :type 'u2 :if (extended-p flags)))
  (padding-size (optional :type 'u4 :if (extended-p flags)))
  (crc (optional :type 'u4 :if (crc-p flags extra-flags)))
  (frames (id3-frames :tag-size size :frame-type 'id3v2.3-frame)))

(define-tagged-binary-class id3v2.3-frame ()
    ((:class-finder (find-frame-class id)))
  (id (frame-id :length 4))
  (size u4)
  (flags u2)
  (decompressed-size (optional :type 'u4 :if (frame-compressed-p flags)))
  (encryption-scheme (optional :type 'u1 :if (frame-encrypted-p flags)))
  (grouping-identity (optional :type 'u1 :if (frame-grouped-p flags))))

(define-binary-class generic-id3-frame-v2.3 (id3v2.3-frame generic-id3-frame))

(defmethod frame-header-size ((frame id3v2.3-frame)) 10)

(defmethod data-bytes ((frame id3v2.3-frame))
  (let ((flags (flags frame)))
    (- (size frame)
       (if (frame-compressed-p flags) 4 0)
       (if (frame-encrypted-p flags) 1 0)
       (if (frame-grouped-p flags) 1 0))))

(defun extended-p (flags)
  (logbitp 6 flags))

(defun crc-p (flags extra-flags)
  "Do the specification flags indicate there is cyclic redundancy check data?"
  (and (extended-p flags) (logbitp 15 extra-flags)))

(defun frame-compressed-p (flags)
  (logbitp 7 flags))

(defun frame-encrypted-p (flags)
  (logbitp 6 flags))

(defun frame-grouped-p (flags)
  (logbitp 5 flags))
