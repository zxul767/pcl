(in-package #:dev.zxul767.binary)

;; Two-byte encodings need to know if the first logical byte is written out first.
(defvar *bom-big-endian* #xfeff)
(defvar *bom-little-endian* #xfffe)

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
      (code-char code)))
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
      (code-char code)))
  (:writer (out char)
    (let ((code (char-code char)))
      (unless (<= 0 code #xffff)
        (error "Illegal character for ucs-2 encoding: ~c with char-code: ~d"
               char code))
      (if swap (setf code (swap-bytes code)))
      (write-value 'u2 out code))))

(define-binary-type ucs-2-char-big-endian ()
  (ucs-2-char :swap nil))

(define-binary-type ucs-2-char-little-endian ()
  (ucs-2-char :swap t))

(define-binary-type generic-string (length char-type)
  (:reader (in)
    (let-return (string (make-string length))
      (dotimes (i length)
        (setf (char string i) (read-value char-type in)))))
  (:writer (out string)
    (dotimes (i length)
      (write-value char-type out (char string i)))))

(define-binary-type generic-terminated-string (terminator char-type)
  (:reader (in)
    (with-output-to-string (out)
      (loop for char = (read-value char-type in)
            until (char= char terminator)
            do (write-char char out))))
  (:writer (out string)
    (loop for char across string
          do (write-value char-type out char)
          finally (write-value char-type out terminator))))

(define-binary-type iso-8859-1-string (length)
  (generic-string :length length :char-type 'iso-8859-1-char))

(define-binary-type iso-8859-1-terminated-string (terminator)
  (generic-terminated-string :terminator terminator :char-type 'iso-8859-1-char))

(define-binary-type ucs-2-string (length)
  (:reader (in)
    (let ((byte-order-mark (read-value 'u2 in))
          (characters (1- (/ length 2))))
      (read-value 'generic-string in
                  :length characters
                  :char-type (ucs-2-char-type byte-order-mark))))
  (:writer (out string)
    (declare (ignorable length))
    (assert (= length (length string)))

    (write-value 'u2 out *bom-big-endian*)
    (write-value 'generic-string out string
                 :length (length string)
                 :char-type (ucs-2-char-type *bom-big-endian*))))

(define-binary-type ucs-2-terminated-string (terminator)
  (:reader (in)
    (let ((byte-order-mark (read-value 'u2 in)))
      (read-value 'generic-terminated-string in
                  :terminator terminator
                  :char-type (ucs-2-char-type byte-order-mark))))
  (:writer (out string)
    (write-value 'u2 out *bom-big-endian*)
    (write-value 'generic-terminated-string out string
                 :terminator terminator
                 :char-type (ucs-2-char-type *bom-big-endian*))))

(define-binary-type raw-bytes (size max-size)
  (:reader (in)
    (if (> size max-size)
        (error 'data-too-large :size size :max-size max-size))
    (let-return (buffer (make-array size :element-type '(unsigned-byte 8)))
      (read-sequence buffer in)))
  (:writer (out buffer)
    (assert (= size (length buffer)))
    (assert (< size max-size))
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
