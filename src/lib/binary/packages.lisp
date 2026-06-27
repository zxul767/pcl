(in-package #:cl-user)

(defpackage #:dev.zxul767.binary
  (:use #:cl
        #:trivial-indent
        #:dev.zxul767.prelude)
  (:export #:*binary-objects-processing-stack*
           #:+null+
           #:first-ancestor-in-processing-stack-by-type
           #:first-object-in-processing-stack
           #:define-binary-class
           #:define-tagged-binary-class
           #:define-binary-type
           #:read-value
           #:write-value
           ;; integers
           #:unsigned-integer
           #:u1
           #:u2
           #:u3
           #:u4
           ;; characters
           #:iso-8859-1-char
           #:ucs-2-char
           #:ucs-2-char-big-endian
           #:ucs-2-char-little-endian
           ;; strings
           #:iso-8859-1-string
           #:iso-8859-1-terminated-string
           #:ucs-2-string
           #:ucs-2-terminated-string
           ;; bytes
           #:raw-bytes))
