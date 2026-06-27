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
           #:write-value))

