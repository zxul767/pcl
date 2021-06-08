(in-package :cl-user)

(defpackage :dev.zxul767.binary
  (:use
   :common-lisp :trivial-indent
   :dev.zxul767.prelude
   :dev.zxul767.macrotools)
  (:export
   :define-binary-class
   :define-tagged-binary-class
   :define-binary-type
   :read-value
   :write-value
   :*binary-objects-processing-stack*
   :first-ancestor-in-processing-stack-by-type
   :first-object-in-processing-stack
   :+null+))
