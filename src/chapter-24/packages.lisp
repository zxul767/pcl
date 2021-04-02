(in-package :cl-user)

(defpackage :dev.zxul767.binary
  (:use :common-lisp :dev.zxul767.macrotools)
  (:export
   :define-binary-class
   :define-tagged-binary-class
   :define-binary-type
   :read-value
   :write-value
   :*in-progress-objects*
   :parent-of-type
   :current-binary-object
   :+null+))
