(in-package :cl-user)

(defpackage :dev.zxul767.macrotools
  (:use :cl :trivial-indent)
  (:export
   :with-gensyms
   :once-only
   :condlet
   :with-labels
   :when-bind
   :when-bind*
   :with-result
   :prog-nil))
