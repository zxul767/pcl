(in-package :cl-user)

(defpackage :dev.zxul767.macrotools
  (:use :cl :trivial-indent)
  (:export
   :as-keyword
   :with-gensyms
   :fn-and
   :once-only
   :condlet
   :with-labels
   :when-bind
   :when-bind*
   :with-result
   :prog-nil))
