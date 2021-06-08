(in-package :cl-user)

(defpackage :dev.zxul767.macrotools
  (:use :cl :trivial-indent)
  (:export
   :as-keyword
   :once-only
   :condlet
   :with-labels
   :when-bind
   :when-bind*
   :with-gensyms
   :with-result
   :with-safe-io-syntax
   :prog-nil))
