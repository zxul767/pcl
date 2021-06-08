(in-package :cl-user)

(defpackage :dev.zxul767.macrotools
  (:use :cl :trivial-indent
        :dev.zxul767.prelude)
  (:export
   :condlet
   :once-only
   :let-guard
   :prog-nil
   :with-labels
   :when-bind
   :when-bind*
   :with-gensyms
   :with-result
   :with-safe-io-syntax))
