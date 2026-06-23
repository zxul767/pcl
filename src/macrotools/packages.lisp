(in-package #:cl-user)

(defpackage #:dev.zxul767.macrotools
  (:use #:cl #:trivial-indent
        #:dev.zxul767.prelude)
  (:export
   #:once-only
   #:let-guard
   #:prog-nil
   #:with-labels
   #:let-when
   #:let-when*
   #:with-gensyms
   #:let-return
   #:with-safe-io-syntax))
