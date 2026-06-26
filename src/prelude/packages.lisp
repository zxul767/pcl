(in-package #:cl-user)

(defpackage #:dev.zxul767.prelude
  (:use #:cl)
  (:export
   #:as-keyword
   #:ensure-list
   #:let-guard
   #:let-when
   #:let-when*
   #:let-return
   #:mappend
   #:once-only
   #:prog-nil
   #:sort!
   #:with-labels
   #:with-gensyms
   #:with-safe-io-syntax
   #:zip))
