(in-package #:cl-user)

(defpackage #:dev.zxul767.web
  (:use
   #:cl
   #:net.aserve
   #:dev.zxul767.html
   #:dev.zxul767.prelude)
  (:export
   #:define-html-handler))
