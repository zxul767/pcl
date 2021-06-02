(in-package :cl-user)

(defpackage :dev.zxul767.web
  (:use
   :common-lisp
   :net.aserve
   :dev.zxul767.html
   :dev.zxul767.macrotools)
  (:export
   :define-html-handler))
