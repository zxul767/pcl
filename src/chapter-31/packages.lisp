(in-package :cl-user)

(defpackage :dev.zxul767.html
  (:use :common-lisp :dev.zxul767.macrotools)
  (:export :with-html-output
           :with-html-to-file
           :in-html-style
           :define-html-macro
           :define-css-macro
           :css
           :html
           :emit-css
           :emit-html
           :&attributes))
