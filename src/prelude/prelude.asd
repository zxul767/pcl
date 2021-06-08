(defpackage :dev.zxul767.prelude-system (:use :asdf :cl))
(in-package :dev.zxul767.prelude-system)

(defsystem prelude
  :name "prelude"
  :author "Willebaldo Gomez <willebaldo.gomez@gmail.com>"
  :version "0.0.1"
  :maintainer "Willebaldo Gomez <willebaldo.gomez@gmail.com>"
  :license "BSD"
  :description "Basic library functions that extend the CL standard library"
  :components
  ((:file "packages")
   (:file "prelude" :depends-on ("packages"))))
