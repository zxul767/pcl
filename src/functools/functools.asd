(defpackage :dev.zxul767.functools-system (:use :asdf :cl))
(in-package :dev.zxul767.functools-system)

(defsystem functools
  :name "functools"
  :author "Willebaldo Gomez <willebaldo.gomez@gmail.com>"
  :version "0.0.1"
  :maintainer "Willebaldo Gomez <willebaldo.gomez@gmail.com>"
  :license "BSD"
  :description "A package for higher-order functions"
  :components
  ((:file "packages")
   (:file "functools" :depends-on ("packages"))))
