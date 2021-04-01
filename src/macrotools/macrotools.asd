(defpackage :dev.zxul767.macrotools-system (:use :asdf :cl))
(in-package :dev.zxul767.macrotools-system)

(defsystem macrotools
  :name "macrotools"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "0.0.1"
  :maintainer "Willebaldo Gomez <willebaldo.gomez@gmail.com>"
  :license "BSD"
  :description "A package for macro writing tools"
  :components
  ((:file "packages")
   (:file "macrotools" :depends-on ("packages"))))
