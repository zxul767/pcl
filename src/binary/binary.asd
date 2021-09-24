(defpackage :dev.zxul767.binary-system (:use :asdf :cl))
(in-package :dev.zxul767.binary-system)

(defsystem binary
  :name "binary"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "1.0"
  :maintainer "Willebaldo Gomez <willebaldo.gomez@gmail.com>"
  :licence "BSD"
  :description "Binary data I/O library"
  :components
  ((:file "packages")
   (:file "binary" :depends-on ("packages")))
  :depends-on (:prelude :macrotools :trivial-indent))
