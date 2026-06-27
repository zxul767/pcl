(defpackage :dev.zxul767.pathnames-system (:use :asdf :cl))
(in-package :dev.zxul767.pathnames-system)

(defsystem pathnames
  :name "pathnames"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "0.0.1"
  :maintainer "Willebaldo Gomez <willebaldo.gomez@gmail.com>"
  :license "BSD"
  :description "A pathnames portable library"
  :components
  ((:file "packages")
   (:file "pathnames" :depends-on ("packages"))))
