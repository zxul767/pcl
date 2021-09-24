(defpackage :dev.zxul767.spam-system (:use :asdf :cl))
(in-package :dev.zxul767.spam-system)

(defsystem spam
  :name "spam"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "1.0"
  :maintainer "Willebaldo Gomez <willebaldo.gomez@gmail.com>"
  :licence "BSD"
  :description "Spam filter"
  :components
  ((:file "packages")
   (:file "spam" :depends-on ("packages")))
  :depends-on (:cl-ppcre :pathnames))
