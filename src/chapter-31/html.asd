(defpackage :dev.zxul767.html-system (:use :asdf :cl))
(in-package :dev.zxul767.html-system)

(defsystem html
  :name "html"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "1.0"
  :maintainer "Willebaldo Gomez <willebaldo.gomez@gmail.com>"
  :licence "BSD"
  :description "HTML and CSS generation from sexps."
  :long-description ""
  :components
  ((:file "packages")
   (:file "html" :depends-on ("packages"))
   (:file "css" :depends-on ("packages" "html")))
  :depends-on (:macrotools))
