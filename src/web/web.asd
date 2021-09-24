(defpackage :dev.zxul767.web-system (:use :asdf :cl))
(in-package :dev.zxul767.web-system)

(defsystem web
  :name "web"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "1.0"
  :maintainer "Willebaldo Gomez <willebaldo.gomez@gmail.com>"
  :licence "BSD"
  :description "Web server to stream mp3 files"
  :components
  ((:file "packages")
   (:file "routes" :depends-on ("web"))
   (:file "web" :depends-on ("packages")))
  :depends-on (:aserve :macrotools :html))
