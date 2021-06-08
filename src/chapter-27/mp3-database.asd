(defpackage :dev.zxul767.mp3-database-system (:use :asdf :cl))
(in-package :dev.zxul767.mp3-database-system)

(defsystem mp3-database
  :name "mp3-database"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "1.0"
  :maintainer "Willebaldo Gomez <willebaldo.gomez@gmail.com>"
  :licence "BSD"
  :description "mp3s metadata database"
  :components
  ((:file "packages")
   (:file "mp3-database" :depends-on ("packages")))
  :depends-on (:str :macrotools :functools :pathnames :id3v2))
