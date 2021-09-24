(defpackage :dev.zxul767.id3v2-system (:use :asdf :cl))
(in-package :dev.zxul767.id3v2-system)

(defsystem id3v2
  :name "id3v2"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "1.0"
  :maintainer "Willebaldo Gomez <willebaldo.gomez@gmail.com>"
  :licence "BSD"
  :description "ID3 v2 library for parsing mp3 tags"
  :components
  ((:file "packages")
   (:file "id3v2" :depends-on ("packages")))
  :depends-on (:prelude :macrotools :functools :pathnames :binary))
