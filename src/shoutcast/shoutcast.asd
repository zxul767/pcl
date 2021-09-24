(defpackage :dev.zxul767.shoutcast-system (:use :asdf :cl))
(in-package :dev.zxul767.shoutcast-system)

(defsystem shoutcast
  :name "shoutcast"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "1.0"
  :maintainer "Willebaldo Gomez <willebaldo.gomez@gmail.com>"
  :licence "BSD"
  :description "Shoutcast protocol implementation"
  :components
  ((:file "packages")
   (:file "shoutcast" :depends-on ("packages")))
  :depends-on (:aserve :macrotools :id3v2))
