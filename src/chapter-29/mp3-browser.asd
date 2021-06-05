(defpackage :dev.zxul767.mp3-browser-system (:use :asdf :cl))
(in-package :dev.zxul767.mp3-browser-system)

(defsystem mp3-browser
  :name "mp3-browser"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "1.0"
  :maintainer "Willebaldo Gomez <willebaldo.gomez@gmail.com>"
  :licence "BSD"
  :description "MP3 web browser"
  :components
  ((:file "packages")
   (:file "playlist" :depends-on ("packages"))
   (:file "playlist-ui" :depends-on ("playlist"))
   (:file "mp3-browser" :depends-on ("packages"))
   (:static-file "mp3-browser.css")
   (:static-file "silentpacket.mp3"))
  :depends-on (:aserve :macrotools :id3v2 :shoutcast :web :mp3-database :html))
