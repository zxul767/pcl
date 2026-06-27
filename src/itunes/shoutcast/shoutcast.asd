(defsystem "shoutcast"
  :name "shoutcast"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "1.0"
  :maintainer "Willebaldo Gomez <willebaldo.gomez@gmail.com>"
  :licence "BSD"
  :description "Shoutcast protocol implementation"
  :components
  ((:file "packages")
   (:file "shoutcast" :depends-on ("packages")))
  :depends-on (:aserve :prelude :id3v2))
