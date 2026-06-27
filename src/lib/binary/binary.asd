(defsystem "binary"
  :name "binary"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "1.0"
  :maintainer "Willebaldo Gomez <willebaldo.gomez@gmail.com>"
  :licence "BSD"
  :description "Binary data I/O library"
  :components
  ((:file "packages")
   (:file "binary" :depends-on ("packages"))
   (:file "primitive-types" :depends-on ("binary")))
  :depends-on (:prelude :trivial-indent))
