(defsystem "spam-filter"
  :name "spam-filter"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "1.0"
  :maintainer "Willebaldo Gomez <willebaldo.gomez@gmail.com>"
  :licence "BSD"
  :description "A bayesian spam filter"
  :components
  ((:file "packages")
   (:file "spam-filter" :depends-on ("packages")))
  :depends-on (:cl-ppcre :pathnames))
