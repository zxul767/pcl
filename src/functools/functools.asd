(defpackage :dev.zxul767.functools-system (:use :asdf :cl))

(in-package :dev.zxul767.functools-system)

(defsystem functools
  :name "functools"
  :author "Willebaldo Gomez <willebaldo.gomez@gmail.com>"
  :version "0.0.1"
  :maintainer "Willebaldo Gomez <willebaldo.gomez@gmail.com>"
  :license "BSD"
  :description "A package for higher-order functions"
  :components
  ((:file "packages")
   (:file "functools" :depends-on ("packages")))
  :in-order-to ((test-op (test-op "functools/tests"))))

(defsystem functools/tests
  :name "functools/tests"
  :author "Willebaldo Gomez <willebaldo.gomez@gmail.com>"
  :version "0.0.1"
  :maintainer "Willebaldo Gomez <willebaldo.gomez@gmail.com>"
  :license "BSD"
  :description "Test suite for `functools`"
  :depends-on (:functools :fiveam)
  :components
  ((:module "tests"
    :components ((:file "packages")
                 (:file "main" :depends-on ("packages")))))
  :perform
  (test-op (o s)
           (uiop:symbol-call :fiveam :run!
                             (uiop:find-symbol* :master-suite :dev.zxul767.functools-tests))))
