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
  :depends-on ("functools" "fiveam")
  :components
  ((:module "tests"
    :components ((:file "packages")
                 (:file "main" :depends-on ("packages")))))
  :perform
  (test-op (o s)
           (declare (ignore o))
           (unless (uiop:symbol-call
                    :dev.zxul767.functools-tests :run-tests)
             (error "Tests failed for ASDF system ~a."
                    (asdf:component-name s)))))
