(defsystem "prelude"
  :name "prelude"
  :author "Willebaldo Gomez <willebaldo.gomez@gmail.com>"
  :version "0.0.1"
  :maintainer "Willebaldo Gomez <willebaldo.gomez@gmail.com>"
  :license "BSD"
  :description "Basic library functions that extend the CL standard library"
  :components
  ((:file "packages")
   (:file "prelude" :depends-on ("packages")))
  :depends-on (:trivial-indent)
  :in-order-to ((test-op (test-op "prelude/tests"))))


(defsystem "prelude/tests"
  :name "prelude/tests"
  :author "Willebaldo Gomez <willebaldo.gomez@gmail.com>"
  :version "0.0.1"
  :maintainer "Willebaldo Gomez <willebaldo.gomez@gmail.com>"
  :license "BSD"
  :description "Test suite for `prelude`"
  :depends-on ("prelude" "fiveam")
  :components
  ((:module "tests"
    :components ((:file "packages")
                 (:file "main" :depends-on ("packages")))))
  :perform
  (test-op (o s)
           (declare (ignore o))
           (unless (uiop:symbol-call
                    :dev.zxul767.prelude-tests :run-tests)
             (error "Tests failed for ASDF system ~a."
                    (asdf:component-name s)))))
