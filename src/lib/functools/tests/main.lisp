(in-package #:dev.zxul767.functools-tests)

(def-suite master-suite
  :description "The master test suite for the `functools` subsystem.")

;; -----------------------------------------------------------------------------
(def-suite and-pipe-suite
  :in master-suite)

(in-suite and-pipe-suite)

(test should-expand-to-function-pipeline
  (is (equal (macroexpand-1 '(ft:and-pipe sqrt exp))
             `(lambda (&rest ft::args)
                (and (apply #'sqrt ft::args)
                     (apply #'exp ft::args))))))

(defun run-tests ()
  (run! 'master-suite))
