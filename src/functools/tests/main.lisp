(in-package #:dev.zxul767.functools-tests)

(def-suite master-suite
  :description "The master test suite for `functools` subsystem")

(def-suite negate-suite
  :description "Test `negate` function"
  :in master-suite)

(in-suite negate-suite)

(test should-invert-boolean-function
  (let ((my-even-p (ft:negate #'oddp)))
    (is (funcall my-even-p 2))
    (is (not (funcall my-even-p 1)))))

(test should-be-noop-when-applied-twice
  (let ((my-odd-p (ft:negate (ft:negate #'oddp))))
    (is (funcall my-odd-p 1))
    (is (not (funcall my-odd-p 2)))))

(def-suite and-pipe-suite
  :description "Test `and-fn` macro"
  :in master-suite)

(in-suite and-pipe-suite)

(test should-expand-to-function-pipeline
  (is (equal (macroexpand-1 '(ft:and-pipe sqrt exp))
             '(function
               (lambda (&rest ft::args)
                (and (apply #'sqrt ft::args)
                     (apply #'exp ft::args)))))))

(defun run-tests ()
  (run! 'master-suite))
