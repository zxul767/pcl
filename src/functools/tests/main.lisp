(in-package :dev.zxul767.functools-tests)

(def-suite master-suite
  :description "The master test suite for `functools` subsystem")

(def-suite negate-suite
  :description "Test `negate` function"
  :in master-suite)

(in-suite negate-suite)

(test should-invert-boolean-function
  (let ((my-even-p (negate #'oddp)))
    (is (funcall my-even-p 2))
    (is (not (funcall my-even-p 1)))))

(test should-be-noop-when-applied-twice
  (let ((my-odd-p (negate (negate #'oddp))))
    (is (funcall my-odd-p 1))
    (is (not (funcall my-odd-p 2)))))
