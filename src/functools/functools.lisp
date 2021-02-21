(in-package :dev.zxul767.functools)

(defun negate (function)
  (lambda (&rest args)
    (not (apply function args))))
