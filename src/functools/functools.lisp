(in-package :dev.zxul767.functools)

(defmacro fn-and (&rest functions)
  "Generate a composite function that ANDs `functions`.
All functions are assumed to have the same signature"
  `#'(lambda (&rest args)
       (and ,@(loop for fn in functions collect `(apply #',fn args)))))

(defun negate (function)
  "Generate a function that is the logical opposite of `function'"
  (lambda (&rest args)
    (not (apply function args))))
