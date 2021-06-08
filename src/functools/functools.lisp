(in-package :dev.zxul767.functools)

(defmacro fn-and (&rest functions)
  "(funcall (fn-and is-mp3 has-id3) file) <=> (lambda (file) (and (is-mp3 file) (has-id3 file)))"
  `#'(lambda (&rest args)
       (and ,@(loop for fn in functions collect `(apply #',fn args)))))

(defun negate (function)
  (lambda (&rest args)
    (not (apply function args))))
