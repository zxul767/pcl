(in-package #:dev.zxul767.functools)

(defmacro and-pipe (&rest functions)
  "Generate a function that ANDs `functions` in a short-circuiting pipeline.
All functions are assumed to have the same signature."
  `(lambda (&rest args)
     (and ,@(loop for fn in functions collect `(apply #',fn args)))))
