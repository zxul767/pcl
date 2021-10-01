(in-package :dev.zxul767.prelude)

(defun zip (&rest lists)
  (apply #'mapcar #'list lists))

(defun mappend (mapper &rest lists)
  (apply #'append (apply #'mapcar mapper lists)))

(defun as-keyword (symbol)
  "Gets or creates a keyword symbol with the same name as `symbol'"
  (intern (string symbol) :keyword))

(defun ensure-list (x)
  "Ensure that x is always a list"
  (if (listp x) x (list x)))
