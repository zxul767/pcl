(in-package #:dev.zxul767.prelude)

;; we want special indentation (the main expression to be indented more):
;; (with-labels
;;     <main-sexp>
;;   (<fn-name-1> ...)
;;   (<fn-name-2> ...))
(indent:define-indentation
    with-labels ((&whole 4 &rest 4) &rest (&whole 2 4 &rest 2)))

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

;; The following functions are used within one or more macros, so they need
;; to be available in the compilation "runtime image", just like other
;; primitives, if such macros are used in other functions or macros being
;; compiled here or in another file that depends on this one.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gensyms (count)
    (loop repeat count collect (gensym))))

(defmacro sort! (sequence predicate &rest args)
  (assert (symbolp sequence))
  `(setf ,sequence (sort ,sequence ,predicate ,@args)))

(defmacro with-safe-io-syntax (&body body)
  `(with-standard-io-syntax
     (let ((*read-eval* nil))
       ,@body)))

(defmacro with-labels (form &body definitions)
  `(labels ,definitions ,form))

(defmacro let-guard ((var condition) error &body body)
  (assert (symbolp var))
  `(let ((,var ,condition))
     (if ,var (progn ,@body) ,error)))

(defmacro let-when (forms &body body)
  (let ((variables (mapcar #'car forms)))
    `(let ,forms
       (when (and ,@variables)
         ,@body))))

(defmacro let-when* (forms &body body)
  (with-labels
      (build-expansion forms)
    (build-expansion (forms)
      (if (null forms)
          `(progn ,@body)
          `(let-when (,(car forms))
             ,(build-expansion (cdr forms)))))))

(defmacro let-return ((result &optional initform) &body body)
  `(let ((,result ,initform))
     ,@body
     ,result))

(defmacro progn-nil (&body body)
  `(progn ,@body nil))

(defmacro with-gensyms ((&rest symbols) &body body)
  "Wrap `body' within a new lexical environment where each symbol in `symbols'
is bound to a a \"gensym\" (a unique symbol guaranteed to not clash with any
symbol in any package.)

`symbols' can be of the form (s1 s2 ...) or ((s1 h1) (s2 h2) ...) or a combination
thereof, where hX is a hint (a string used to prefix gensym'd variables.)

Hints are used to make it easier to debug macro expansions. If not hints are given,
the name of the symbol is used by default as a hint.

Example:
(defmacro test-macro (@rest args)
  (with-gensyms (a (b \"b-hint\"))
    `(let ((,a 0) (,b 1))
       (+ ,a ,b ,@args))))

(macroexpand-1 '(test-macro 2 3))
=>
(LET ((#:A598 1) (#:|b-hint599| 2))
  (+ #:A598 #:|b-hint599| 2 3))
"
  (with-labels
      (let ((hints (mapcar #'extract-hint symbols))
            (names (mapcar #'extract-name symbols)))
        `(let ,(loop for name in names
                     for hint in hints
                     collect `(,name (gensym ,hint)))
           ,@body))
    ;; helpers
    (extract-hint (symbol)
      (or (and (consp symbol) (second symbol))
          (string symbol)))
    (extract-name (symbol)
      (or (and (consp symbol) (first symbol))
          symbol))))

(defmacro once-only ((&rest names) &body body)
  "Generate a macro template that, upon expansion, will rewrite `body' to guarantee that:
1. Each value (an expression) in `names' is evaluated exactly once, and
2. There is no need to manually introduce gensyms to acomplish (1)

Example:
(macroexpand-1 '(once-only (n) `(fibonacci ,n (1- ,n))))
=>
(LET ((#:G558 N) (N (GENSYM \"N\")))
  `(LET (,`(,N ,#:G558))
     ,`(FIBONACCI ,N (1- ,N))))

When used within a macro (as it is intended):
(defmacro test (n)
  (once-only (n)
    `(fibonacci ,n (1- ,n))))

the final expansion would be:
(macroexpand-1 '(test (+ 1 2)))
=>
(LET ((#:N557 (+ 1 2)))
   (FIBONACCI #:N557 (1- #:N557)))
"
  (let ((names-symbols (zip names (gensyms (length names)))))
    `(let (,@(loop for (n s) in names-symbols collect `(,s ,n))
           ,@(loop for n in names collect `(,n (gensym ,(string n)))))
       `(let (,,@(loop for (n s) in names-symbols collect ``(,,n ,,s)))
          ,,@body))))

;; this is the original definition in the book (minus some minor modifications
;; which don't modify the basic shape):
;;
;; (let ((names-gensyms (zip names (gensyms (length names)))))
;;   `(let (,@(loop for (n g) in names-gensyms collect `(,g (gensym ,(string n)))))
;;      `(let (,,@(loop for (n g) in names-gensyms collect ``(,,g ,,n)))
;;         ,(let (,@(loop for (n g) in names-gensyms collect `(,n ,g)))
;;            ,@body))))
;;
;; the version above is an alternative--and IMO simpler--definition, as it avoids
;; one nesting level by taking advantage of the fact that (let ((n <expr>) (g n)) ...)
;; rebinds `n' to a new value, but also binds `g' to the previous value of `n' at
;; the same time (think of it as `psetf')
