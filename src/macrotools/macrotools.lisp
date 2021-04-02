(in-package :dev.zxul767.macrotools)

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
  (labels
      ((extract-hint (symbol)
         (or (and (consp symbol) (second symbol))
             (string symbol)))
       (extract-name (symbol)
         (or (and (consp symbol) (first symbol))
             symbol)))
    (let ((hints (mapcar #'extract-hint symbols))
          (names (mapcar #'extract-name symbols)))
      `(let ,(loop for name in names for hint in hints collect `(,name (gensym ,hint)))
         ,@body))))

;; TODO:
;; + rewrite to simplify and improve readability
;; + document
(defmacro once-only ((&rest names) &body body)
  (let ((symbols (loop for n in names collect (gensym))))
    `(let (,@(loop for s in symbols collect `(,s (gensym))))
       `(let (,,@(loop for s in symbols for n in names collect ``(,,s ,,n)))
          ,(let (,@(loop for n in names for s in symbols collect `(,n ,s)))
             ,@body)))))
