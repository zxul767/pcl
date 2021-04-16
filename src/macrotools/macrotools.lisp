(in-package :dev.zxul767.macrotools)

(defmacro fn (args &body body)
  `#'(lambda ,args ,@body))

;; The following functions are used within one or more macros, so they need
;; to be available in the compilation "runtime image", just like other primitives
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gensyms (count)
    (loop repeat count collect (gensym)))

  (defun zip (&rest lists)
    (apply #'mapcar #'list lists)))

(defun as-keyword (symbol)
  "Gets or creates a keyword symbol with the same name as `symbol'"
  (intern (string symbol) :keyword))

(defun mappend (fn &rest lsts)
  "Maps elements in list and finally appends all resulted lists."
  (apply #'append (apply #'mapcar fn lsts)))

(defmacro with-labels (form &body definitions)
  `(labels ,definitions ,form))

(indent:define-indentation
    with-labels ((&whole 4 &rest 4) &rest (&whole 2 4 &rest 2)))
(indent:initialize-slime)

(defmacro when-bind (forms &body body)
  (let ((variables (mapcar #'car forms)))
    `(let ,forms
       (when (and ,@variables)
         ,@body))))

(defmacro when-bind* (forms &body body)
  (with-labels
      (build-expansion forms)
    (build-expansion (forms)
      (if (null forms)
          `(progn ,@body)
          `(when-bind (,(car forms))
             ,(build-expansion (cdr forms)))))))

(defmacro with-result ((result &optional initform) &body body)
  `(let ((,result ,initform))
     ,@body
     ,result))

(defmacro prog-nil (&body body)
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
        `(let ,(loop for name in names for hint in hints collect `(,name (gensym ,hint)))
           ,@body))
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

(defmacro condlet (clauses &body body)
  "Execute `body' in the lexical context established by the bindings
defined by the first clause whose condition evaluates to non-nil.

Example:
(condlet (((= 2 2) (x (+ 1 2)) (y (+ 3 4)))
          ((= 1 3) (z (+ 5 6)))
          (t (z 1) (y 2) (x 3)))
  (list x y z))

will end up being equivalent to:

(let ((x (+ 1 2)) (y (+ 3 4)))
  (list x y z))
"
  (with-gensyms (body-fn-name)
    (with-labels
        (let ((variables (extract-all-variables clauses)))
          `(labels ((,body-fn-name (&key ,@variables)
                      ,@body))
             (cond ,@(mapcar #'build-cond-clause clauses))))

      ;; clauses :: (clause+)
      (extract-all-variables (clauses)
        (extract-variables (mappend #'rest clauses)))

      ;; clause :: (condition bind-form+)
      (build-cond-clause (clause)
        (let ((condition (first clause))
              (bind-forms (rest clause)))
          `(,condition
            (let ,bind-forms
              (,body-fn-name ,@(build-invocation-args bind-forms))))))

      ;; bind-forms: ((variable expression) ...)
      (build-invocation-args (bind-forms)
        (loop for var in (extract-variables bind-forms)
              append `(,(as-keyword var) ,var)))

      (extract-variables (bind-forms)
        (remove-duplicates
         (mapcar #'first bind-forms))))))

(defun map-range (start end mapper)
  (loop for i from start below end
        collect (funcall mapper i)))

(defun map0-n (n mapper)
  (map-range 0 n mapper))

(defmacro do-tuples/close (tuple sequence &body body)
  "Repeatedly evaluate `body' with `tuple' bound to all n-sized, wraparound,
contiguous subsequences of `sequence', where `n' is the size of `tuple'.

For subsequences smaller than `n', the remaining elements are obtained by
'wrapping around the sequence'. This behavior is what distinguishes this
macro from `do-tuples/open'.

Example:
(do-tuples/close (x y z) '(a b c d)
   (princ (list x y z)))
=>
(A B C)(B C D)(C D A)(D A B)
NIL
"
  (assert (not (null tuple)) (tuple) "A non-empty tuple is required!")
  (with-labels
      (with-gensyms (body-fn-name)
        (once-only (sequence)
          `(when (nthcdr ,(1- (length tuple)) ,sequence)
             (labels ((,body-fn-name ,tuple ,@body))
               (do ((subsequence ,sequence (cdr subsequence)))
                   ((not (nthcdr ,(1- (length tuple)) subsequence))
                    ,@(build-wraparound-tuple-calls body-fn-name
                                                    (length tuple)
                                                    sequence
                                                    'subsequence)
                    nil)
                 ,(build-tuple-call body-fn-name
                                    (length tuple)
                                    'subsequence))))))

    (build-tuple-call (body-fn-name size subsequence)
      `(,body-fn-name ,@(map0-n size (fn (n) `(nth ,n ,subsequence)))))

    (build-wraparound-tuple-calls (body-fn-name size sequence subsequence)
      (mapcar (fn (args) `(,body-fn-name ,@args))
              (build-wraparound-tuple-call-args size sequence subsequence)))

    (build-wraparound-tuple-call-args (size sequence subsequence)
      (let ((limit (- size 2)))
        (loop for i upto limit
              collect
              (append (map-range i (1+ limit) (fn (n) `(nth ,n ,subsequence)))
                      (map-range 0 (1+ i) (fn (n) `(nth ,n ,sequence)))))))))
