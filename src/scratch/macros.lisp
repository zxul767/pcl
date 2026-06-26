;; The following are a set of macros which are not actually used anywhere
;; in the mp3 browser project, but are useful exercises to master macros.
;; 
;; TODO: turn them into a package that depends on the `prelude` package
;; where the `with-gensyms` and `with-labels` macros live.

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


(defmacro do-tuples/close (tuple source &body body)
  "Evaluate `body' N times with `tuple' bound to every M-sized contiguous
subsequence of `source' (including 'wraparound' ones), where N is the size
of `source' and M is the size of `tuple'.

Wrapround subsequences happen when the number of remaining elements to
process from `source' is smaller than M, so we need to wrap around and
take elements from the start of `source'. Including wraparound
subsequences is what distinguishes this macro from `do-tuples/open'.

Example:
(do-tuples/close (x y z) '(a b c d)
   (princ (list x y z)))
=>
(A B C)(B C D)(C D A)(D A B)
NIL
"
  (assert (not (null tuple)) (tuple) "A non-empty tuple is required!")
  (with-labels
      (with-gensyms (body-fn-name rest)
        (once-only (source)
          `(when (nthcdr ,(1- (length tuple)) ,source)
             (labels ((,body-fn-name ,tuple ,@body))
               ;; It seems like `rest' cannot suffer from variable capture
               ;; problems, given the structure of the expansion code, so
               ;; we don't need a gensym'ed symbol for it
               (do ((,rest ,source (cdr ,rest)))
                   ((not (nthcdr ,(1- (length tuple)) ,rest))
                    ,@(build-wraparound-tuple-calls body-fn-name
                                                    (length tuple)
                                                    source
                                                    rest)
                    nil)
                 ,(build-tuple-call body-fn-name
                                    (length tuple)
                                    rest))))))

    (build-tuple-call (body-fn-name tuple-size rest)
                      `(,body-fn-name ,@(loop for n below tuple-size collect `(nth ,n ,rest))))

    (build-wraparound-tuple-calls (body-fn-name tuple-size source rest)
                                  (mapcar #'(lambda (args) `(,body-fn-name ,@args))
                                          (build-wraparound-tuple-call-args tuple-size source rest)))

    (build-wraparound-tuple-call-args (tuple-size source rest)
                                      (let ((limit (- tuple-size 2)))
                                        (loop for i upto limit
                                              collect
                                              (append (loop for n from i upto limit collect `(nth ,n ,rest))
                                                      (loop for n from 0 upto i collect `(nth ,n ,source))))))))
