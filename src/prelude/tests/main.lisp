(in-package #:dev.zxul767.prelude-tests)

(def-suite prelude-suite
  :description "Test functions/macros in the `prelude` subsystem.")

;; -----------------------------------------------------------------------------
(def-suite zip-suite
  :in prelude-suite)

(in-suite zip-suite)

(test should-zip-two-lists
  (is (equal
       (std:zip '(1 2 3) '(2 4 6))
       '((1 2) (2 4) (3 6)))))

(test should-zip-several-lists
  (is (equal
       (std:zip '(1 2 3)
                '("one" "two" "three")
                '("unu" "doi" "trei")
                '("un" "deux" "trois"))
       '((1 "one" "unu" "un")
         (2 "two" "doi" "deux")
         (3 "three" "trei" "trois")))))

;; -----------------------------------------------------------------------------
(def-suite ensure-list-suite
  :in prelude-suite)

(in-suite ensure-list-suite)

(test ensure-list-is-noop-for-list
  (let ((singleton '(1)))
    (is (eq (std:ensure-list singleton)
            singleton))))

(test ensure-list-wraps-atoms
  (is (equal (std:ensure-list 1)
             '(1))))

;; -----------------------------------------------------------------------------
(def-suite utilities-suite
  :in prelude-suite)

(in-suite utilities-suite)

(test as-keyword-transforms-symbol
  (is (eq (std:as-keyword 'keyword)
          :keyword)))

(test sort!-sorts-sequence-in-place
  (let ((seq '(5 4 3 2 1)))
    (is (equal (std:sort! seq #'<)
               '(1 2 3 4 5)))))

;; -----------------------------------------------------------------------------
(def-suite let-when-suite
  :in prelude-suite)

(in-suite let-when-suite)

(test should-return-nil-when-any-binding-is-nil
  (is (null (std:let-when ((a 1) (b nil))
              (* a b)))))

(test should-eval-body-when-all-bindings-are-nonnil
  (is (equal (std:let-when ((a 1) (b 2) (c 3))
               (* a b c))
             6)))

;; -----------------------------------------------------------------------------
(def-suite let-return-suite
  :in prelude-suite)

(in-suite let-return-suite)

(test binds-result-to-nil-by-default
  (is (null (std:let-return (id)
              (format nil "testing `let-return`")))))

(test binds-result-to-initial-value
  (is (equal (std:let-return (id 1)
               (format nil "testing `let-return`"))
             1)))

(test returns-modified-result
  (let ((result (std:let-return (ids)
                  (push 1 ids))))
    (is (equal result '(1)))))

;; -----------------------------------------------------------------------------
(defun run-tests ()
  (run! 'prelude-suite))
