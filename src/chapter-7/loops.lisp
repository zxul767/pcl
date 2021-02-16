;; dolist macro

(dolist (x '(1 2 3 4 5))
  (print x)
  (if (evenp x) (return)))

;; dotimes macro

(dotimes (x 20)
  (dotimes (y 20)
    (format t "~3d " (* (1+ x) (1+ y))))
  (format t "~%"))

;; do macro

(do ((i 0 (1+ i)))
    ((>= i 4))
  (print i))

;; loop macro

;; https://en.wikipedia.org/wiki/Collatz_conjecture
(defun collatz (n)
  (loop
    for i = 0 then (1+ i)
    while (not (= 1 n))
    do
       (if (evenp n)
           (setf n (/ n 2))
           (setf n (1+ (* 3 n))))
    finally (return i)))

(loop for i from 1 to 10 collecting i) ; --> (1 2 3 4 5 6 7 8 9 10)

(loop for x from 1 to 10 summing (expt x 2)) ; --> 385

;; macros don't have access to runtime information of the values pointed to
;; by symbols, so it's not possible to tell whether the collection being
;; iterated over is a list or a vector. the only way to give the macro a hint
;; about this is by use of a conventional interface, namely the keyword `across'

(loop for x across "the quick brown fox jumps over the lazy dog"
      counting (find x "aeiou")) ; --> 11

;; compute the nth fibonacci number
(defun fibonacci (n)
  (loop for i below n
        and a = 0 then b
        and b = 1 then (+ b a)
        finally (return a)))
