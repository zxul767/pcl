(in-package #:cl-user)

(defun run-project-checks ()
  (handler-case
      (progn
        ;; Loading the top-level application compiles and loads all of the local
        ;; systems in its dependency graph.
        (ql:quickload "mp3-browser")

        ;; TODO: make sure all tests, not just the ones for `functools` are run
        (ql:quickload "functools/tests")
        (unless (uiop:symbol-call
                 '#:dev.zxul767.functools-tests '#:run-tests)
          (error "The test suite failed."))

        (format t "~&ALL PROJECT CHECKS PASSED~%")
        (uiop:quit 0))
    (error (condition)
      (format *error-output* "~&PROJECT CHECKS FAILED: ~a~%" condition)
      (uiop:quit 1))))

(run-project-checks)
