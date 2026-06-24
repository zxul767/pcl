(in-package #:cl-user)

(defvar *main-system* "mp3-browser")

(defun asd-files-under (directory)
  (append (uiop:directory-files directory "*.asd")
          (mapcan #'asd-files-under (uiop:subdirectories directory))))

(defun tests-system-p (name)
  (let ((suffix "/tests"))
    (and (<= (length suffix) (length name))
         (string= suffix name :start2 (- (length name) (length suffix))))))

(defun get-source-directory ()
  (uiop:pathname-directory-pathname *load-truename*))

(defun get-warnings-filepath ()
  (merge-pathnames
   "warnings.log"
   (uiop:pathname-parent-directory-pathname (get-source-directory))))

(defun local-system-names (directory)
  ;; ASDF requires each primary system to have the same name as its .asd file.
  ;; Secondary systems, such as "functools/tests", are tested through their
  ;; primary system and therefore do not need to be discovered separately.
  (sort
   (remove-duplicates
    (loop for file in (asd-files-under directory)
          for name = (pathname-name file)
          unless (tests-system-p name) collect name)
    :test #'string=)
   #'string<))

(defun run-system-test (system-name &key verbose)
  (handler-case
      (progn
        (format t "~&Testing ASDF system ~a...~%" system-name)

        ;; load the tests system via quicklisp so we can fetch any required
        ;; third-party dependencies first (e.g., "fiveam")
        (let ((system-tests-name (format nil "~a/tests" system-name)))
          (if (asdf:find-system system-tests-name nil)
              (ql:quickload system-tests-name :silent (not verbose) :verbose verbose)))

        (asdf:test-system system-name)
        nil)
    (error (condition)
      (format *error-output* "~&ASDF system ~a failed: ~a~%"
              system-name condition)
      (cons system-name condition))))

(defun run-system-tests (system-names &key verbose)
  (loop for name in system-names
        for failure = (run-system-test name :verbose verbose)
        when failure collect failure))

(defun verbose-checks-p ()
  (member (string-downcase (or (uiop:getenv "CHECK_VERBOSE") ""))
          '("1" "true" "yes")
          :test #'string=))

(defun write-warning (condition stream number)
  (format stream "~&[~d] ~s~%~a~2%" number (type-of condition) condition))

(defun report-suppressed-warnings (count warnings-file)
  (when (plusp count)
    (format t "~&Suppressed ~d warning~:p; details written to ~a. ~
               Rerun with CHECK_VERBOSE=1 to print them.~%"
            count (namestring warnings-file))))

(defun run-function (function &key verbose)
  (let ((warning-count 0)
        (warnings-file (get-warnings-filepath)))
    (flet ((invoke-with-warning-capture (warnings-stream)
             (handler-bind
                 ((warning
                    (lambda (condition)
                      (unless verbose
                        (incf warning-count)
                        (write-warning condition warnings-stream warning-count)
                        (muffle-warning condition)))))
               (let ((*compile-verbose* verbose)
                     (*compile-print* verbose)
                     (*load-verbose* verbose))
                 (funcall function)))))
      (prog1
          (if verbose
              (invoke-with-warning-capture nil)
              (with-open-file (stream warnings-file
                                      :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
                (invoke-with-warning-capture stream)))
        (report-suppressed-warnings warning-count warnings-file)))))

(defun project-check-status (source-directory &key verbose)
  (handler-case
      (asdf/session:with-asdf-session (:override t)
        ;; Compile and load the top-level application and all dependent subsystems.
        (ql:quickload *main-system* :silent (not verbose) :verbose verbose)
        ;; Run all subsystems' tests.
        (let ((failures
                (run-system-tests (local-system-names source-directory) :verbose verbose)))
          (when failures
            (error "~d ASDF system~:p failed project checks."
                   (length failures))))
        (format t "~&ALL PROJECT CHECKS PASSED~%")
        0)
    (error (condition)
      (format *error-output* "~&PROJECT CHECKS FAILED: ~a~%" condition)
      1)))

(defun run-project-checks ()
  (let* ((verbose (verbose-checks-p)))
    (uiop:quit
     (run-function
      (lambda () (project-check-status (get-source-directory) :verbose verbose))
      :verbose verbose))))

(run-project-checks)
