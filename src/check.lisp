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

(defun dependency-name (dependency)
  (etypecase dependency
    (string dependency)
    (symbol (string-downcase dependency))
    (cons (dependency-name (first dependency)))))

(defun ensure-system-dependencies-installed (system-name &optional seen)
  (unless (member system-name seen :test #'string=)
    (let ((seen (cons system-name seen))
          (asdf-system (asdf:find-system system-name nil))
          (quicklisp-system (ql-dist:find-system system-name)))
      (cond
        (asdf-system
         (dolist (dependency (asdf:system-depends-on asdf-system))
           (ensure-system-dependencies-installed
            (dependency-name dependency) seen)))
        (quicklisp-system
         (ql-dist:ensure-installed quicklisp-system)
         (dolist (dependency (ql-dist:required-systems quicklisp-system))
           (ensure-system-dependencies-installed dependency seen)))
        (t
         (error "ASDF/Quicklisp dependency ~s was not found." system-name))))))

(defun install-project-dependencies (system-names)
  (ensure-system-dependencies-installed *main-system*)
  (dolist (system-name system-names)
    (let ((tests-system-name (format nil "~a/tests" system-name)))
      (when (asdf:find-system tests-system-name nil)
        (ensure-system-dependencies-installed tests-system-name)))))

(defun run-system-test (system-name)
  (handler-case
      (progn
        (format t "~&Testing ASDF system ~a...~%" system-name)
        (asdf:test-system system-name)
        nil)
    (error (condition)
      (format *error-output* "~&ASDF system ~a failed: ~a~%"
              system-name condition)
      (cons system-name condition))))

(defun run-system-tests (system-names)
  (loop for name in system-names
        for failure = (run-system-test name)
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
      (let ((system-names (local-system-names source-directory)))
        ;; Install third-party dependencies before opening an ASDF session
        ;; so Quicklisp downloads do not invalidate its action plan.
        (install-project-dependencies system-names)

        (asdf/session:with-asdf-session (:override t)
          ;; Compile and load the top-level application and all dependent subsystems.
          (ql:quickload *main-system* :silent (not verbose) :verbose verbose)
          ;; Run all subsystems' tests.
          (let ((failures (run-system-tests system-names)))
            (when failures
              (error "~d ASDF system~:p failed project checks."
                     (length failures))))
          (format t "~&ALL PROJECT CHECKS PASSED~%")
          0))
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
