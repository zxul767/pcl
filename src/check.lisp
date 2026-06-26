(in-package #:cl-user)

(defun asd-files-under (dirpath)
  "Returns all `.asd` files under `dirpath` and nested directories."
  (append (uiop:directory-files dirpath "*.asd")
          (mapcan #'asd-files-under (uiop:subdirectories dirpath))))

(defun tests-system-p (name)
  "Is `name` a test system?"
  (let ((suffix "/tests"))
    (and (<= (length suffix) (length name))
         (string= suffix name :start2 (- (length name) (length suffix))))))

(defun get-source-dirpath ()
  "Returns the path of the source directory of the current file."
  (uiop:pathname-directory-pathname *load-truename*))

(defun get-warnings-filepath ()
  "Returns the path of the warnings log file.

By convention, it is placed on the parent directory of the source directory."
  (merge-pathnames
   "warnings.log"
   (uiop:pathname-parent-directory-pathname (get-source-dirpath))))

(defun get-local-system-names (dirpath)
  ;; ASDF requires each primary system to have the same name as its .asd file.
  ;; Secondary systems, such as "functools/tests", are tested through their
  ;; primary system and therefore do not need to be discovered separately.
  (sort
   (remove-duplicates
    (loop for file in (asd-files-under dirpath)
          for name = (pathname-name file)
          unless (tests-system-p name) collect name)
    :test #'string=)
   #'string<))

(defun resolve-dependency-names (dependency)
  "Returns the active system names from an ASDF dependency specification.

Plain dependencies may be strings or symbols.  ASDF also supports
(:FEATURE expression dependency), which applies only when EXPRESSION matches
the current Lisp's *FEATURES*, and (:VERSION dependency version), which adds a
minimum-version constraint.  This function selects names only so Quicklisp can
download their releases; ASDF still validates version constraints when it
loads and tests the systems."
  (etypecase dependency
    ;; ASDF may expose plain names as symbols even when the original DEFSYSTEM
    ;; form used strings.
    (string (list dependency))
    (symbol (list (string-downcase dependency)))
    (cons
     (ecase (first dependency)
       (:feature
        ;; FEATUREP understands compound CL feature expressions such as
        ;; (:AND :SBCL (:NOT :WINDOWS)).  An inactive dependency yields NIL.
        (when (uiop:featurep (second dependency))
          (resolve-dependency-names (third dependency))))
       (:version
        ;; Installation only needs the nested system name.  ASDF retains and
        ;; later enforces the version requirement itself.
        (resolve-dependency-names (second dependency)))))))

(defun ensure-system-dependencies (system-name &optional seen)
  "Ensure all direct and transitive dependencies for `system-name` are locally available.

It DOES NOT compile/load any such dependencies the way `(ql:quickload ...)` would do.
"
  (if (member system-name seen :test #'string=)
      seen
      (let ((seen (cons system-name seen)) ;; track systems to avoid infinite loops
            (asdf-system (asdf:find-system system-name nil))
            (quicklisp-system (ql-dist:find-system system-name)))
        (cond
          (asdf-system
           ;; if a system is already in the ASDF registry, we still need to ensure
           ;; all its direct and transitive dependencies are locally available too.
           (dolist (dependency (asdf:system-depends-on asdf-system))
             (dolist (name (resolve-dependency-names dependency))
               (setf seen (ensure-system-dependencies name seen)))))
          (quicklisp-system
           ;; this is what actually downloads and registers third-party dependencies.
           (ql-dist:ensure-installed quicklisp-system)
           (dolist (dependency (ql-dist:required-systems quicklisp-system))
             (setf seen (ensure-system-dependencies dependency seen))))
          (t
           (error "ASDF/Quicklisp dependency ~s was not found." system-name)))
        seen)))

(defun ensure-all-dependencies (system-names)
  "Ensure all direct and transitive dependencies for `system-names`
(and their respective test systems) are locally available."
  (let (seen)
    (dolist (system-name system-names)
      ;; install the system itself...
      (setf seen (ensure-system-dependencies system-name seen))
      (let ((tests-system-name (format nil "~a/tests" system-name)))
        (when (asdf:find-system tests-system-name nil)
          ;; ...and its test system
          (setf seen (ensure-system-dependencies tests-system-name seen)))))))

(defun run-system-test (system-name index)
  (handler-case
      (progn
        (format t "~&~a. Testing ASDF system ~a...~%" index system-name)
        (asdf:test-system system-name)
        nil)
    (error (condition)
      (format *error-output* "~&ASDF system ~a failed: ~a~%"
              system-name condition)
      (cons system-name condition))))

(defun run-system-tests (system-names)
  (loop for i = 0 then (1+ i) for name in system-names
        for failure = (run-system-test name i)
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
  "Run `function`, capturing warnings to a file when `verbose` is `nil`."
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

(defun run-tests-under (source-dirpath &key verbose)
  "Runs all tests under `source-dirpath`.
Returns 0 if there are no failures, and 1 if there are any failures."
  (handler-case
      (let ((system-names (get-local-system-names source-dirpath)))
        ;; Install third-party dependencies before opening an ASDF session
        ;; so Quicklisp downloads do not invalidate its action plan.
        (ensure-all-dependencies system-names)

        (asdf/session:with-asdf-session (:override t)
          ;; Compile and load every discovered primary system.  Using the same
          ;; discovery as the test phase keeps new .asd files covered by CI.
          (ql:quickload system-names :silent (not verbose) :verbose verbose)
          ;; Run all subsystems' tests.
          (let ((failures (run-system-tests system-names)))
            (when failures
              (error "~d ASDF system~:p failed project checks."
                     (length failures))))
          (format t "~%ALL PROJECT CHECKS PASSED~%"))
        0) ;; success exit code
    (error (condition)
      (format *error-output* "~&PROJECT CHECKS FAILED: ~a~%" condition)
      1))) ;; failure exit code

(defun run-tests-and-quit ()
  "Run all tests for the project and quit with exit status."
  (let ((verbose (verbose-checks-p)))
    (uiop:quit
     (run-function
      (lambda () (run-tests-under (get-source-dirpath) :verbose verbose))
      :verbose verbose))))

(defun main ()
  (run-tests-and-quit))

#+run-main
(apply #'main roswell:*argv*)
