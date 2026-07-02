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

(defun get-test-errors-filepath ()
  "Returns the path of the test errors log file.

By convention, it is placed on the parent directory of the source directory."
  (merge-pathnames
   "test-errors.log"
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

(defun detailed-output-p (verbosity)
  (>= verbosity 1))

(defun test-output-p (verbosity)
  (>= verbosity 2))

(defun full-output-p (verbosity)
  (>= verbosity 3))

(defun call-with-output-to-stream (function stream)
  (with-open-stream (input (make-string-input-stream ""))
    (with-open-stream (io (make-two-way-stream input stream))
      (let ((*standard-output* stream)
            (*error-output* stream)
            (*trace-output* stream)
            (*debug-io* io)
            (*query-io* io)
            (*terminal-io* io))
        (funcall function)))))

(defun call-with-output-suppressed (function)
  (with-open-stream (stream (make-broadcast-stream))
    (call-with-output-to-stream function stream)))

(defmacro with-output-suppressed-unless ((condition) &body body)
  `(if ,condition
       (progn ,@body)
       (call-with-output-suppressed (lambda () ,@body))))

(defun run-system-test (system-name index verbosity test-errors-stream)
  (handler-case
      (progn
        (when (detailed-output-p verbosity)
          (format t "~&~a. Testing ASDF system ~a...~%" index system-name))
        (if (test-output-p verbosity)
            (asdf:test-system system-name)
            (progn
              (format test-errors-stream
                      "~&~a. Testing ASDF system ~a...~%"
                      index system-name)
              (call-with-output-to-stream
               (lambda () (asdf:test-system system-name))
               test-errors-stream)))
        nil)
    (error (condition)
      (format *error-output* "~&ASDF system ~a failed: ~a~%"
              system-name condition)
      (cons system-name condition))))

(defun run-system-tests (system-names verbosity test-errors-stream)
  (loop for i = 0 then (1+ i) for name in system-names
        for failure = (run-system-test name i verbosity test-errors-stream)
        when failure collect failure))

(defun compile-systems (system-names verbosity)
  (dolist (name system-names)
    (when (detailed-output-p verbosity)
      (format t "~&Compiling ASDF system ~a...~%" name))
    ;; `:force t` because we actually want to get any style warnings, so we don't
    ;; want to bypass any cache.
    (with-output-suppressed-unless ((full-output-p verbosity))
      (asdf:compile-system name :force t))))

(defun load-systems (system-names verbosity)
  (dolist (name system-names)
    (when (detailed-output-p verbosity)
      (format t "~&Loading ASDF system ~a...~%" name))
    (with-output-suppressed-unless ((full-output-p verbosity))
      (ql:quickload name
                    :silent (not (full-output-p verbosity))
                    :verbose (full-output-p verbosity)))))

(defun check-verbosity-level ()
  (let ((value (string-downcase (or (uiop:getenv "CHECK_VERBOSE") ""))))
    (cond
      ((member value '("" "0" "false" "no") :test #'string=) 0)
      ((member value '("1" "true" "yes") :test #'string=) 1)
      ((string= value "2") 2)
      ((string= value "3") 3)
      (t (error "Unsupported CHECK_VERBOSE value ~s; expected 0, 1, 2, or 3."
                value)))))

(defun write-warning (condition stream number)
  (format stream "~&[~d] ~s~%~a~2%" number (type-of condition) condition))

(defun report-suppressed-warnings (count warnings-file)
  (when (plusp count)
    (format t "~&Suppressed ~d warning~:p; details written to ~a. ~
               Rerun with CHECK_VERBOSE=3 to print them.~%"
            count (namestring warnings-file))))

(defun report-style-warnings (style-warnings-count)
  (when (plusp style-warnings-count)
    (format *error-output*
            "~&PROJECT CHECKS FAILED: produced ~d style warning~:p.~%"
            style-warnings-count)))

(defun report-test-errors-log (test-errors-file)
  (format *error-output*
          "~&Test output/errors written to ~a. Rerun with CHECK_VERBOSE=2 to print test output.~%"
          (namestring test-errors-file)))

(defun redefinition-warning-p (condition)
  (let ((type (type-of condition)))
    (and (symbolp type)
         (symbol-package type)
         (string= "SB-KERNEL" (package-name (symbol-package type)))
         (let ((name (symbol-name type)))
           (and (<= (length "REDEFINITION-") (length name))
                (string= "REDEFINITION-" name
                         :end2 (length "REDEFINITION-")))))))

(defun ignorable-warning-p (condition)
  (redefinition-warning-p condition))

(defun pathname-prefix-p (prefix pathname)
  (let ((prefix (namestring (truename prefix)))
        (pathname (namestring (truename pathname))))
    (and (<= (length prefix) (length pathname))
         (string= prefix pathname :end2 (length prefix)))))

(defun compiling-project-file-p ()
  (and *compile-file-truename*
       (pathname-prefix-p (get-source-dirpath) *compile-file-truename*)))

(defun handle-warning (condition warnings-stream verbosity warning-count style-warning-count)
  (when (and (typep condition 'style-warning)
             (compiling-project-file-p)
             (not (ignorable-warning-p condition)))
    (incf (car style-warning-count)))
  (cond
    ((and (not (full-output-p verbosity))
          (not (ignorable-warning-p condition)))
     (incf (car warning-count))
     (write-warning condition warnings-stream (car warning-count))
     (muffle-warning condition))
    ((and (not (full-output-p verbosity))
          (ignorable-warning-p condition))
     (muffle-warning condition))))

(defun run-function (function &key verbosity)
  "Run `function`, capturing warnings to a file unless full output is enabled."
  (let ((warning-count (list 0))
        (style-warning-count (list 0))
        (warnings-file (get-warnings-filepath)))
    (flet ((invoke-with-warning-capture (warnings-stream)
             (handler-bind
                 ((warning
                    (lambda (condition)
                      (handle-warning condition
                                      warnings-stream
                                      verbosity
                                      warning-count
                                      style-warning-count))))
               (let ((*compile-verbose* (full-output-p verbosity))
                     (*compile-print* (full-output-p verbosity))
                     (*load-verbose* (full-output-p verbosity)))
                 (funcall function)))))
      (let ((result (if (full-output-p verbosity)
                        (invoke-with-warning-capture nil)
                        (with-open-file (stream warnings-file
                                                :direction :output
                                                :if-exists :supersede
                                                :if-does-not-exist :create)
                          (invoke-with-warning-capture stream)))))
        (report-suppressed-warnings (car warning-count) warnings-file)
        (report-style-warnings (car style-warning-count))
        (if (plusp (car style-warning-count)) 1 result)))))

(defun run-tests-under (source-dirpath &key verbosity)
  "Runs all tests under `source-dirpath`.
Returns 0 if there are no failures, and 1 if there are any failures."
  (handler-case
      (let ((system-names (get-local-system-names source-dirpath)))
        ;; Install third-party dependencies before opening an ASDF session
        ;; so Quicklisp downloads do not invalidate its action plan.
        (format t "~&Ensuring dependencies...~%")
        (ensure-all-dependencies system-names)

        ;; Force compilation of every discovered primary system before
        ;; loading/testing. This makes style warnings reproducible even when
        ;; ASDF's cache is warm.
        (format t "~&Compiling systems...~%")
        (compile-systems system-names verbosity)

        (asdf/session:with-asdf-session (:override t)
          ;; Load every discovered primary system. Using the same discovery as
          ;; the test phase keeps new .asd files covered by CI.
          (format t "~&Loading systems...~%")
          (load-systems system-names verbosity)
          ;; Run all subsystems' tests.
          (format t "~&Testing systems...~%")
          (let* ((test-errors-file (get-test-errors-filepath))
                 (failures
                   (if (test-output-p verbosity)
                       (run-system-tests system-names verbosity nil)
                       (with-open-file (stream test-errors-file
                                               :direction :output
                                               :if-exists :supersede
                                               :if-does-not-exist :create)
                         (run-system-tests system-names verbosity stream)))))
            (when failures
              (unless (test-output-p verbosity)
                (report-test-errors-log test-errors-file))
              (error "~d ASDF system~:p failed project checks."
                     (length failures))))
          (format t "~&ALL PROJECT CHECKS PASSED~%"))
        0) ;; success exit code
    (error (condition)
      (format *error-output* "~&PROJECT CHECKS FAILED: ~a~%" condition)
      1))) ;; failure exit code

(defun run-tests-and-quit ()
  "Run all tests for the project and quit with exit status."
  (let ((verbosity (check-verbosity-level)))
    (uiop:quit
     (run-function
      (lambda () (run-tests-under (get-source-dirpath) :verbosity verbosity))
      :verbosity verbosity))))

(defun main ()
  (run-tests-and-quit))

#+run-main
(apply #'main roswell:*argv*)
