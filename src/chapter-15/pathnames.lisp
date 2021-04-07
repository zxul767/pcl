(in-package :dev.zxul767.pathnames)

;;
;; Public API
;;
(defun as-directory-pathname (name)
  "Converts `name', if necessary, to a canonical directory pathname."
  (let ((pathname (pathname name)))
    (if (wild-pathname-p pathname)
        (error "Cannot reliably convert wild pathnames"))
    (if (directory-pathname-p pathname)
        pathname
        (make-pathname
         :directory (as-directory-parts pathname)
         :name nil
         :type nil
         :defaults pathname))))

(defun as-file-pathname (name)
  "Converts `name', if necessary, to a canonical file pathname."
  (let ((pathname (pathname name)))
    (if (wild-pathname-p pathname)
        (error "Cannot reliably convert wild pathnames"))
    (if (not (directory-pathname-p name))
        pathname
        (let* ((directory (pathname-directory pathname))
               (name-and-type (pathname (first (last directory)))))
          (make-pathname
           :directory (butlast directory)
           :name (pathname-name name-and-type)
           :type (pathname-type name-and-type)
           :defaults pathname)))))

(defun list-directory (dirname)
  "Returns all files and directories under `dirname', as canonical pathnames."
  (if (wild-pathname-p dirname)
      (error "Can only list concrete directory names."))
  (let ((wildcard (directory-wildcard dirname)))
    #+ (or sbcl cmu lispworks)
    (directory wildcard)
    #+openmcl
    (directory wildcard :directories t)
    #+allegro
    (directory wildcard :directories-are-files nil)
    #+clisp
    (nconc
     (directory wildcard)
     (directory (clisp-subdirectories-wildcard wildcard)))
    #- (or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented")))

(defun file-exists-p (pathname)
  "Returns non-nil if `pathname' represents an existing file or directory."
  #+ (or sbcl lispworks openmcl)
  (probe-file pathname)
  #+ (or allegro cmu)
  (or (probe-file (as-directory-pathname pathname))
      (probe-file pathname))
  #+clisp
  (or (ignore-errors
       (probe-file (pathname-as-file pathname)))
      (ignore-errors
       (let ((directory-form (as-directory-pathname pathname)))
         (when (ext:probe-directory directory-form)
           directory-form))))
  #- (or sbcl cmu lispworks openmcl allegro clisp)
  (error "file-exists-p not implemented"))

(defun directory-pathname-p (pathname)
  "Returns non-nil if `pathname' represents a directory pathname."
  (labels
      ((component-present-p (value)
         (and value (not (eql value :unspecific)))))
    (and
     (not (component-present-p (pathname-name pathname)))
     (not (component-present-p (pathname-type pathname)))
     pathname)))

(defun file-pathname-p (pathname)
  "Return non-nil if `pathname' represents a file pathname"
  (unless (directory-pathname-p pathname) pathname))

(defun walk-directory (dirname callback
                       &key
                         (recursively nil)
                         (report-directories nil)
                         (file-condition (constantly t))
                         (directory-condition (constantly t)))
  "Traverse directory `dirname', invoking `callback' on each file.
On each call to `callback' a canonical pathname is passed back.

By default, all top-level files are considered but `file-condition' can be used
to limit which files are actually passed to `callback'.

By default, only top-level files and directories are reported but `recursively'
can change that; when this option is used, `directory-condition' can be used to
limit which directories are (also recursively) traversed.

By default, only files are passed back to `callback' but `report-directories'
can change that.

Example: traverse the `~/src' directory recursively, printing each Python file
and directory underneath it, excluding everything under the `node_modules'
directory.

(walk-directory \"~/src\" #'print
                :recursively t
                :report-directories t
                :file-condition #'is-python-file
                :directory-condition (fn-not is-node-modules-directory))"
  (labels
      ((report-unconditionally (pathname)
         (funcall callback pathname))
       (report (pathname)
         (if (funcall file-condition pathname)
             (report-unconditionally pathname)))
       (walk (dirpath)
         (when (funcall directory-condition dirpath)
           (if report-directories
               (report-unconditionally dirpath))
           (dolist (pathname (list-directory dirpath))
             ;; In this implementation, we can see why it is not a good idea to
             ;; conflate the listing of files and directories in a single function
             ;; as `list-directory' does; without such separation, this inner loop
             ;; is hopelessly inefficient, having to ask on each iteration whether
             ;; we're dealing with a file or a directory
             (if (file-p pathname)
                 (report pathname)
                 (if recursively ;; pathname is a directory
                     (walk pathname)))))))
    (walk (as-directory-pathname dirname))))

(defun file-p (name)
  "Is `name' the name of an existing file, i.e. not a directory?"
  (let ((truename (file-exists-p name)))
    (and truename (file-pathname-p name))))

(defun directory-p (name)
  "Is `name' the name of an existing directory?"
  (let ((truename (file-exists-p name)))
    (and truename (directory-pathname-p name))))

;;
;; Auxiliary Functions
;;
(defun directory-wildcard (dirname)
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (as-directory-pathname dirname)))

#+clisp
(defun clisp-subdirectories-wildcard (wildcard)
  (make-pathname
   :directory (append (pathname-directory wildcard) (list :wild))
   :name nil
   :type nil
   :defaults wildcard))

(defun as-directory-parts (pathname)
  (append (or (pathname-directory pathname) (list :relative))
          (list (file-namestring pathname))))
