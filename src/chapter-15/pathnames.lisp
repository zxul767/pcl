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
                         recursively
                         include-directories
                         (condition (constantly t)))
  "Traverse directory `dirname', invoking `callback' on each file.
On each call to `callback' a canonical pathname is passed back.
By default, all files (and possibly directories) are considered but `condition'
can be used to limit which files are actually passed back via `callback'.
By default, only top-level files are visited but `recursively' can change that.
By default, only files are visited but `include-directories' can change that.

Example: traverse the `src' directory recursively, printing each file and
directory underneath it, excluding hidden files.

(walk-directory \"~/src\" #'print
                :recursively t
                :include-directories t
                :condition #'is-not-hidden-file)

Notice that if `is-not-hidden-file' returns `nil' for directories (e.g., `.git'),
then files underneath them won't be visited, even if they would pass `condition'"
  (labels
      ((visit (pathname)
         (if (funcall condition pathname)
             (funcall callback pathname)))
       (walk (dirpath)
         (if include-directories
             (visit dirpath))
         (dolist (pathname (list-directory dirpath))
           (if (and recursively (directory-p pathname) (funcall condition pathname))
               (walk pathname)
               (visit pathname)))))
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
