(in-package :dev.zxul767.pathnames)

;;
;; public API
;;
(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
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
  #+ (or sbcl lispworks openmcl)
  (probe-file pathname)
  #+ (or allegro cmu)
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))
  #+clisp
  (or (ignore-errors
       (probe-file (pathname-as-file pathname)))
      (ignore-errors
       (let ((directory-form (pathname-as-directory pathname)))
         (when (ext:probe-directory directory-form)
           directory-form))))
  #- (or sbcl cmu lispworks openmcl allegro clisp)
  (error "file-exists-p not implemented"))

(defun directory-pathname-p (p)
  (labels
      ((component-present-p (value)
         (and value (not (eql value :unspecific)))))
    (and
     (not (component-present-p (pathname-name p)))
     (not (component-present-p (pathname-type p)))
     p)))

(defun file-pathname-p (p)
  (and (not (directory-pathname-p p))
       p))

(defun pathname-as-directory (name)
  (labels
      ((to-directory (pathname)
         (append (or (pathname-directory pathname) (list :relative))
                 (list (file-namestring pathname)))))
    (let ((pathname (pathname name)))
      (when (wild-pathname-p pathname)
        (error "Cannot reliably convert wild pathnames."))
      (if (directory-pathname-p pathname)
          pathname
          (make-pathname
           :directory (to-directory pathname)
           :name nil
           :type nil
           :defaults pathname)))))

(defun pathname-as-file (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
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

;; TODO: analyze if the current implementation is flexible and sensible enough
(defun walk-directory (dirname callback
                       &key recursively directories (condition (constantly t)))
  (labels
      ((visit (name)
         (when (funcall condition name)
           (funcall callback name)))
       (walk (dirname)
         (when directories
           (visit dirname))
         (dolist (child (list-directory dirname))
           (if (and recursively (directory-p child) (funcall condition child))
               (walk child)
               (visit child)))))
    (walk (pathname-as-directory dirname))))

(defun file-p (path)
  (and (file-pathname-p path) (file-exists-p path)))

(defun directory-p (path)
  (not (file-p path)))

;;
;; implementation details
;;
(defun directory-wildcard (dirname)
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))

#+clisp
(defun clisp-subdirectories-wildcard (wildcard)
  (make-pathname
   :directory (append (pathname-directory wildcard) (list :wild))
   :name nil
   :type nil
   :defaults wildcard))

(defun is-hidden-p (path)
  (let ((name (or (pathname-name path)
                  (first (last (pathname-directory path))))))
    (and name (eql #\. (aref name 0)))))
