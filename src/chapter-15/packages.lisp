(in-package :cl-user)

(defpackage :dev.zxul767.pathnames
  (:use :common-lisp)
  (:export
   :as-directory-pathname
   :as-file-pathname
   :list-directory
   :file-exists-p
   :directory-pathname-p
   :file-pathname-p
   :walk-directory
   :directory-p
   :file-p))
