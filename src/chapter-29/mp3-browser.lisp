(in-package :dev.zxul767.mp3-browser)

(defvar *styles-filepath*
  (asdf:system-relative-pathname 'mp3-browser "mp3-browser.css"))

(defvar *major-version* 1)
(defvar *minor-version* 0)

(defparameter *mp3s-rootpath* nil)
(defparameter *random-items-per-page* 25)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup & Startup
(defun configure-mp3-browser (&optional force)
  (unless (or *styles-filepath* force)
    (format t "Enter full filename of mp3-browser.css: ")
    (force-output *standard-output*)
    (setf *styles-filepath* (read)))
  (unless (or *mp3s-rootpath* force)
    (format t "Enter root directory of MP3 collection: ")
    (force-output *standard-output*)
    (setf *mp3s-rootpath* (read))))

(defun start-mp3-browser ()
  (unless (and *mp3s-rootpath* *styles-filepath*)
    (configure-mp3-browser))
  (setf *songs-source-type* 'playlist)
  (load-database *mp3s-rootpath* *mp3-table*)
  (publish-file :path "/mp3-browser.css" :file *styles-filepath* :content-type "text/css")
  (net.aserve::debug-on :notrap)
  (net.aserve:start :port 2020))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Application UIs

;; This is the entry point to browse songs by various criteria
(define-html-handler browse
    (request (what keyword :genre) genre artist album (random integer))
  (let* ((values (values-for-page what genre artist album random))
         (title (browse-page-title what genre artist album random))
         (single-column (if (eql what :song) :file what))
         (values-string (values->base64 single-column values)))
    (html
      (:mp3-browser-page
       (:title title)
       ((:form :method "POST" :action "playlist")
        (:input :name "values" :type "hidden" :value values-string)
        (:input :name "what" :type "hidden" :value single-column)
        (:input :name "action" :type "hidden" :value :add-songs)
        (:input :name "submit" :type "submit" :value "Add All"))
       (:ul (do-rows (row values) (list-item-for-page what row)))))))

(defun browse-page-title (what genre artist album random-count)
  (with-output-to-string (s)
    (when random-count
      ;; ~:((...)~) overlays capitalization to the formatting done by (...)
      (format s "~:(~r~) Random " random-count))
    (format s "~:(~a~p~)" what random-count)
    (when (or genre artist album)
      (when (not (eql what :song)) (princ " with songs" s))
      (when genre (format s " in genre ~a" genre))
      (when artist (format s " by artist ~a" artist))
      (when album (format s " on album ~a" album)))))

(defun list-item-for-page (what row)
  (if (eql what :song)
      (with-column-values (song file album artist genre) row
        (html
          (:li
           (:a :href (link "playlist" :file file :action "add-songs") (:b song))
           " from "
           (:a :href (link "browse" :what :song :album album) album)
           " by "
           (:a :href (link "browse" :what :song :artist artist) artist)
           " in genre "
           (:a :href (link "browse" :what :song :genre genre) genre))))
      (let ((value (column-value row what)))
        (html
          (:li value " - "
               (browse-link :genre what value)
               (browse-link :artist what value)
               (browse-link :album what value)
               (browse-link :song what value))))))

(defun values-for-page (what genre artist album random-count)
  (let* ((query (select
                 :from *mp3-table*
                 :columns (if (eql what :song) t what)
                 :where (matching *mp3-table* :genre genre :artist artist :album album)
                 :distinct (not (eql what :song))
                 :order-by (if (eql what :song) '(:album :track) what)))
         (values query))
    (if random-count
        (random-selection values random-count) values)))

(defun browse-link (new-what what value)
  (unless (eql new-what what)
    (html
      "["
      (:a :href (link "browse" :what new-what what value)
          (:format "~(~as~)" new-what))
      "] ")))

(defun link (target &rest attributes)
  (html
    (:attribute
     ;; the following is a cryptic way to write URLs like this:
     ;; target?attr1=value1&attr2=value2...
     ;; ~@[(...)~] consumes the next argument, and if non-nil, formats it according
     ;; to (...)
     ;; ~{(...)~} consumes a list argument and formats each element according to (...)
     ;; ~((...)~) consumes the next argument and formats it as lowercase
     ;; ~^ tells `format' to ignore the rest of the formatting spec if processing the
     ;; last element in a list
     (:format "~a~@[?~{~(~a~)=~a~^&~}~]" target (mapcar #'urlencode attributes)))))

(defun values->base64 (column-name values-table)
  (flet ((value (row) (column-value row column-name)))
    (obj->base64 (map-rows #'value values-table))))

;; TODO: move to (nonexisting) `data-conversion' package
(defun obj->base64 (obj)
  (base64-encode (with-safe-io-syntax (write-to-string obj))))

(defun urlencode (string)
  (net.aserve::encode-form-urlencoded string))
