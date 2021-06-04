(in-package :dev.zxul767.mp3-browser)

(defparameter *mp3-dir* nil)

(defparameter *mp3-css* (asdf:system-relative-pathname 'mp3-browser "mp3-browser.css"))
(defparameter *silence-mp3* (asdf:system-relative-pathname 'mp3-browser "silentpacket.mp3"))

(defclass playlist ()
  ((id :accessor id :initarg :id)
   (songs :accessor songs :initform (make-playlist-table))
   (current-song :accessor current-song :initform *empty-playlist-song*)
   (current-song-index :accessor current-song-index :initform 0)
   (ordering :accessor ordering :initform :album) ;; values: `:album', `:genre', `:artist', `:song'
   (shuffle :accessor shuffle :initform :none) ;; values: `:none', `song', `:album'
   (repeat :accessor repeat :initform :none) ;; values: `:none', `:song', `:all'
   (user-agent :accessor user-agent :initform "Unknown")
   (lock :reader lock :initform (make-process-lock))))

(defun make-playlist-table ()
  (make-instance 'table :schema *mp3-schema*))

(defmacro with-playlist-locked ((playlist) &body body)
  `(with-process-lock ((lock ,playlist))
     ,@body))

(defvar *playlists* (make-hash-table :test #'equal))

(defparameter *playlists-lock* (make-process-lock :name "playlists-lock"))

(defun get-or-create-playlist (id)
  (with-process-lock (*playlists-lock*)
    (or (gethash id *playlists*)
        (setf (gethash id *playlists*)
              (make-instance 'playlist :id id)))))

(defmethod find-songs-source ((type (eql 'playlist)) request)
  (let ((playlist (get-or-create-playlist (playlist-id request))))
    (with-playlist-locked (playlist)
      (when-bind ((user-agent (header-slot-value request :user-agent)))
        (setf (user-agent playlist) user-agent)))
    playlist))

(defun playlist-id (request)
  (ipaddr-to-dotted (remote-host (request-socket request))))

(defmethod current-song :around ((playlist playlist))
  (with-playlist-locked (playlist)
    (call-next-method)))

(defmethod still-current-p (song (playlist playlist))
  (with-playlist-locked (playlist)
    (eql song (current-song playlist))))

(defun update-current-song (playlist)
  (unless (equal (file (current-song playlist))
                 (file-for-current-song-index playlist))
    (reset-current-song playlist)))

(defun file-for-current-song-index (playlist)
  (unless (at-end-p playlist)
    (column-value (nth-row (current-song-index playlist)
                           (songs playlist))
                  :file)))

(defun at-end-p (playlist)
  (>= (current-song-index playlist)
      (table-size (songs playlist))))


(defun make-silent-song (title &optional (file *silence-mp3*))
  (if file
      (make-instance
       'song
       :file file
       :title title
       :id3-size (if (id3-p file) (size (read-id3 file)) 0))))

(defparameter *empty-playlist-song* (make-silent-song "Playlist empty."))
(defparameter *end-of-playlist-song* (make-silent-song "At end of playlist."))

(defun reset-current-song (playlist)
  (setf
   (current-song playlist)
   (cond
     ((empty-p playlist) *empty-playlist-song*)
     ((at-end-p playlist) *end-of-playlist-song*)
     (t (row->song (nth-row (current-song-index playlist)
                            (songs playlist)))))))

(defun row->song (songs-table-entry)
  (with-column-values (file song artist album id3-size) songs-table-entry
    (make-instance
     'song
     :file file
     :title (format nil "~a by ~a from ~a" song artist album)
     :id3-size id3-size)))

(defun empty-p (playlist)
  (zerop (table-size (songs playlist))))

(defmethod maybe-move-to-next-song (song (playlist playlist))
  (with-playlist-locked (playlist)
    (when (still-current-p song playlist)
      (unless (at-end-p playlist)
        (ecase (repeat playlist)
          (:song) ; nothing changes
          (:none (incf (current-song-index playlist)))
          (:all (setf (current-song-index playlist)
                      (mod (1+ (current-song-index playlist))
                           (table-size (songs playlist)))))))
      (update-current-song playlist))))

(defun add-songs (playlist column-name values)
  ;; FIXME: The need to build `column-values' is an undesirable consequence of how
  ;; `in' works A better API should accept any sequence of values (and implicitly
  ;; convert to a table if deemed necessary by the implementation)
  (let ((column-values (build-single-column-table column-name values)))
    (do-rows (row (select :from *mp3-table* :where (in column-name column-values)))
      (insert-row row (songs playlist))))
  (update-current-song playlist))

(defun build-single-column-table (column-name values)
  (with-result (table (make-instance
                       'table
                       :schema (extract-schema (list column-name) *mp3-schema*)))
    (dolist (value values)
      (insert-row (list column-name value) table))))

(defun delete-songs (playlist &rest names-and-values)
  (delete-rows
   :from (songs playlist)
   :where (apply
           #'matching
           (songs playlist)
           names-and-values))
  (setf (current-song-index playlist) (or (position-of-current-song playlist) 0))
  (update-current-song playlist))

(defun position-of-current-song (playlist)
  (let* ((songs (songs playlist))
         (matcher (matching songs :file (file (current-song playlist))))
         (position 0))
    (do-rows (song songs)
      (when (funcall matcher song)
        ;; Cannot simply use `(return position)` because `do-rows' defines an implicit
        ;; `nil' block, so it would just break out of the loop but not out of the function
        (return-from position-of-current-song position))
      (incf position))))

(defun clear-playlist (playlist)
  (delete-all-rows (songs playlist))
  (setf (current-song-index playlist) 0)
  (update-current-song playlist))

(defun sort-playlist (playlist ordering)
  (setf (ordering playlist) ordering)
  (setf (shuffle playlist) :none)
  (order-playlist playlist)
  (setf (current-song-index playlist) (position-of-current-song playlist)))

(defun order-playlist (playlist)
  (apply #'sort-rows (songs playlist)
         (case (ordering playlist)
           (:genre '(:genre :album :track))
           (:artist '(:artist :album :track))
           (:album '(:album :track))
           (:song '(:song)))))

(defun shuffle-playlist (playlist shuffle)
  (setf (shuffle playlist) shuffle)
  (case shuffle
    (:none (order-playlist playlist))
    (:song (shuffle-by-song playlist))
    (:album (shuffle-by-album playlist)))
  (setf (current-song-index playlist) (position-of-current-song playlist)))

(defun shuffle-by-album (playlist)
  (let ((new-table (make-playlist-table)))
    (do-rows (album-row (shuffled-album-names playlist))
      (do-rows (song (songs-for-album playlist (column-value album-row :album)))
        (insert-row song new-table)))
    (setf (songs playlist) new-table)))

(defun shuffle-by-song (playlist)
  (shuffle-table (songs playlist)))

(defun shuffled-album-names (playlist)
  (shuffle-table
   (select :columns :album :from (songs playlist) :distinct t)))

(defun songs-for-album (playlist album)
  (let ((songs (songs playlist)))
    (select :from songs :where (matching songs :album album) :order-by :track)))

(defmethod (setf repeat) :after (value (playlist playlist))
  (if (and (at-end-p playlist) (not (empty-p playlist)))
      (ecase value
        (:song (setf (current-song-index playlist)
                     (1- (table-size (songs playlist)))))
        (:none)
        (:all (setf (current-song-index playlist) 0)))
      (update-current-song playlist)))

(define-html-macro :mp3-browser-page ((&key title (header title)) &body body)
  `(:html
     (:head
      (:title ,title)
      (:link :rel "stylesheet" :type "text/css" :href "mp3-browser.css"))
     (:body
      (standard-header)
      (when ,header (html (:h1 :class "title" ,header)))
      ,@body
      (standard-footer))))

(defparameter *random-items-per-page* 25)

(defparameter *major-version* 1)
(defparameter *minor-version* 0)

(defun standard-header ()
  (html
    ((:p :class "toolbar")
     "[" (:a :href (link "/browse" :what "genre") "All Genres") "] ")))

(defun standard-footer ()
  (html
    (:hr)
    ((:p :class "footer") "MP3 Browser v" *major-version* "." *minor-version*)))

(define-html-macro :table-row (&attributes attributes &rest values)
  `(:tr ,@attributes ,@(loop for value in values collect `(:td ,value))))

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

(defun urlencode (string)
  (net.aserve::encode-form-urlencoded string))

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

(define-html-handler playlist
    (request
     (playlist-id string (playlist-id request) :package)
     (action keyword)                  ; playlist manipulation action
     (what keyword :file)              ; for :add-songs actin
     (values :base64)                  ; for :add-songs action
     file                              ; for :add-songs and :delete-songs action
     genre                             ; for :delete-songs action
     artist                            ; for :delete-songs action
     album                             ; for :delete-songs action
     (order-by keyword)                ; for :sort action
     (shuffle keyword)                 ; for :shuffle action
     (repeat keyword))                 ; for :set-repeat action
  (let ((playlist (get-or-create-playlist playlist-id)))
    (with-playlist-locked (playlist)
      (case action
        (:add-songs (add-songs playlist what (or values (list file))))
        (:delete-songs (delete-songs
                        playlist
                        :file file
                        :genre genre
                        :artist artist
                        :album album))
        (:clear (clear-playlist playlist))
        (:sort (sort-playlist playlist order-by))
        (:shuffle (shuffle-playlist playlist shuffle))
        (:set-repeat (setf (repeat playlist) repeat))))
    (html
      (:mp3-browser-page
       (:title (:format "Playlist - ~a" (id playlist)) :header nil)
       (playlist-toolbar playlist)
       (if (empty-p playlist)
           (html (:p (:i "Empty.")))
           (html
             ((:table :class "playlist")
              (:table-row "Song" "Album" "Artist" "Genre")
              (let ((index 0)
                    (current-index (current-song-index playlist)))
                (do-rows (row (songs playlist))
                  (with-column-values (file song album artist genre) row
                    (let ((row-style (if (= index current-index) "now-playing" "normal")))
                      (html
                        ((:table-row :class row-style)
                         (:progn song (delete-songs-link :file file))
                         (:progn album (delete-songs-link :album album))
                         (:progn artist (delete-songs-link :artist artist))
                         (:progn genre (delete-songs-link :genre genre)))))
                    (incf index)))))))))))

(defun playlist-toolbar (playlist)
  (let ((current-repeat (repeat playlist))
        (current-sort (ordering playlist))
        (current-shuffle (shuffle playlist)))
    (html
      (:p :class "playlist-toolbar"
          (:i "Sort by:")
          " [ "
          (sort-playlist-button "genre" current-sort) " | "
          (sort-playlist-button "artist" current-sort) " | "
          (sort-playlist-button "album" current-sort) " | "
          (sort-playlist-button "song" current-sort) " ] "
          (:i "Shuffle by:")
          " [ "
          (playlist-shuffle-button "none" current-shuffle) " | "
          (playlist-shuffle-button "song" current-shuffle) " | "
          (playlist-shuffle-button "album" current-shuffle) " ] "
          (:i "Repeat:")
          " [ "
          (playlist-repeat-button "none" current-repeat) " | "
          (playlist-repeat-button "song" current-repeat) " | "
          (playlist-repeat-button "all" current-repeat) " ] -- "
          "[ " (:a :href (link "playlist" :action "clear") "Clear Playlist") " ] "))))

(defun playlist-button (action argument new-value current-value)
  (let ((label (string-capitalize new-value)))
    (if (string-equal new-value current-value)
        (html (:b label))
        (html (:a :href (link "playlist" :action action argument new-value) label)))))

(defun sort-playlist-button (order-by current-sort)
  (playlist-button :sort :order-by order-by current-sort))

(defun playlist-shuffle-button (shuffle current-shuffle)
  (playlist-button :shuffle :shuffle shuffle current-shuffle))

(defun playlist-repeat-button (repeat current-repeat)
  (playlist-button :set-repeat :repeat repeat current-repeat))

(defun delete-songs-link (what value)
  (html " [" (:a :href (link "playlist" :action :delete-songs what value) "x") "]"))

(define-html-handler all-playlists (request)
  (:mp3-browser-page
   (:title "All playlists")
   ((:table :class "all-playlists")
    (:table-row "Playlist" "# Songs" "Most Recent User-Agent")
    (with-process-lock (*playlists-lock*)
      (loop for playlist being the hash-values of *playlists* do
        (html
          (:table-row
           (:a :href (link "playlist" :playlist-id (id playlist))
               (:print (id playlist)))
           (:print (table-size (songs playlist)))
           (:print (user-agent playlist)))))))))

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

(defun values->base64 (column-name values-table)
  (flet ((value (row) (column-value row column-name)))
    (obj->base64 (map-rows #'value values-table))))

;; TODO: move to (nonexisting) `data-conversion' package
(defun obj->base64 (obj)
  (base64-encode (with-safe-io-syntax (write-to-string obj))))

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

(defun browse-link (new-what what value)
  (unless (eql new-what what)
    (html
      "["
      (:a :href (link "browse" :what new-what what value)
          (:format "~(~as~)" new-what))
      "] ")))

(defun configure-mp3-browser (&optional force)
  (unless (or *mp3-css* force)
    (format t "Enter full filename of mp3-browser.css: ")
    (force-output *standard-output*)
    (setf *mp3-css* (read)))
  (unless (or *mp3-dir* force)
    (format t "Enter root directory of MP3 collection: ")
    (force-output *standard-output*)
    (setf *mp3-dir* (read))))

(defun start-mp3-browser ()
  (unless (and *mp3-dir* *mp3-css*)
    (configure-mp3-browser))
  (setf *songs-source-type* 'playlist)
  (load-database *mp3-dir* *mp3-table*)
  (publish-file :path "/mp3-browser.css" :file *mp3-css* :content-type "text/css")
  (net.aserve::debug-on :notrap)
  (net.aserve:start :port 2020))
