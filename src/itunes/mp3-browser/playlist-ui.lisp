(in-package :dev.zxul767.mp3-browser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Application UIs
(define-html-handler playlist
    ;; FIXME: this function has grown unfocused , needs refactoring!
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
