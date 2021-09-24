(in-package :dev.zxul767.mp3-browser)

(defvar *playlists* (make-hash-table :test #'equal))

(defvar *playlists-lock*
  (make-process-lock :name "playlists-lock"))

(defclass playlist ()
  ((id :accessor id :initarg :id)
   (songs :accessor songs :initform (create-mp3-table))
   (current-song :accessor current-song :initform *empty-playlist-song*)
   (current-song-index :accessor current-song-index :initform 0)
   (ordering :accessor ordering :initform :album) ;; values: `:album', `:genre', `:artist', `:song'
   (shuffle :accessor shuffle :initform :none) ;; values: `:none', `song', `:album'
   (repeat :accessor repeat :initform :none) ;; values: `:none', `:song', `:all'
   (user-agent :accessor user-agent :initform "Unknown")
   (lock :reader lock :initform (make-process-lock))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Macros
(defmacro with-playlist-locked ((playlist) &body body)
  `(with-process-lock ((lock ,playlist))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Shoutcast Protocol Implementation for Playlists

;; TODO: should we rename `find-songs-source` to `get-playlist`?
(defmethod find-songs-source ((type (eql 'playlist)) request)
  (with-result (playlist (get-or-create-playlist (playlist-id request)))
    (with-playlist-locked (playlist)
      (when-bind ((user-agent (header-slot-value request :user-agent)))
        (setf (user-agent playlist) user-agent)))))

(defmethod current-song :around ((playlist playlist))
  (with-playlist-locked (playlist)
    (call-next-method)))

(defmethod still-current-p (song (playlist playlist))
  (with-playlist-locked (playlist)
    (eql song (current-song playlist))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Accessors
(defun playlist-id (request)
  (ipaddr-to-dotted (remote-host (request-socket request))))

(defun get-or-create-playlist (id)
  (with-process-lock (*playlists-lock*)
    (or (gethash id *playlists*)
        (setf (gethash id *playlists*)
              (make-instance 'playlist :id id)))))

(defun file-for-current-song-index (playlist)
  (unless (at-end-p playlist)
    (column-value (nth-row (current-song-index playlist)
                           (songs playlist))
                  :file)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Predicates
(defun at-end-p (playlist)
  (>= (current-song-index playlist)
      (table-size (songs playlist))))

(defun empty-p (playlist)
  (zerop (table-size (songs playlist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Mutators
(defun add-songs (playlist column-name values)
 (with-labels
      (let ((column-table (build-column-table column-name values)))
        (do-rows (row (select :from *mp3-table* :where (in column-name column-table)))
          (insert-row row (songs playlist)))
        (update-current-song playlist))

    (build-column-table (column-name values)
      (with-result
          (table (make-instance 'table :schema (extract-schema `(,column-name) *mp3-schema*)))
        (dolist (value values)
          (insert-row `(,column-name ,value) table))))))

(defun delete-songs (playlist &rest names-and-values)
  (delete-rows
   :from (songs playlist)
   :where (apply
           #'matching
           (songs playlist)
           names-and-values))
  (setf (current-song-index playlist) (or (position-of-current-song playlist) 0))
  (update-current-song playlist))

(defun clear-playlist (playlist)
  (delete-all-rows (songs playlist))
  (setf (current-song-index playlist) 0)
  (update-current-song playlist))

(defun sort-playlist (playlist ordering)
  (with-labels
      (progn (setf (ordering playlist) ordering)
             (setf (shuffle playlist) :none)
             (order-playlist playlist)
             (setf (current-song-index playlist)
                   (position-of-current-song playlist)))
    (order-playlist (playlist)
      (apply #'sort-rows (songs playlist)
             (case (ordering playlist)
               (:genre '(:genre :album :track))
               (:artist '(:artist :album :track))
               (:album '(:album :track))
               (:song '(:song)))))))

(defun update-current-song (playlist)
  (unless (equal (file (current-song playlist))
                 (file-for-current-song-index playlist))
    (reset-current-song playlist)))

(defun reset-current-song (playlist)
  (with-labels
      (setf
          (current-song playlist)
          (cond
            ((empty-p playlist) *empty-playlist-song*)
            ((at-end-p playlist) *end-of-playlist-song*)
            (t (row->song (nth-row (current-song-index playlist)
                                   (songs playlist))))))
    (row->song (entry)
      (with-column-values (file song artist album id3-size) entry
        (make-instance
         'song
         :file file
         :title (format nil "~a by ~a from ~a" song artist album)
         :id3-size id3-size)))))

(defun shuffle-playlist (playlist shuffle)
  (setf (shuffle playlist) shuffle)
  (case shuffle
    (:none (order-playlist playlist))
    (:song (shuffle-by-song playlist))
    (:album (shuffle-by-album playlist)))
  (setf (current-song-index playlist) (position-of-current-song playlist)))

(defun shuffle-by-album (playlist)
  (with-labels
      (let ((shuffled-songs (create-mp3-table)))
        (do-rows (album-row (shuffled-album-names playlist))
          (do-rows (song (songs-for-album playlist (column-value album-row :album)))
            (insert-row song shuffled-songs)))
        (setf (songs playlist) shuffled-songs))
    (songs-for-album (playlist album)
      (let ((songs (songs playlist)))
        (select :from songs :where (matching songs :album album) :order-by :track)))))

(defun shuffle-by-song (playlist)
  (shuffle-table (songs playlist)))

(defun shuffled-album-names (playlist)
  (shuffle-table
   (select :columns :album :from (songs playlist) :distinct t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Invariants Preservation
(defmethod (setf repeat) :after (value (playlist playlist))
  (if (and (at-end-p playlist) (not (empty-p playlist)))
      (ecase value
        (:song (setf (current-song-index playlist)
                     (1- (table-size (songs playlist)))))
        (:none)
        (:all (setf (current-song-index playlist) 0)))
      (update-current-song playlist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Playlist Silence
(defvar *silence-mp3*
  (asdf:system-relative-pathname 'mp3-browser "silentpacket.mp3"))

(defun create-silent-song (title &optional (file *silence-mp3*))
  (if file
      (make-instance
       'song
       :file file
       :title title
       :id3-size (if (id3-p file) (size (read-id3 file)) 0))))

(defvar *empty-playlist-song*
  (create-silent-song "Playlist empty."))

(defvar *end-of-playlist-song*
  (create-silent-song "At end of playlist."))
