(in-package :dev.zxul767.shoutcast)

(defgeneric current-song (songs-source)
  (:documentation "Return the currently playing song (or NIL) in `songs-source'"))

(defgeneric maybe-move-to-next-song (song songs-source)
  (:documentation "If `song' is still the current one in `songs-source', update the
value returned by `current-song'"))

(defgeneric still-current-p (song songs-source)
  (:documentation "Return non-nil if `song' is the current song in `songs-source'"))

(defclass song ()
  ((file :reader file :initarg :file)
   (title :reader title :initarg :title)
   (id3-size :reader id3-size :initarg :id3-size)))

(defgeneric find-songs-source (type request)
  (:documentation "Find the songs-source matching `type' for the given web request"))

(defclass simple-song-queue ()
  ((songs :accessor songs :initform (make-array 10 :adjustable t :fill-pointer 0))
   (current-song-index :accessor current-song-index :initform 0)))

(defparameter *songs-queue* (make-instance 'simple-song-queue))

(defmethod find-songs-source ((type (eql 'singleton)) request)
  (declare (ignore request))
  *songs-queue*)

(defmethod current-song ((songs-source simple-song-queue))
  (let ((index (current-song-index songs-source))
        (songs (songs songs-source)))
    (when (array-in-bounds-p songs index)
      (aref songs index))))

(defmethod still-current-p (song (songs-source simple-song-queue))
  (eql song (current-song songs-source)))

(defun next-song ()
  (maybe-move-to-next-song (current-song *songs-queue*)
                           *songs-queue*))

(defmethod maybe-move-to-next-song (song (songs-source simple-song-queue))
  (when (still-current-p song songs-source)
    (logger "moving to next song...")
    (let ((index (current-song-index songs-source)))
      (setf (current-song-index songs-source)
            (mod (1+ index) (length (songs songs-source)))))))

;; FIXME: the current code cannot handle the absence of id3 tags in a file
;; we should definitely handle that case because metadata about an audio file
;; is supposed to be optional
(defun add-file-to-songs (filepath)
  (let ((song (file->song filepath)))
    (if song
        (vector-push-extend song (songs *songs-queue*))
        (logger "Cannot add ~a (unsupported file or missing metadata)" filepath))))

(defun song-information (id3-tag)
  (if id3-tag
      (format nil "[~a] by [~a] from [~a]"
              (song id3-tag) (artist id3-tag) (album id3-tag))
      (format nil "Unknown song information")))

(defun file->song (filepath)
  (when-bind ((id3-tag (read-id3 filepath)))
    (make-instance
     'song
     :file (namestring (truename filepath))
     :title (song-information id3-tag)
     :id3-size (size id3-tag))))

;; There is no way to tell AllegroServe to never time out the request, so we set it to
;; a very large number (10 years)
(defparameter *shoutcast-request-timeout-in-seconds* (* 60 60 24 7 52 10))

;; The number of bytes of mp3 data to send inbetween each metadata packet transmission
(defparameter *shoutcast-metadata-interval-in-bytes* (expt 2 15))

(defparameter *songs-source-type* 'singleton)

(defun shoutcast (request entity)
  (with-http-response
      (request entity :content-type "audio/MP3"
                      :timeout *shoutcast-request-timeout-in-seconds*)
    (prepare-icy-response request *shoutcast-metadata-interval-in-bytes*)
    (let ((wants-metadata-p (header-slot-value request :icy-metadata)))
      (logger "client wants metadata? ~a" wants-metadata-p)
      (with-http-body (request entity)
        (stream-songs
         (request-socket request)
         (find-songs-source *songs-source-type* request)
         (if wants-metadata-p *shoutcast-metadata-interval-in-bytes*))))))

(defun prepare-icy-response (request metadata-interval)
  (logger "preparing icy response headers...")
  (setf (request-reply-protocol-string request) "ICY")
  (loop for (key value) in (reverse
                            `((:|icy-metaint| ,(princ-to-string metadata-interval))
                              (:|icy-notice1| "...")
                              (:|icy-notice2| "...")
                              (:|icy-name| "Lisp Shoutcast Server")
                              (:|icy-genre| "Unknown")
                              (:|icy-url| ,(request-uri request))
                              (:|icy-pub| "1")))
        do (setf (reply-header-slot-value request key) value)))

;; iTunes, despite claiming to speak HTTP/1.1, doesn't understand chunked-transfer
;; encoding, so we just turn it off.
;; (turn-off-chunked-transfer-encoding request))

(defun turn-off-chunked-transfer-encoding (request)
  (setf (request-reply-strategy request)
        (remove :chunked (request-reply-strategy request))))

(defun logger (message &rest args)
  (format *trace-output* "~&")
  (apply #'format *trace-output* message args)
  (format *trace-output* "~%"))

(defun stream-songs (stream songs-source metadata-interval)
  (logger "starting songs stream with metadata interval ~a..." metadata-interval)
  (handler-case
      (loop
        for next-metadata-interval = metadata-interval
          then (stream-current-song
                stream
                songs-source
                next-metadata-interval
                metadata-interval)
        while next-metadata-interval)
    (error (e)
      (logger "Caught error in stream-songs: ~a" e))))

;; FIXME: should we merge `stream-current-song' and `stream-mp3-file'?
(defun stream-current-song
    (stream songs-source next-metadata-interval metadata-interval)
  (when-bind ((song (current-song songs-source)))
    (logger "streaming ~a -- ~a" (title song) (file song))
    (let ((metadata (make-icy-metadata (title song) (file song))))
      (setf next-metadata-interval
            (stream-mp3-file
             stream
             song
             songs-source
             metadata next-metadata-interval metadata-interval))
      (logger "next metadata interval: ~a" next-metadata-interval)
      (maybe-move-to-next-song song songs-source))
    next-metadata-interval))

;; The Shoutcast protocol was designed to stream radio on the internet, so there
;; is no provision to interrupt a song in the middle and switch to another one.
;; The poor man's way of doing this is simply by constantly checking if the
;; current song (as determined by `still-current-p') hasn't been changed in another
;; thread and interrupting the current stream if so.
;;
;; This works more or less okay as long as the client hasn't pulled in too much data
;; (e.g., by having a large network cache), but it's very hard to achieve a good,
;; reactive, and smooth switching because most clients use a relatively large network
;; cache, so the switch only happens after the cached data has been played back.
(defun stream-mp3-file (stream song songs-source
                        metadata next-metadata-interval metadata-interval)
  (with-open-file (mp3 (file song) :element-type '(unsigned-byte 8))
    (unless (file-position mp3 (id3-size song))
      (error "Can't skip to position ~d in ~a" (id3-size song) (file song)))
    (let ((bytes-read 0))
      (loop for byte = (read-byte mp3
                                  nil  ;; eof-error-p
                                  nil) ;; eof-value
            while (and byte (still-current-p song songs-source)) do
              (incf bytes-read)
              ;; NOTE: We try to limit the streaming rate to the client, but this
              ;; probably wouldn't work in production, as high, realistic load on
              ;; the server would probably make this empirical setting inadequate.
              (when (zerop (mod bytes-read 50000))
                (sleep 1.5))
              (write-byte byte stream)
              (decf next-metadata-interval)
            when (and (zerop next-metadata-interval) metadata-interval) do
              (write-sequence metadata stream)
              (setf next-metadata-interval metadata-interval))
      (logger "finished streaming song: ~a bytes were sent" bytes-read))
    next-metadata-interval))

;; NOTE: The book doesn't explain why it is necessary to use 16-bit blocks, or why do
;; we need to reserve an additional element, but we assume that it is part of the
;; Shoutcast protocol requirements.
(defun make-icy-metadata (title file)
  (declare (ignore title))
  (let* ((text (format nil "StreamTitle='~a';"
                       (substitute #\Space #\' (pathname-name file))))
         (block-size-in-bits 16)
         (blocks-count (ceiling (length text) block-size-in-bits))
         (buffer (make-array (1+ (* blocks-count block-size-in-bits))
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)))
    (setf (aref buffer 0) blocks-count)
    (loop
      for char across text
      for i from 1
      do (setf (aref buffer i) (char-code char)))
    buffer))

(publish :path "/stream.mp3" :function 'shoutcast)
