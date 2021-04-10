(in-package :dev.zxul767.id3v2)

;; two-byte encodings require knowing whether the first logical byte is written out first
(defvar *bom-big-endian* #xfeff)
(defvar *bom-little-endian* #xfffe)

(defparameter *silence-errors* nil)

(defparameter *id3-v1-genres*
  #(
    ;;These are the official ID3v1 genres.
    "Blues" "Classic Rock" "Country" "Dance" "Disco" "Funk" "Grunge"
    "Hip-Hop" "Jazz" "Metal" "New Age" "Oldies" "Other" "Pop" "R&B" "Rap"
    "Reggae" "Rock" "Techno" "Industrial" "Alternative" "Ska"
    "Death Metal" "Pranks" "Soundtrack" "Euro-Techno" "Ambient"
    "Trip-Hop" "Vocal" "Jazz+Funk" "Fusion" "Trance" "Classical"
    "Instrumental" "Acid" "House" "Game" "Sound Clip" "Gospel" "Noise"
    "AlternRock" "Bass" "Soul" "Punk" "Space" "Meditative"
    "Instrumental Pop" "Instrumental Rock" "Ethnic" "Gothic" "Darkwave"
    "Techno-Industrial" "Electronic" "Pop-Folk" "Eurodance" "Dream"
    "Southern Rock" "Comedy" "Cult" "Gangsta" "Top 40" "Christian Rap"
    "Pop/Funk" "Jungle" "Native American" "Cabaret" "New Wave"
    "Psychadelic" "Rave" "Showtunes" "Trailer" "Lo-Fi" "Tribal"
    "Acid Punk" "Acid Jazz" "Polka" "Retro" "Musical" "Rock & Roll"
    "Hard Rock"
    ;;These were made up by the authors of Winamp but backported into
    ;; the ID3 spec.
    "Folk" "Folk-Rock" "National Folk" "Swing" "Fast Fusion"
    "Bebob" "Latin" "Revival" "Celtic" "Bluegrass" "Avantgarde"
    "Gothic Rock" "Progressive Rock" "Psychedelic Rock" "Symphonic Rock"
    "Slow Rock" "Big Band" "Chorus" "Easy Listening" "Acoustic" "Humour"
    "Speech" "Chanson" "Opera" "Chamber Music" "Sonata" "Symphony"
    "Booty Bass" "Primus" "Porn Groove" "Satire" "Slow Jam" "Club"
    "Tango" "Samba" "Folklore" "Ballad" "Power Ballad" "Rhythmic Soul"
    "Freestyle" "Duet" "Punk Rock" "Drum Solo" "A capella" "Euro-House"
    "Dance Hall"
    ;;These were also invented by the Winamp folks but ignored by the
    ;; ID3 authors.
    "Goa" "Drum & Bass" "Club-House" "Hardcore" "Terror" "Indie"
    "BritPop" "Negerpunk" "Polsk Punk" "Beat" "Christian Gangsta Rap"
    "Heavy Metal" "Black Metal" "Crossover" "Contemporary Christian"
    "Christian Rock" "Merengue" "Salsa" "Thrash Metal" "Anime" "Jpop"
    "Synthpop"))

;; -----------------------------------------------------------------------------
;; General Utilities & Macros
;; -----------------------------------------------------------------------------
(defmacro fn-and (&rest functions)
  `#'(lambda (&rest args)
       (and ,@(loop for fn in functions collect `(apply #',fn args)))))

(defmacro if-let ((var condition) &body then)
  `(let ((,var ,condition))
     (when ,var ,@then)))

(defmacro with-result-as ((result &optional initform) &body body)
  `(let ((,result ,initform))
     ,@body
     ,result))

(defmacro sort! (sequence predicate &rest args)
  (assert (symbolp sequence))
  `(setf ,sequence (sort ,sequence ,predicate ,@args)))

(defmacro prog-nil (&body body)
  `(progn ,@body nil))

(defun human-readable-size (size-in-bytes)
  (with-output-to-string (stream)
    (format stream "~,3f MB" (bytes->megabytes size-in-bytes))))

(defun bytes->megabytes (bytes)
  (/ bytes (* 1024.0 1024.0)))

(defun megabytes->bytes (megabytes)
  (round (* megabytes 1024 1024)))

;; -----------------------------------------------------------------------------
;; Primitive Types
;; -----------------------------------------------------------------------------
(define-binary-type unsigned-integer (bytes bits-per-byte)
  (:reader (in)
           (loop with value = 0
                 for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte
                 do (setf (ldb (byte bits-per-byte low-bit) value) (read-byte in))
                 finally (return value)))
  (:writer (out value)
           (loop for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte
                 do (write-byte (ldb (byte bits-per-byte low-bit) value) out))))

(define-binary-type u1 () (unsigned-integer :bytes 1 :bits-per-byte 8))
(define-binary-type u2 () (unsigned-integer :bytes 2 :bits-per-byte 8))
(define-binary-type u3 () (unsigned-integer :bytes 3 :bits-per-byte 8))
(define-binary-type u4 () (unsigned-integer :bytes 4 :bits-per-byte 8))

(define-binary-type iso-8859-1-char ()
  (:reader (in)
           (let ((code (read-byte in)))
             (code-char code)))
  (:writer (out char)
           (let ((code (char-code char)))
             (if (<= 0 code #xff)
                 (write-byte code out)
                 (error "Illegal character for iso-8859-1 encoding. Character: ~c with code: ~d"
                        char code)))))

(define-binary-type ucs-2-char (swap)
  (:reader (in)
           (let ((code (read-value 'u2 in)))
             (if swap (setf code (swap-bytes code)))
             (code-char code)))
  (:writer (out char)
           (let ((code (char-code char)))
             (unless (<= 0 code #xffff)
               (error "Illegal character for ucs-2 encoding: ~c with char-code: ~d"
                      char code))
             (if swap (setf code (swap-bytes code)))
             (write-value 'u2 out code))))

(define-binary-type ucs-2-char-big-endian ()
  (ucs-2-char :swap nil))

(define-binary-type ucs-2-char-big-endian ()
  (ucs-2-char :swap t))

(define-binary-type generic-string (length character-type)
  (:reader (in)
           (with-result-as (string (make-string length))
             (dotimes (i length)
               (setf (char string i) (read-value character-type in)))))
  (:writer (out string)
           (dotimes (i length)
             (write-value character-type out (char string i)))))

(define-binary-type generic-terminated-string (terminator character-type)
  (:reader (in)
           (with-output-to-string (s)
             (loop for char = (read-value character-type in)
                   until (char= char terminator)
                   do (write-char char s))))
  (:writer (out string)
           (loop for char across string
                 do (write-value character-type out char)
                 finally (write-value character-type out terminator))))

(define-binary-type iso-8859-1-string (length)
  (generic-string :length length :character-type 'iso-8859-1-char))

(define-binary-type iso-8859-1-terminated-string (terminator)
  (generic-terminated-string :terminator terminator :character-type 'iso-8859-1-char))

(define-binary-type ucs-2-string (length)
  (:reader (in)
           (let ((byte-order-mark (read-value 'u2 in))
                 (characters (1- (/ length 2))))
             (read-value 'generic-string in
                         :length characters
                         :character-type (ucs-2-char-type byte-order-mark))))
  (:writer (out string)
           (declare (ignorable length))
           (assert (= length (length string)))

           (write-value 'u2 out *bom-big-endian*)
           (write-value 'generic-string out string
                        :length (length string)
                        :character-type (ucs-2-char-type *bom-big-endian*))))

(define-binary-type ucs-2-terminated-string (terminator)
  (:reader (in)
           (let ((byte-order-mark (read-value 'u2 in)))
             (read-value 'generic-terminated-string in
                         :terminator terminator
                         :character-type (ucs-2-char-type byte-order-mark))))
  (:writer (out string)
           (write-value 'u2 out *bom-big-endian*)
           (write-value 'generic-terminated-string out string
                        :terminator terminator
                        :character-type (ucs-2-char-type *bom-big-endian*))))

(define-binary-type raw-bytes (size max-size)
  (:reader (in)
           (if (> size max-size)
               (error 'data-too-large :size size :max-size max-size))
           (with-result-as (buffer (make-array size :element-type '(unsigned-byte 8)))
             (read-sequence buffer in)))
  (:writer (out buffer)
           (assert (= size (length buffer)))
           (assert (< size max-size))
           (write-sequence buffer out)))

(defun ucs-2-char-type (byte-order-mark)
  (ecase byte-order-mark
    (*bom-big-endian* 'ucs-2-char-big-endian)
    (*bom-little-endian* 'ucs-2-char-little-endian)))

(defun swap-bytes (code)
  (assert (<= code #xffff))
  (rotatef (ldb (byte 8 0) code)
           (ldb (byte 8 8) code))
  code)

;; -----------------------------------------------------------------------------
;; ID3 Generic
;; -----------------------------------------------------------------------------

;; At the end of the frames, there is likely to be some padding. We use a signal
;; and the condition system to deal with this.
(define-condition in-padding () ())

(define-condition missing-id3-tag (error) ())

(define-condition data-too-large (error)
  ((size :initarg :size :reader size)
   (max-size :initarg :max-size :reader max-size)))

(define-condition unsupported-version (error)
  ((major-version :initarg :major-version :reader major-version)))

(defgeneric frame-header-size (frame))

(define-binary-type id3-tag-size ()
  (unsigned-integer :bytes 4 :bits-per-byte 7))

(define-binary-type optional (type if)
  (:reader (in)
           (when if (read-value type in)))
  (:writer (out value)
           (when if (write-value type out value))))

(define-tagged-binary-class id3-tag ()
    ((:class-finder (case major-version
                      (2 'id3v2.2-tag)
                      (3 'id3v2.3-tag)
                      (t (error 'unsupported-version :major-version major-version)))))
  (identifier (tag-id :length 3))
  (major-version u1)
  (revision u1)
  (flags u1)
  (size id3-tag-size))

(define-binary-type tag-id (length)
  (:reader (in)
           (with-result-as (id (read-value 'iso-8859-1-string in :length length))
             (if (not (string= "ID3" id))
                 (error 'missing-id3-tag))))
  (:writer (out id)
           (write-value 'iso-8859-1-string id :length length)))

;; frame IDs are just like other regular IDs in the ID3 spec, but they are
;; designed to signal the `in-padding' "exception" to transfer control up
;; to the `read-frame' function when padding is found.
(define-binary-type frame-id (length)
  (:reader (in)
           (let ((first-byte (read-byte in)))
             (if (= first-byte 0) (signal 'in-padding))
             (let ((rest (read-value 'iso-8859-1-string in :length (1- length))))
               (concatenate 'string (string (code-char first-byte)) rest))))
  (:writer (out id)
           (write-value 'iso-8859-1-string id :length length)))

(define-tagged-binary-class id3-frame ()
    ((:class-finder (find-frame-class id)))
  (id (frame-id :length 3))
  (size u3))

;; The decision to make this a base class (as opposed to subclassing
;; `id3-frame') is done to avoid the concrete frame classes in each
;; version (2.2 and 2.3) duplicate the `data' field. However, the tradeoff
;; is that we lose access to the `size' field, so a workaround is needed
;; to make it work; using `first-object-in-processing-stack' is that workaround.
;; However, it's harder to reason about this because we now have to think about
;; how things look like at runtime, and convince ourselves that the call to
;; `data-bytes' will return the correct value (based on the return value from
;; `first-object-in-processing-stack')
;;
(define-binary-class generic-id3-frame ()
  (data (raw-bytes :size (data-bytes (first-object-in-processing-stack))
                   :max-size (megabytes->bytes 1.0))))

(defgeneric data-bytes (frame))

(define-binary-type id3-frames (tag-size frame-type)
  (:reader (in)
           (loop with to-read = tag-size
                 while (plusp to-read)
                 for frame = (read-frame frame-type in)
                 while frame
                 do (decf to-read (+ (frame-header-size frame) (size frame)))
                 collect frame
                 ;; skip over null padding
                 finally (loop repeat (1- to-read) do (read-byte in))))
  (:writer (out frames)
           (loop with to-write = tag-size
                 for frame in frames
                 do (write-value frame-type out frame)
                    (decf to-write (+ (frame-header-size frame) (size frame)))
                    ;; write null padding if necessary
                 finally (loop repeat to-write do (write-byte 0 out)))))

(defun read-frame (frame-type in)
  (handler-case (read-value frame-type in)
    (in-padding () nil)))

(defun show-error (message &rest args)
  (prog-nil
    (unless *silence-errors*
      (format t "ERROR: ~a~%" (apply #'format nil message args)))))

(defun read-id3 (filepath)
  "Return the ID3 of `filepath' or `nil' if there was an error during parsing."
  (let ((shortpath (enough-namestring filepath)))
    (with-open-file (in filepath :element-type '(unsigned-byte 8))
      (handler-case (read-value 'id3-tag in)
        (missing-id3-tag ()
          (show-error "missing ID3 tag (maybe it is IDv1?): ~a" shortpath))
        (data-too-large (condition)
          (show-error "raw data too large (~a -- ~a) (unsupported ID3 version?) -- skipping~"
                      shortpath
                      (human-readable-size (size condition))))
        (unsupported-version (condition)
          (show-error "unsupported major version (~a): ~a" (major-version condition) shortpath))
        (error (condition)
          (show-error "failed to process ~a: ~a" shortpath condition))))))

(defun show-tag-headers (directory)
  "Show the ID3 tag header for all mp3 files under `directory'"
  (flet
      ((show-tag-header (file)
         (if-let (id3-tag (read-id3 file))
           (with-slots (identifier major-version revision flags size) id3-tag
             (format t "~a ~d.~d ~8,'0b ~d bytes -- ~a~%"
                     identifier major-version revision flags size (enough-namestring file))))))
    (walk-directory directory #'show-tag-header
                    :recursively t
                    :file-condition (fn-and mp3-p id3-p))))

(defun show-mp3s (directory)
  (walk-directory directory #'show-mp3-metadata
                  :recursively t
                  :file-condition #'mp3-p))

(defun count-versions (directory)
  "Count the total number of mp3 files grouped by the version of their ID3 tags"
  (let ((version-counts (mapcar #'(lambda (version) (cons version 0)) '(2 3 4))))
    (flet ((count-version (file)
             ;; FIXME: design and implement a multi-argument version of `if-let'
             (if-let (tag (read-id3 file))
               (if-let (major-version (assoc (major-version tag) version-counts))
                 (incf (cdr major-version))))))
      (walk-directory directory #'count-version
                      :recursively t
                      :file-condition #'mp3-p))
    (loop for (version . count) in version-counts
          collect `(:version ,version :count ,count))))

(defun mp3-p (filepath)
  "Does `filepath' point to what seems to be an mp3 file?"
  (and (file-pathname-p filepath)
       (string-equal "mp3" (pathname-type filepath))))

(defun id3-p (filepath)
  "Is `filepath' an mp3 file with a valid ID3 tag?"
  (with-open-file (in filepath :element-type '(unsigned-byte 8))
    (string= "ID3" (read-value 'iso-8859-1-string in :length 3))))

(defun show-mp3-metadata (filepath)
  (if-let (id3 (read-id3 filepath))
    (format t "file: ~a ~%" (enough-namestring filepath))
    (show-id3-information id3))
  (format t "~%"))

(defun show-id3-information (tag)
  (format t "version: v2.~a" (major-version tag))
  (format t "~&artist: ~a ~&song: ~a ~&album: ~a ~&genre: ~a ~&year: ~a ~%"
          (artist tag)
          (song tag)
          (album tag)
          (genre tag)
          (year tag)))

(defun find-frame-class (name)
  (cond
    ((and (char= (char name 0) #\T)
          (not (member name '("TXX" "TXXX") :test #'string=)))
     (ecase (length name)
       (3 'text-info-frame-v2.2)
       (4 'text-info-frame-v2.3)))
    ((string= name "COM") 'comment-frame-v2.2)
    ((string= name "COMM") 'comment-frame-v2.3)
    (t
     (ecase (length name)
       (3 'generic-id3-frame-v2.2)
       (4 'generic-id3-frame-v2.3)))))

(defun frame-id (plist)
  (getf plist :id))

(defun frame-version (plist)
  (getf plist :version))

(defun frame-types (filepath)
  (if-let (id3 (read-id3 filepath))
    (delete-duplicates
     (mapcar #'(lambda (frame) (list :id (id frame) :version (major-version id3)))
             (frames id3))
     :key #'frame-id
     :test #'string=)))

(defun frame-types-in-directory (directory &key sort-by)
  (with-result-as (ids)
    (flet
        ((collect (filepath)
           (setf ids (nunion ids (frame-types filepath) :key #'frame-id :test #'string=))))
      (walk-directory directory #'collect
                      :recursively t
                      :file-condition #'mp3-p))
    (case sort-by
      (id      (sort! ids #'string< :key #'frame-id))
      (version (sort! ids #'< :key #'frame-version)))))

(defun non-terminated-type (encoding)
  (ecase encoding
    (0 'iso-8859-1-string)
    (1 'ucs-2-string)))

(defun terminated-type (encoding)
  (ecase encoding
    (0 'iso-8859-1-terminated-string)
    (1 'ucs-2-terminated-string)))

(defun string-args (encoding length terminator)
  (cond
    (length
     (values (non-terminated-type encoding) :length length))
    (terminator
     (values (terminated-type encoding) :terminator terminator))))

(define-binary-type id3-encoded-string (encoding length terminator)
  (:reader (in)
           (multiple-value-bind (type keyword arg)
               (string-args encoding length terminator)
             (read-value type in keyword arg)))
  (:writer (out string)
           (multiple-value-bind (type keyword arg)
               (string-args encoding length terminator)
             (write-value type out string keyword arg))))

(define-binary-class text-info-frame ()
  (encoding u1)
  (information (id3-encoded-string :encoding encoding :length (bytes-left 1))))

(define-binary-class comment-frame ()
  (encoding u1)
  (language (iso-8859-1-string :length 3))
  (description (id3-encoded-string :encoding encoding :terminator +null+))
  (text (id3-encoded-string
         :encoding encoding
         :length (bytes-left
                  (+ 1 ; encoding
                     3 ; language
                     (encoded-string-length description encoding t))))))

(defun bytes-left (bytes-read)
  (- (size (first-object-in-processing-stack))
     bytes-read))

(defun encoded-string-length (string encoding terminated)
  (let ((characters (+ (length string) (if terminated 1 0))))
    (* characters (ecase encoding (0 1) (1 2)))))

(defun upto-null (string)
  (subseq string 0 (position +null+ string)))

(defun find-frame (id3 ids)
  (find-if #'(lambda (frame) (find (id frame) ids :test #'string=))
           (frames id3)))

(defun get-text-info (id3 &rest ids)
  (let ((frame (find-frame id3 ids)))
    (when frame (upto-null (information frame)))))

(defmacro def-id3-getter (name &rest ids)
  `(defun ,name (id3)
     (get-text-info id3 ,@ids)))

(def-id3-getter song "TT2" "TIT2")
(def-id3-getter album "TAL" "TALB")
(def-id3-getter artist "TP1" "TPE1")
(def-id3-getter track "TRK" "TRCK")
(def-id3-getter year "TYE" "TYER" "TDRC")
(def-id3-getter genre "TCO" "TCON")

(defun translated-genre (id3)
  (let ((genre (genre id3)))
    (if (and genre (char= #\( (char genre 0)))
        (translate-v1-genre genre)
        genre)))

(defun translate-v1-genre (genre)
  (aref *id3-v1-genres* (parse-integer genre :start 1 :junk-allowed t)))

;; -----------------------------------------------------------------------------
;; ID3 V2.2
;; -----------------------------------------------------------------------------
(define-binary-class id3v2.2-tag (id3-tag)
  (frames (id3-frames :tag-size size :frame-type 'id3v2.2-frame)))

(define-tagged-binary-class id3v2.2-frame ()
    ((:class-finder (find-frame-class id)))
  (id (frame-id :length 3))
  (size u3))

(define-binary-class generic-id3-frame-v2.2 (id3v2.2-frame generic-id3-frame))

(defmethod frame-header-size ((frame id3v2.2-frame)) 6)

(defmethod data-bytes ((frame id3v2.2-frame))
  (size frame))

(define-binary-class text-info-frame-v2.2 (id3v2.2-frame text-info-frame))

(define-binary-class comment-frame-v2.2 (id3v2.2-frame comment-frame))

;; -----------------------------------------------------------------------------
;; ID3 V2.3
;; -----------------------------------------------------------------------------
(define-binary-class id3v2.3-tag (id3-tag)
  (extended-header-size (optional :type 'u4 :if (extended-p flags)))
  (extra-flags (optional :type 'u2 :if (extended-p flags)))
  (padding-size (optional :type 'u4 :if (extended-p flags)))
  (crc (optional :type 'u4 :if (crc-p flags extra-flags)))
  (frames (id3-frames :tag-size size :frame-type 'id3v2.3-frame)))

(define-tagged-binary-class id3v2.3-frame ()
    ((:class-finder (find-frame-class id)))
  (id (frame-id :length 4))
  (size u4)
  (flags u2)
  (decompressed-size (optional :type 'u4 :if (frame-compressed-p flags)))
  (encryption-scheme (optional :type 'u1 :if (frame-encrypted-p flags)))
  (grouping-identity (optional :type 'u1 :if (frame-grouped-p flags))))

(define-binary-class generic-id3-frame-v2.3 (id3v2.3-frame generic-id3-frame))

(define-binary-class text-info-frame-v2.3 (id3v2.3-frame text-info-frame))

(define-binary-class comment-frame-v2.3 (id3v2.3-frame comment-frame))

(defmethod frame-header-size ((frame id3v2.3-frame)) 10)

(defmethod data-bytes ((frame id3v2.3-frame))
  (let ((flags (flags frame)))
    (- (size frame)
       (if (frame-compressed-p flags) 4 0)
       (if (frame-encrypted-p flags) 1 0)
       (if (frame-grouped-p flags) 1 0))))

(defun extended-p (flags)
  (logbitp 6 flags))

(defun crc-p (flags extra-flags)
  "Do the specification flags indicate there is cyclic redundancy check data?"
  (and (extended-p flags) (logbitp 15 extra-flags)))

(defun frame-compressed-p (flags)
  (logbitp 7 flags))

(defun frame-encrypted-p (flags)
  (logbitp 6 flags))

(defun frame-grouped-p (flags)
  (logbitp 5 flags))
