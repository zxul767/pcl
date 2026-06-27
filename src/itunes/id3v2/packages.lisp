(in-package #:cl-user)

(defpackage #:dev.zxul767.id3v2
  (:use
   #:cl
   #:dev.zxul767.prelude
   #:dev.zxul767.binary
   #:dev.zxul767.functools)
  (:local-nicknames (#:path #:dev.zxul767.pathnames))
  (:export
   #:read-id3
   #:mp3-p
   #:id3-p
   #:album
   #:composer
   #:genre
   #:encoding-program
   #:artist
   #:part-of-set
   #:track
   #:song
   #:year
   #:size
   #:translated-genre))
