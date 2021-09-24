(defpackage :dev.zxul767.shoutcast
  (:use
   :common-lisp
   :net.aserve
   :dev.zxul767.macrotools
   :dev.zxul767.id3v2)
  (:export
   :song
   :file
   :title
   :id3-size
   :find-songs-source
   :current-song
   :still-current-p
   :maybe-move-to-next-song
   :*songs-source-type*))
