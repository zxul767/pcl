(in-package #:cl-user)

(defpackage #:dev.zxul767.mp3-database
  (:use
   #:cl
   #:dev.zxul767.prelude
   #:dev.zxul767.functools
   ;; TODO: import with local nickname `path`
   #:dev.zxul767.pathnames
   #:dev.zxul767.id3v2)
  (:export
   #:*default-table-size*
   #:column
   #:column-value
   #:create-mp3-table
   #:get-mp3-table
   #:get-mp3-schema
   #:delete-all-rows
   #:delete-rows
   #:do-rows
   #:ensure-non-nullable
   #:extract-schema
   #:in
   #:insert-row
   #:load-database
   #:make-column
   #:make-schema
   #:map-rows
   #:matching
   #:nth-row
   #:print-table
   #:random-selection
   #:schema
   #:select
   #:shuffle-table
   #:sort-rows
   #:table
   #:table-size
   #:with-column-values))
