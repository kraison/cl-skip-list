(in-package #:cl-user)

(defpackage #:cl-skip-list
  (:use #:cl #:cffi)
  (:export 
   #:make-skip-list
   #:less-than
   #:skip-list?
   #:skip-list-empty?
   #:skip-list-to-list
   #:skip-list-lookup
   #:skip-list-replace-kv
   #:skip-list-add
   #:skip-list-delete
   #:sl-cursor
   #:skip-list-values-cursor
   #:skip-list-keys-cursor
   #:skip-list-range-cursor
   #:sl-cursor-next
   #:map-skip-list
   #:map-skip-list-values
   #:skip-list-fetch-all
   #:skip-list-length
   #:+mcas-succeeded+
   #:+mcas-undecided+
   #:+mcas-failed+

   #:make-skip-pq
   #:delete-min
   ))
