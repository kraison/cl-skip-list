;; ASDF package description for cl-skip-list              -*- Lisp -*-

(defpackage :cl-skip-list-system (:use :cl :asdf))
(in-package :cl-skip-list-system)

(defsystem cl-skip-list
  :name "cl-skip-list"
  :maintainer "Kevin Raison"
  :author "Kevin Raison <last name @ chatsubo dot net>"
  :version "0.1"
  :description "Concurrent lock-free skip list."
  :long-description "Concurrent lock-free skip list."
  :depends-on (:cffi)
  :components ((:file "cl-skip-list-package")
	       (:file "random" :depends-on ("cl-skip-list-package"))
	       (:file "utilities" :depends-on ("cl-skip-list-package"))
	       (:file "gettimeofday" :depends-on ("cl-skip-list-package"))
	       (:file "constants" :depends-on ("cl-skip-list-package"))
	       (:file "mcas" :depends-on ("utilities" "constants" "gettimeofday"))
	       (:file "skip-list" :depends-on ("mcas" "random"))
	       (:file "skip-pq" :depends-on ("skip-list"))))
