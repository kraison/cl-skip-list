(in-package #:cl-skip-list)

;; MCAS status markers
(defconstant +mcas-undecided+ :undecided)
(defconstant +mcas-failed+ :failed)
(defconstant +mcas-succeeded+ :succeeded)
(defconstant +mcas-make-durable+ :make-durable)

;; MCAS transaction global
(defvar *mcas* nil)
