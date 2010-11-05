(require 'asdf)
(asdf:oos 'asdf:load-op 'cl-skip-list)
(in-package #:cl-skip-list)
(asdf:oos 'asdf:load-op 'fiveam)
;;(use-package (find-package "FIVEAM"))

(fiveam:def-suite skip-list-test-suite :description "Skip List Test Suite")
(fiveam:in-suite skip-list-test-suite)
(format t "~%~%Preparing to run all Skip List Tests.~%")

(fiveam:def-fixture sl/fixture
    (&key
     (sl-no-dupes (make-skip-list))
     (sl-dupes (make-skip-list :duplicates-allowed? t)))
  (&body))

(fiveam:test (skip-list-tests :fixture sl/fixture)
  ;;; no dupes tests
  (fiveam:is-true (skip-list? sl-no-dupes))
  (fiveam:is (eq t (skip-list-add sl-no-dupes 2 "V2")))
  (fiveam:is (eq t (skip-list-add sl-no-dupes :k6 "v6")))
  (fiveam:is (eq t (skip-list-add sl-no-dupes 1 1)))
  (fiveam:is (eq t (skip-list-add sl-no-dupes "K3" 3)))
  (fiveam:is (eq t (skip-list-add sl-no-dupes :k5 5)))
  (fiveam:is (eq t (skip-list-add sl-no-dupes "K4" "V4")))
  (fiveam:signals
      (skip-list-duplicate-error "Attempt to add node with duplicate key succeeded.  Not OK!")
    (skip-list-add sl-no-dupes :k6 "v666"))
  (fiveam:signals
      (skip-list-duplicate-error "Attempt to add node with duplicate key succeeded.  Not OK!")
    (skip-list-add sl-no-dupes 1 "v1"))
  (fiveam:signals
      (skip-list-duplicate-error "Attempt to add node with duplicate key succeeded.  Not OK!")
    (skip-list-add sl-no-dupes "K3" "v3"))
  (fiveam:is (= 1 (skip-list-lookup sl-no-dupes 1)))
  (fiveam:is (= 3 (skip-list-lookup sl-no-dupes "K3")))
  (fiveam:is (= 5 (skip-list-lookup sl-no-dupes :k5)))
  (fiveam:is (equal "V2" (skip-list-lookup sl-no-dupes 2)))
  (fiveam:is (equal "V4" (skip-list-lookup sl-no-dupes "K4")))
  (fiveam:is (equal "v6" (skip-list-lookup sl-no-dupes :k6)))
  (fiveam:is (eq t (skip-list-delete sl-no-dupes 1)))
  (fiveam:is (eq t (skip-list-delete sl-no-dupes 2)))
  (fiveam:is (eq t (skip-list-delete sl-no-dupes :k5)))
  ;; Dupe list tests UNFINISHED!
  (fiveam:is-true (skip-list? sl-dupes))
  (fiveam:is (eq t (skip-list-add sl-dupes 1 1)))
  (fiveam:is (eq t (skip-list-add sl-dupes 2 "V2")))
  (fiveam:is (eq t (skip-list-add sl-dupes "K3" 3)))
  (fiveam:is (eq t (skip-list-add sl-dupes "K4" "V4")))
  (fiveam:is (eq t (skip-list-add sl-dupes :k5 5)))
  (fiveam:is (eq t (skip-list-add sl-dupes :k6 "v6")))
  (fiveam:is (eq t (skip-list-add sl-dupes 1 11)))
  (fiveam:is (eq t (skip-list-add sl-dupes 2 "V12")))
  (fiveam:is (eq t (skip-list-add sl-dupes "K3" 13)))
  (fiveam:is (eq t (skip-list-add sl-dupes "K4" "V14")))
  (fiveam:is (eq t (skip-list-add sl-dupes :k5 15)))
  (fiveam:is (eq t (skip-list-add sl-dupes :k6 "v16")))
  ;; TEST LOOKUPS...
  )

(fiveam:run!)
