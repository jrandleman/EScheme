;; Author: Jordan Randleman -- boolean.scm
;; => Tests for EScheme's primitive functions.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load (path #path ".." ".." "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(ut (boolean? #t) #t)
(ut (boolean? #f) #t)
(ut (boolean? 0) #f)
(ut (boolean? '()) #f)

(ut (not #t) #f)
(ut (not #f) #t)
