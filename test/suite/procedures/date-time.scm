;; Author: Jordan Randleman -- date-time.scm
;; => Tests for EScheme's primitive functions.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load (path (path-parent #path 2) "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(define ct (current-time))
(ut (length ct) 4)
(ut (integer? (car ct)) #t)
(ut (integer? (cadr ct)) #t)
(ut (integer? (caddr ct)) #t)
(ut (integer? (cadddr ct)) #t)

(define cd (current-date))
(ut (length cd) 3)
(ut (integer? (car cd)) #t)
(ut (integer? (cadr cd)) #t)
(ut (integer? (caddr cd)) #t)

(ut (integer? (epoch-time)) #t)

(ut (string? (time-zone)) #t)

(ut (string? (day)) #t)
(ut (string? (month)) #t)
(ut (string? (year)) #t)
