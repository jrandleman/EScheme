;; Author: Jordan Randleman -- equality.scm
;; => Tests for EScheme's primitive functions.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load (path (path-parent #path 2) "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(ut (eq? #\1) #t)
(ut (equal? #f) #t)
(ut (eq? #\1 #\1 #\1) #t)
(ut (equal? #\1 #\1 #\1) #t)
(ut (eq? #\2 #\1 #\1) #f)
(ut (equal? #\2 #\1 #\1) #f)
(ut (eq? #\1 #\2 #\1) #f)
(ut (equal? #\1 #\2 #\1) #f)
(ut (eq? #\1 #\1 #\2) #f)
(ut (equal? #\1 #\1 #\2) #f)

; test <=> alaising eq? for 2+ non-numbers
(ut (= #\1 #\1 #\1) #t)
(ut (= #\2 #\1 #\1) #f)
(ut (= #\1 #\2 #\1) #f)
(ut (= #\1 #\1 #\2) #f)
(ut (= #\2 #\2 #\2) #t)
(ut (eq? 1 1.0) #f)
(ut (= 1 1.0) #t) ; still coerces for numbers

; testing `eq?` & `equal?` on value & reference objects
(define c (class))
(ut (eq? c c) #t)
(ut (eq? (class) (class)) #f)
(ut (equal? (class) (class)) #f)
(ut (eq? (c) (c)) #f)
(ut (equal? (c) (c)) #t)
