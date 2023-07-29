;; Author: Jordan Randleman -- number.scm
;; => Tests for EScheme's primitive functions.
;;    Invoked by ../../main.scm
;; => Note that more tests are executed by those in `./number-tests/`,
;;    having been generated by `./.escm-number-unit-test-generator.scm`.
;; => Here, we simply execute number tests that aren't available in Chez-Scheme.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load (path #path ".." ".." "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS

; All `#t` solutions to `number?` are provided in `./number-tests/`
(ut (number? #\a) #f)
(ut (number? #t) #f)
(ut (number? "a") #f)

(define r1 (random))
(define r2 (random))
(ut (inexact? r1) #t)
(ut (inexact? r2) #t)
(ut (= r1 r2) #f)

(ut (modf NaN) '(NaN . NaN))
(ut (modf Infinity) '(Infinity . 0.0))
(ut (modf -Infinity) '(-Infinity . -0.0))
(ut (modf 1.0) '(1.0 . 0.0))
(ut (modf -1.0) '(-1.0 . -0.0))
(ut (modf 1.5) '(1.0 . 0.5))
(ut (modf -1.5) '(-1.0 . -0.5))
(ut (modf 1) '(1.0 . 0.0))
(ut (modf -1) '(-1.0 . -0.0))
(ut (modf 3/2) '(1.0 . 0.5))
(ut (modf -3/2) '(-1.0 . -0.5))
(ut (modf 1/8) '(0.0 . 0.125))
(ut (modf -1/8) '(-0.0 . -0.125))

(ut (integral 1.0) 1)
(ut (integral -1.0) -1)
(ut (integral 1.5) 1)
(ut (integral -1.5) -1)
(ut (integral 1) 1)
(ut (integral -1) -1)
(ut (integral 3/2) 1)
(ut (integral -3/2) -1)

(ut (fractional 1.0) 0)
(ut (fractional -1.0) -0)
(ut (fractional 1.5) 5)
(ut (fractional -1.5) -5)
(ut (fractional 1) 0)
(ut (fractional -1) -0)
(ut (fractional 3/2) 5)
(ut (fractional -3/2) -5)
(ut (fractional 1/8) 125)
(ut (fractional -1/8) -125)

(ut eq? ** expt) ; verify alias
