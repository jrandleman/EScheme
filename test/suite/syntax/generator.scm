;; Author: Jordan Randleman -- generator.scm
;; => Tests for EScheme's primitive generator syntax.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load (path (path-parent #path 2) "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(define generator-container [])
(define-generator (save-numbers start total)
  (let loop ((i start))
    (if (< i total)
          (begin 
            (vector-push! generator-container i)
            (yield) ; pause execution until re-invoked
            (loop (+ i 1))))))
(define-generator (save-strings start total)
  (let loop ((i start))
    (if (< i total)
          (begin 
            (vector-push! generator-container (number->string i))
            (yield) ; pause execution until re-invoked
            (loop (+ i 1))))))
(complete-all-generators! (save-numbers 0 10) (save-strings 0 10))
(ut generator-container [0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9"])

(set! generator-container [])
(define-generator (save-chars start total)
  (let loop ((i start))
    (if (< i total)
          (begin 
            (vector-push! generator-container (integer->char i))
            (yield) ; pause execution until re-invoked
            (loop (+ i 1))))))

(complete-n-generators! 3 (save-numbers 0 10) (save-strings 0 10) (save-chars 0 100)) ; same as <complete-all-generators!>
(ut (length generator-container) 120)

(set! generator-container [])
(complete-n-generators! 10 (save-numbers 0 10) (save-strings 0 10) (save-chars 0 100)) ; same as <complete-all-generators!>
(ut (length generator-container) 120)

(set! generator-container [])
(complete-n-generators! 2 (save-numbers 0 10) (save-strings 0 10) (save-chars 0 100)) ; guarentee only 2 generators were completed
(ut (and (>= (length generator-container) 20) (< (length generator-container) 120)) #t)

(define-generator (powers-of-2-generator)
  (let loop ((n 0))
    (yield (expt 2 n))
    (loop (+ n 1))))
(define powers-of-2 (powers-of-2-generator))
(ut (powers-of-2) 1)
(ut (powers-of-2) 2)
(ut (powers-of-2) 4)
(ut (powers-of-2) 8)
(ut (powers-of-2) 16)
(ut (powers-of-2) 32)
