;; Author: Jordan Randleman -- utility.scm
;; => Tests for EScheme's primitive functions.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load (path (path-parent #path 2) "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(ut (symbol? (typeof 12)) #t)

; NO TEST FOR `error`
; NO TEST FOR `errorf`

(ut (copy 12) 12)
(ut (equal? (copy [1]) [1]) #t)
(ut (equal? (copy '(1)) '(1)) #t)
(ut (eq? (copy [1]) [1]) #f)
(ut (eq? (copy '(1)) '(1)) #t)

; testing chached force/delay values
(ut (begin (define v 42) (define p (delay (set! v (+ v 1)))) v) 42)
(ut (begin (force p) (force p) (force p) v) 43)

; testing call/cc [example from `https://en.wikipedia.org/wiki/Call-with-current-continuation`]:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; [LISTOF X] -> ( -> X u 'you-fell-off-the-end)
(define (wiki-call/cc-generate-one-element-at-a-time lst)
  ;; Both internal functions are closures over lst

  ;; Internal variable/Function which passes the current element in a list
  ;; to its return argument (which is a continuation), or passes an end-of-list marker 
  ;; if no more elements are left. On each step the function name is 
  ;; rebound to a continuation which points back into the function body,
  ;; while return is rebound to whatever continuation the caller specifies.
  (define (control-state return)
    (for-each 
     (lambda (element)
               (set! return (call-with-current-continuation
                              (lambda (resume-here)
                                ;; Grab the current continuation
                               (set! control-state resume-here)
                               (return element))))) ;; (return element) evaluates to next return
     lst)
    (return 'you-fell-off-the-end))
  
  ;; (-> X u 'you-fell-off-the-end)
  ;; This is the actual generator, producing one item from a-list at a time.
  (define (generator)
    (call-with-current-continuation control-state))

  ;; Return the generator 
  generator)

(define wiki-call/cc-generate-digit
  (wiki-call/cc-generate-one-element-at-a-time '(0 1 2)))

(ut (wiki-call/cc-generate-digit) 0)
(ut (wiki-call/cc-generate-digit) 1)
(ut (wiki-call/cc-generate-digit) 2)
(ut (wiki-call/cc-generate-digit) 'you-fell-off-the-end)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; testing dynamic-wind [adapted from `https://en.wikipedia.org/wiki/Call-with-current-continuation`]:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; [LISTOF X] -> ( -> X u 'you-fell-off-the-end)
(define wiki-dynamic-wind-count 1)

(define (wiki-dynamic-wind-generate-one-element-at-a-time lst)
  ;; Both internal functions are closures over lst

  ;; Internal variable/Function which passes the current element in a list
  ;; to its return argument (which is a continuation), or passes an end-of-list marker 
  ;; if no more elements are left. On each step the function name is 
  ;; rebound to a continuation which points back into the function body,
  ;; while return is rebound to whatever continuation the caller specifies.
  (define (control-state return)
    (for-each 
     (lambda (element)
               (set! return (call-with-current-continuation
                              (lambda (resume-here)
                                ;; Grab the current continuation
                               (set! control-state resume-here)
                               (return 
                                 (if (number? element)
                                     (* wiki-dynamic-wind-count element)
                                     element)))))) ;; (return element) evaluates to next return
     lst)
    (return 'you-fell-off-the-end))
  
  ;; (-> X u 'you-fell-off-the-end)
  ;; This is the actual generator, producing one item from a-list at a time.
  (define (generator)
    (dynamic-wind 
      (lambda () (set! wiki-dynamic-wind-count 2))
      (lambda () (call-with-current-continuation control-state))
      (lambda () (set! wiki-dynamic-wind-count 1))))

  ;; Return the generator 
  generator)

(define wiki-dynamic-wind-generate-even-digit
  (wiki-dynamic-wind-generate-one-element-at-a-time '(0 1 2)))

(ut wiki-dynamic-wind-count 1)
(ut (wiki-dynamic-wind-generate-even-digit) 0)
(ut wiki-dynamic-wind-count 1)
(ut (wiki-dynamic-wind-generate-even-digit) 2)
(ut wiki-dynamic-wind-count 1)
(ut (wiki-dynamic-wind-generate-even-digit) 4)
(ut wiki-dynamic-wind-count 1)
(ut (wiki-dynamic-wind-generate-even-digit) 'you-fell-off-the-end)
(ut wiki-dynamic-wind-count 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (values-test x)
  (values x (* x 2) (* x 3)))
(call-with-values 
  (lambda () (values-test 2)) 
  (lambda (fst snd thr)
    (ut fst 2)
    (ut snd 4)
    (ut thr 6)))

(define with-exception-handler-error-value #f)
(define (raising-function) (+ 1 (raise 'an-error)))
(call-with-current-continuation
 (lambda (escape-procedure)
   (with-exception-handler 
     (lambda (x) 
      (set! with-exception-handler-error-value x)
      (escape-procedure 'exception))
     raising-function)))
(ut with-exception-handler-error-value 'an-error)

(define time-result (time + 1 2 3))
(ut (number? (car time-result)) #t)
(ut (cadr time-result) 6)
