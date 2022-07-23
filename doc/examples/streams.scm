; streams.scm
; => Demos use of EScheme streams (implemented via macros)
; => This file does not need any cmd-line arguments when being executed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generating Primes
(define (sieve int-stream)
  (scons 
    (scar int-stream)
    (sieve 
      (stream-filter 
        (lambda (n) (positive? (remainder n (scar int-stream))))
        (scdr int-stream)))))

(define int-stream-from-2
  (let loop ((n 2))
    (scons n (loop (+ n 1)))))

(define primes (sieve int-stream-from-2))

(display (stream->list primes 30))
(newline)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generating Fibonacci Numbers
(define fibs
  (let loop ((a 0) (b 1))
    (scons a (loop b (+ a b)))))

(display (stream->list fibs 30))
(newline)

