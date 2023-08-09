;; Author: Jordan Randleman -- stream.scm
;; => Tests for EScheme's primitive stream syntax.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load (path (path-parent #path 2) "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
; helper syntax to reuse c****r tests from './procedures/pair.scm' as sc****r test
(define-syntax stream
  (lambda xs
    (if (null? xs) '() `(scons ,(car xs) (stream ,@(cdr xs))))))

(define s1 (scons 0 (scons 1 (scons 2 (scons 3 '())))))
(ut (stream-pair? s1) #t)
(ut (stream-pair? '()) #f)
(ut (stream? s1) #t)
(ut (stream? '()) #t)

(ut (scaar (stream (stream 1))) 1)
(ut (scadr (stream 1 2)) 2)
(ut (scar (scdar (stream (stream 1 2)))) 2)
(ut (scar (scddr (stream 1 2 3))) 3)

(ut (scaaar (stream (stream (stream 1)))) 1)
(ut (scaadr (stream 1 (stream 2))) 2)
(ut (scar (scadar (stream (stream 1 (stream 2))))) 2)
(ut (scar (scaddr (stream 1 2 (stream 3)))) 3)
(ut (scar (scdaar (stream (stream (stream 1 2))))) 2)
(ut (scar (scdadr (stream 1 (stream 2 3)))) 3)
(ut (scar (scddar (stream (stream 1 2 3)))) 3)
(ut (scar (scdddr (stream 1 2 3 4))) 4)

(ut (scaaaar (stream (stream (stream (stream 1))))) 1)
(ut (scaaadr (stream 1 (stream (stream 2)))) 2)
(ut (scar (scaadar (stream (stream 1 (stream (stream 2)))))) 2)
(ut (scar (scaaddr (stream 1 2 (stream (stream 3))))) 3)
(ut (scar (scadaar (stream (stream (stream 1 (stream 2)))))) 2)
(ut (scar (scadadr (stream 1 (stream 2 (stream 3))))) 3)
(ut (scar (scaddar (stream (stream 1 2 (stream 3))))) 3)
(ut (scar (scadddr (stream 1 2 3 (stream 4)))) 4)
(ut (scar (scdaaar (stream (stream (stream (stream 1 2)))))) 2)
(ut (scar (scdaadr (stream 1 (stream (stream 2 3))))) 3)
(ut (scar (scdadar (stream (stream 1 (stream 2 3))))) 3)
(ut (scar (scdaddr (stream 1 2 (stream 3 4)))) 4)
(ut (scar (scddaar (stream (stream (stream 1 2 3))))) 3)
(ut (scar (scddadr (stream 1 (stream 2 3 4)))) 4)
(ut (scar (scdddar (stream (stream 1 2 3 4)))) 4)
(ut (scar (scddddr (stream 1 2 3 4 5))) 5)

(define pos-ints (let loop ((n 1)) (scons n (loop (+ n 1)))))
(ut (stream->list pos-ints 0) '())
(ut (stream->list pos-ints 10) '(1 2 3 4 5 6 7 8 9 10))
(ut (stream-ref pos-ints 0) 1)
(ut (stream-ref pos-ints 9) 10)

(define even-pos-ints (stream-map + pos-ints pos-ints))
(ut (stream->list even-pos-ints 0) '())
(ut (stream->list even-pos-ints 10) '(2 4 6 8 10 12 14 16 18 20))
(ut (stream-ref even-pos-ints 0) 2)
(ut (stream-ref even-pos-ints 9) 20)

(define neg-ints (stream-map - pos-ints))
(ut (stream->list neg-ints 0) '())
(ut (stream->list neg-ints 10) '(-1 -2 -3 -4 -5 -6 -7 -8 -9 -10))
(ut (stream-ref neg-ints 0) -1)
(ut (stream-ref neg-ints 9) -10)

(ut (stream-map + '()) '())

(define even-pos-ints-2 (stream-filter even? pos-ints))
(ut (stream->list even-pos-ints-2 0) '())
(ut (stream->list even-pos-ints-2 10) '(2 4 6 8 10 12 14 16 18 20))
(ut (stream-ref even-pos-ints-2 0) 2)
(ut (stream-ref even-pos-ints-2 9) 20)

(ut (stream-filter even? '()) '())

(define even-pos-ints-3 (stream-iterate (lambda (x) (+ x 2)) 2))
(ut (stream->list even-pos-ints-3 0) '())
(ut (stream->list even-pos-ints-3 10) '(2 4 6 8 10 12 14 16 18 20))
(ut (stream-ref even-pos-ints-3 0) 2)
(ut (stream-ref even-pos-ints-3 9) 20)

(define stream-of-1-2-3s (stream-constant 1 2 3))
(ut (stream->list stream-of-1-2-3s 0) '())
(ut (stream->list stream-of-1-2-3s 10) '(1 2 3 1 2 3 1 2 3 1))
(ut (stream-ref stream-of-1-2-3s 0) 1)
(ut (stream-ref stream-of-1-2-3s 9) 1)
(ut (stream-constant) '())

(define stream-of-1-2-3 (stream-append (stream 1) (stream 2) (stream 3)))
(ut (stream->list stream-of-1-2-3 0) '())
(ut (stream->list stream-of-1-2-3 3) '(1 2 3))
(ut (stream-ref stream-of-1-2-3 0) 1)
(ut (stream-ref stream-of-1-2-3 2) 3)

(define stream-of-1-2-3 (stream-append (stream 1 2) (stream 3)))
(ut (stream->list stream-of-1-2-3 0) '())
(ut (stream->list stream-of-1-2-3 3) '(1 2 3))
(ut (stream-ref stream-of-1-2-3 0) 1)
(ut (stream-ref stream-of-1-2-3 2) 3)

(define stream-of-1-2-3 (stream-append (stream 1) (stream 2 3)))
(ut (stream->list stream-of-1-2-3 0) '())
(ut (stream->list stream-of-1-2-3 3) '(1 2 3))
(ut (stream-ref stream-of-1-2-3 0) 1)
(ut (stream-ref stream-of-1-2-3 2) 3)

(ut (stream->list (stream-append '() (stream 1 2)) 2) '(1 2))
(ut (stream->list (stream-append (stream 1 2) '()) 2) '(1 2))
(ut (stream-append '() '() '()) '())

(define odd-pos-ints (stream-filter odd? pos-ints))
(define pos-ints-2 (stream-interleave odd-pos-ints even-pos-ints))
(ut (stream->list pos-ints-2 0) '())
(ut (stream->list pos-ints-2 10) '(1 2 3 4 5 6 7 8 9 10))
(ut (stream-ref pos-ints-2 0) 1)
(ut (stream-ref pos-ints-2 9) 10)

(define even-pos-ints-generator (stream->generator even-pos-ints))
(do ((i 0 (+ i 1)))
    ((= i 10))
    (ut (even-pos-ints-generator) (stream-ref even-pos-ints i)))
