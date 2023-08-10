; print-pascal-triangle.scm 
; => may be executed with or without a positive integer cmd-line argument (defaults to 20)

(define (generate-next-row row)
  (cons 0 (map + row (append (cdr row) (quote (0))))))

(define (generate-pascal-triangle n acc)
  (if (zero? n)
      (reverse acc)
      (generate-pascal-triangle (- n 1) (cons (generate-next-row (car acc)) acc))))

(define (get-pascal-triangle n)
  (if (zero? n)
      (quote ((0)))
      (generate-pascal-triangle (- n 1) (quote ((0 1 0) (0))))))

(define (print-pascal-triangle n)
  (for-each (lambda (r) (write r) (newline)) (get-pascal-triangle n)))

(define n (if (< (length *argv*) 2) 20 (string->number (cadr *argv*))))

(print-pascal-triangle n) ; Prints "n" layers of Pascal's triangle
