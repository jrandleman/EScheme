(define (f x)
  (* x 2))

(define (g x)
  (* (f x) 2))

(define-syntax *3
  (lambda (x)
    `(* ,x 3)))