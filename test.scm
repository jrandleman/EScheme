


(define start (epoch-time))









; print-pascal-triangle.scm 
; => may be executed with or without a positive integer cmd-line argument (defaults to 20)

; (define (generate-next-row row)
;   (cons 0 (map + row (append (cdr row) (quote (0))))))

; (define (generate-pascal-triangle n acc)
;   (if (zero? n)
;       (reverse acc)
;       (generate-pascal-triangle (- n 1) (cons (generate-next-row (car acc)) acc))))

; (define (get-pascal-triangle n)
;   (if (zero? n)
;       (quote ((0)))
;       (generate-pascal-triangle (- n 1) (quote ((0 1 0) (0))))))

; (define (print-pascal-triangle n)
;   (for-each (lambda (r) (write r) (newline)) (get-pascal-triangle n)))




; (get-pascal-triangle 3000)

; (get-pascal-triangle 20)





; (define n (if (null? *argv*) 20 (string->number (car *argv*))))
; (print-pascal-triangle n) ; Prints "n" layers of Pascal's triangle












(define-class C
  ((g) (error "in g!"))
  ((f) (self.g)))

(def (f) 99)


(f)

(def o (C))
(displayf "%a\n" o.f)
; (o.f)



; (define (func) (error "In func!"))

; (define (geostation) (func))

; (define (harries) (geostation))

; (harries)












; (do ((i 0 (+ i 1)))
;     ((>= i 200))
;     (displayf "Here: %n\n" (sqrt -1))
;     )










; (define-syntax assert
;   (lambda (condition)
;     (def cond-result (gensym))
;     `(let ((,cond-result ,condition))
;       (if (not ,cond-result)
;           (errorf "Assert Failed: %wa" ',condition)))))

; ; (assert (eq? 1 1.0))

; (assert (= 1 1.0))










(define end (epoch-time))

(displayf "\nTime elapsed: %n\n" (/ (- end start) 1000.0))



; ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ; ;; TESTING THREADS + CALL/CC


; ; (define *end* 26)


; ; (define (fwrap)

; ;   (define-generator (f1)
; ;     (let loop1 ((n 0))
; ;       (if (< n *end*)
; ;           (begin 
; ;             (dosync 
; ;               (display n) 
; ;               (newline))
; ;             (yield)
; ;             (loop1 (+ n 1))))))

; ;   (define-generator (f2)
; ;     (let loop2 ((n 0))
; ;       (if (< n *end*)
; ;           (begin 
; ;             (dosync 
; ;               (display (+ n 1000)) 
; ;               (newline))
; ;             (yield)
; ;             (loop2 (+ n 1))))))

; ;   (define-generator (f3)
; ;     (let loop3 ((n 0))
; ;       (if (< n *end*)
; ;           (begin 
; ;             (dosync 
; ;               (write (number->string n)) 
; ;               (newline))
; ;             (yield)
; ;             (loop3 (+ n 1))))))


; ;   (complete-all-generators! (f1) (f2) (f3))

; ; )


; ; (parallel fwrap fwrap fwrap)




; ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ; ;; TESTING OO


; ; (define-class Rectangle
; ;   ((new w h)
; ;     (define self.width w)
; ;     (define self.height h))
; ;   ((area)
; ;     (* self.width self.height))
; ;   ((perimeter)
; ;     (* 2 (+ self.width self.height))))


; ; (define-class Square (:extends Rectangle)
; ;   ((new l)
; ;     (super! l l)))


; ; (define s (Square 12))

; ; (write s)
; ; (newline)
; ; (write (s.area))
; ; (newline)
; ; (write (s.perimeter))
; ; (newline)