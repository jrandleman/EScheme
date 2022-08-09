


(define start (epoch-time))





; (define-syntax assert
;   (lambda (condition)
;     (def cond-result (gensym))
;     `(let ((,cond-result ,condition))
;       (if (not ,cond-result)
;           (errorf "Assert Failed: %wa" ',condition)))))

; ; (assert (eq? 1 1.0))

; (assert (= 1 1.0))










(define end (epoch-time))

(displayf "Time elapsed: %n\n" (/ (- end start) 1000.0))



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;; TESTING THREADS + CALL/CC


; (define *end* 26)


; (define (fwrap)

;   (define-generator (f1)
;     (let loop1 ((n 0))
;       (if (< n *end*)
;           (begin 
;             (dosync 
;               (display n) 
;               (newline))
;             (yield)
;             (loop1 (+ n 1))))))

;   (define-generator (f2)
;     (let loop2 ((n 0))
;       (if (< n *end*)
;           (begin 
;             (dosync 
;               (display (+ n 1000)) 
;               (newline))
;             (yield)
;             (loop2 (+ n 1))))))

;   (define-generator (f3)
;     (let loop3 ((n 0))
;       (if (< n *end*)
;           (begin 
;             (dosync 
;               (write (number->string n)) 
;               (newline))
;             (yield)
;             (loop3 (+ n 1))))))


;   (complete-all-generators! (f1) (f2) (f3))

; )


; (parallel fwrap fwrap fwrap)




; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;; TESTING OO


; (define-class Rectangle
;   ((new w h)
;     (define self.width w)
;     (define self.height h))
;   ((area)
;     (* self.width self.height))
;   ((perimeter)
;     (* 2 (+ self.width self.height))))


; (define-class Square (:extends Rectangle)
;   ((new l)
;     (super! l l)))


; (define s (Square 12))

; (write s)
; (newline)
; (write (s.area))
; (newline)
; (write (s.perimeter))
; (newline)

