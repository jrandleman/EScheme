



(define-class C)

(def o (C))
(write o)
(newline)





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

