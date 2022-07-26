


(pprint
  (id (case (* 2 21)
          ((-42 2 3) "1" "2")
          ((2 -42 4) "3" "5")
          ((-42) => id)
          (else "4" "6"))))



; (define-syntax case
;   (lambda (value . clauses)
;     (define (make-consequence c) (cons (quote begin) c))
;     (define (make-condition c) 
;       (if (eq? c (quote else)) 
;           #t 
;           (list (quote member) value (cons (quote list) c))))
;     (define (arrow-syntax? c) (and (= (length c) 3) (eq? (quote =>) (cadr c))))
;     (define (arrow->let c a) 
;       (define condition-result (gensym))
;       (list (quote let) (list (list condition-result (list (quote member) value (cons (quote list) c))))
;         (list (quote if) condition-result
;             (list (caddr c) condition-result)
;             a)))
;     (fold-right (lambda (clause acc)
;                   (if (arrow-syntax? clause)
;                       (arrow->let clause acc)
;                       (list (quote if) (make-condition (car clause))
;                             (make-consequence (cdr clause))
;                             acc)))
;                 (quote (if #f #f)) ; inner expression yields #void
;                 clauses)))










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

