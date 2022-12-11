


;; UNIT TEST FOR AC ALGO PRIMS ON LISTS
(define-syntax profile-call 
  (lambda (call)
    (define fcn (car call))
    (define args (cdr call))
    `(displayf "Effect of applying %wa to %wa = %wa\n" ,fcn (list ,@args) (apply ,fcn (list ,@args)))))


; (define l1 '(1 2 3))
; (define l2 '(4 5 6))
; (define l3 '(9 9 0 0 7 7 2 2 5 5 4 4 3 3 6 6 1 1 8 8))
; (define l4 '(1 3 5))
; (define l5 '(2 4 6))

(define l1 "123")
(define l2 "456")
(define l3 "99007722554433661188")
(define l4 "135")
(define l5 "246")

(define char->integer
  (let ((original char->integer))
    (lambda (c)
      (- (original c) (original #\0)))))

(define (char-odd? c) (odd? (char->integer c)))
(define (char-even? c) (even? (char->integer c)))


(profile-call (oc-conj #\9 l1))

(profile-call (oc-init l1))
(profile-call (oc-last l1))

(profile-call (oc-slice l1 1))
(profile-call (oc-slice l1 0 1))
(profile-call (oc-slice l1 0 4))
(profile-call (oc-slice "l1" 0 \(char=? %1 #\2)))
(profile-call (oc-slice "l1" 1 \(char=? %1 #\2)))
(profile-call (oc-slice "l1" 2 \(char=? %1 #\2)))

(profile-call (oc-reverse "l1"))

(profile-call (oc-remove-first char-odd? l1))
(profile-call (oc-remove-last char-odd? l1))

(profile-call (oc-remove-first char-even? l1))
(profile-call (oc-remove-last char-even? l1))

(profile-call (oc-remove-first \(< (char->integer %1) 4) l1))
(profile-call (oc-remove-last \(< (char->integer %1) 4) l1))

(profile-call (oc-skip char-even? l1))
(profile-call (oc-skip-right char-even? l1))

(profile-call (oc-skip char-odd? l1))
(profile-call (oc-skip-right char-odd? l1))

(profile-call (oc-fold-right \(integer->char (apply - (map char->integer %%))) #\0 l1 l2))

(profile-call (oc-key-right char-odd? l1))
(profile-call (oc-key-right char-even? l1))

(profile-call (oc-drop-right l1 0))
(profile-call (oc-drop-right l1 1))
(profile-call (oc-drop-right l1 2))
(profile-call (oc-drop-right l1 3))
(profile-call (oc-drop-right l1 4))

(profile-call (oc-drop-while char-odd? l1))
(profile-call (oc-drop-while char-even? l1))
(profile-call (oc-drop-while \(< (char->integer %1) 3) l1))
(profile-call (oc-drop-while \(>= (char->integer %1) 3) l1))

(profile-call (oc-drop-right-while char-odd? l1))
(profile-call (oc-drop-right-while char-even? l1))
(profile-call (oc-drop-right-while \(< (char->integer %1) 3) l1))
(profile-call (oc-drop-right-while \(>= (char->integer %1) 3) l1))

(profile-call (oc-take-right l1 0))
(profile-call (oc-take-right l1 1))
(profile-call (oc-take-right l1 2))
(profile-call (oc-take-right l1 3))
(profile-call (oc-take-right l1 4))

(profile-call (oc-take-while char-odd? l1))
(profile-call (oc-take-while char-even? l1))
(profile-call (oc-take-while \(< (char->integer %1) 3) l1))
(profile-call (oc-take-while \(>= (char->integer %1) 3) l1))

(profile-call (oc-take-right-while char-odd? l1))
(profile-call (oc-take-right-while char-even? l1))
(profile-call (oc-take-right-while \(< (char->integer %1) 3) l1))
(profile-call (oc-take-right-while \(>= (char->integer %1) 3) l1))


(profile-call (oc-sort char<? l3))
(profile-call (oc-sort char>? l3))

(profile-call (oc-sorted? char<=? l3))
(profile-call (oc-sorted? char>=? l3))
(profile-call (oc-sorted? char<=? (oc-sort char<? l3)))
(profile-call (oc-sorted? char>=? (oc-sort char>? l3)))

(profile-call (oc-merge char<? l4 l5))

(profile-call (oc-delete-neighbor-duplicates char=? l3))

; ------------------------------------------------------------------------------------------------------------------------------------------------------
; ------------------------------------------------------------------------------------------------------------------------------------------------------


; (profile-call (oc-conj 99 l1))

; (profile-call (oc-init l1))
; (profile-call (oc-last l1))

; (profile-call (oc-slice l1 1))
; (profile-call (oc-slice l1 0 1))
; (profile-call (oc-slice l1 0 4))
; (profile-call (oc-slice l1 0 \(= %1 2)))
; (profile-call (oc-slice l1 1 \(= %1 2)))
; (profile-call (oc-slice l1 2 \(= %1 2)))

; (profile-call (oc-reverse l1))

; (profile-call (oc-remove-first odd? l1))
; (profile-call (oc-remove-last odd? l1))

; (profile-call (oc-remove-first even? l1))
; (profile-call (oc-remove-last even? l1))

; (profile-call (oc-remove-first \(< %1 4) l1))
; (profile-call (oc-remove-last \(< %1 4) l1))

; (profile-call (oc-skip even? l1))
; (profile-call (oc-skip-right even? l1))

; (profile-call (oc-skip odd? l1))
; (profile-call (oc-skip-right odd? l1))

; (profile-call (oc-fold-right - 0 l1 l2))

; (profile-call (oc-key-right odd? l1))
; (profile-call (oc-key-right even? l1))

; (profile-call (oc-drop-right l1 0))
; (profile-call (oc-drop-right l1 1))
; (profile-call (oc-drop-right l1 2))
; (profile-call (oc-drop-right l1 3))
; (profile-call (oc-drop-right l1 4))

; (profile-call (oc-drop-while odd? l1))
; (profile-call (oc-drop-while even? l1))
; (profile-call (oc-drop-while \(< %1 3) l1))
; (profile-call (oc-drop-while \(>= %1 3) l1))

; (profile-call (oc-drop-right-while odd? l1))
; (profile-call (oc-drop-right-while even? l1))
; (profile-call (oc-drop-right-while \(< %1 3) l1))
; (profile-call (oc-drop-right-while \(>= %1 3) l1))

; (profile-call (oc-take-right l1 0))
; (profile-call (oc-take-right l1 1))
; (profile-call (oc-take-right l1 2))
; (profile-call (oc-take-right l1 3))
; (profile-call (oc-take-right l1 4))

; (profile-call (oc-take-while odd? l1))
; (profile-call (oc-take-while even? l1))
; (profile-call (oc-take-while \(< %1 3) l1))
; (profile-call (oc-take-while \(>= %1 3) l1))

; (profile-call (oc-take-right-while odd? l1))
; (profile-call (oc-take-right-while even? l1))
; (profile-call (oc-take-right-while \(< %1 3) l1))
; (profile-call (oc-take-right-while \(>= %1 3) l1))


; (profile-call (oc-sort < l3))
; (profile-call (oc-sort > l3))

; (profile-call (oc-sorted? <= l3))
; (profile-call (oc-sorted? >= l3))
; (profile-call (oc-sorted? <= (oc-sort < l3)))
; (profile-call (oc-sorted? >= (oc-sort > l3)))

; (profile-call (oc-merge < l4 l5))

; (profile-call (oc-delete-neighbor-duplicates = l3))

; ------------------------------------------------------------------------------------------------------------------------------------------------------
; ------------------------------------------------------------------------------------------------------------------------------------------------------

; (profile-call (ac-head l1))
; (profile-call (ac-tail l1))

; (profile-call (ac-empty? l1))
; (profile-call (ac-empty? {}))
; (profile-call (ac-length l1))
; (profile-call (ac-length {}))
; (profile-call (ac-length+ l1))
; (profile-call (ac-length+ {}))
; (profile-call (ac-length+ '(1 2 . 3)))

; (profile-call (ac-fold \(stringf "%a%a%a" %1 %2 %3) {} l1 l2))

; (profile-call (ac-map \(* %1 %2) l1 l2))
; (profile-call (ac-for-each \(displayf "%wa, %wa\n" %1 %2) l1 l2))
; (profile-call (ac-filter \(even? %1) l2))

; (profile-call (ac-count \(even? %1) l2))
; (profile-call (ac-remove \(even? %1) l2))

; (profile-call (ac-val l1 :a))

; (profile-call (ac-key \(= %1 5) l2))

; (profile-call (ac-append {} l1 {} l2 {}))

; (profile-call (ac-delete l1 :c))

; (profile-call (ac-conj :z 99 l2))

; (profile-call (ac-take l1 1))
; (profile-call (ac-drop l1 1))

; (profile-call (ac-ac->list l1))
; (profile-call (ac-ac->string l1))
; (profile-call (ac-ac->vector l1))
; (profile-call (ac-ac->hashmap l1))


#eof
































(define-class Rectangle
  ((new w l)
    (define self.w w)
    (define self.l l))
  ((area)
    (* self.w self.l))
  ((perimeter)
    (* 2 (+ self.w self.l))))

(define-class Square (:extends Rectangle)
  ((new l)
    (super! l l)))








(define-syntax do-n-times
  (lambda (n expr)
    (def count (gensym))
    `(let ((,count 0))
      (while ((< ,count ,n))
          ,expr
          (set! ,count (+ 1 ,count))))))







(define start (epoch-time))





(define-class C
  ((new (val 10))
    (def self.scale val))
  ((->procedure n)
    (* self.scale n)))


(def o (C 88))


(do-n-times 10
  (o 12))













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
;               (newline)
;               )
;             (yield)
;             (loop1 (+ n 1))))))

;   (define-generator (f2)
;     (let loop2 ((n 0))
;       (if (< n *end*)
;           (begin 
;             (dosync 
;               (display (+ n 1000)) 
;               (newline)
;               )
;             (yield)
;             (loop2 (+ n 1))))))

;   (define-generator (f3)
;     (let loop3 ((n 0))
;       (if (< n *end*)
;           (begin 
;             (dosync 
;               (write (number->string n)) 
;               (newline)
;               )
;             (yield)
;             (loop3 (+ n 1))))))


;   (complete-all-generators! (f1) (f2) (f3))

; )


; (parallel fwrap fwrap fwrap)




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