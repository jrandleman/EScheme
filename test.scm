; (define data (cdr (file-read "stdlib.scm")))

; (for-each 
;   (lambda (expr)
;     (write (compile expr))
;     (newline)
;     (newline))
;   data)




(display (substring "[{\n  \"name\": \"number->string\",\n  \"aliases\": [\"number->str\"],\n  \"signatures\": [\"(number->string <num>)\"],\n  \"description\": \"Convert a number to a string.\",\n  \"example\": null\n}, {\n  \"name\": \"string->number\",\n  \"aliases\": [\"str->number\"],\n  \"signatures\": [\"(string->number <str>)\"],\n  \"description\": \"Convert a string to a number.\",\n  \"example\": null\n}, {\n  \"name\": \"keyword->symbol\",\n  \"aliases\": [\"keyword->sym\"],\n  \"signatures\": [\"(keyword->symbol <keyword>)\"],\n  \"description\": \"Convert a keyword to a symbol.\",\n  \"example\": null\n}, {\n  \"name\": \"symbol->keyword\",\n  \"aliases\": [\"sym->keyword\"],\n  \"signatures\": [\"(symbol->keyword <sym>)\"],\n  \"description\": \"Convert a symbol to a keyword.\",\n  \"example\": null\n}, {\n  \"name\": \"string->symbol\",\n  \"aliases\": [\"str->symbol\",\"string->sym\",\"str->sym\"],\n  \"signatures\": [\"(string->symbol <str>)\"],\n  \"description\": \"Convert a string to a symbol.\",\n  \"example\": null\n}, {\n  \"name\": \"symbol->string\",\n  \"aliases\": [\"sym->string\",\"symbol->str\",\"sym->str\"],\n  \"signatures\": [\"(symbol->string <sym>)\"],\n  \"description\": \"Convert a symbol to a string.\",\n  \"example\": null\n}, {\n  \"name\": \"string->keyword\",\n  \"aliases\": [\"str->keyword\"],\n  \"signatures\": [\"(string->keyword <str>)\"],\n  \"description\": \"Convert a string to a keyword.\",\n  \"example\": null\n}, {\n  \"name\": \"keyword->string\",\n  \"aliases\": [\"keyword->str\"],\n  \"signatures\": [\"(keyword->string <keyword>)\"],\n  \"description\": \"Convert a keyword to a string.\",\n  \"example\": null\n}, {\n  \"name\": \"write-to-string\",\n  \"aliases\": [\"write-to-str\"],\n  \"signatures\": [\"(write-to-string <obj>)\"],\n  \"description\": \"Serialize <obj> in machine-readable form.\",\n  \"example\": null\n}, {\n  \"name\": \"display-to-string\",\n  \"aliases\": [\"display-to-str\"],\n  \"signatures\": [\"(display-to-string <obj>)\"],\n  \"description\": \"Serialize <obj> in human-readable form.\",\n  \"example\": null\n}, ]" 1858))



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









