;; Author: Jordan Randleman -- reader.scm
;; => Tests for EScheme's reader literal syntax & VM-specific operations
;;    Invoked by ../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load #path (append ".." *file-separator* "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(ut (boolean? #t) #t)
(ut (boolean? #f) #t)
(ut (string? #path) #t)
(ut (void? #void) #t)
(ut (eof? #eof) #t)
(ut (null? #nil) #t)

(ut ((lambda (. xs) xs) 1 2 3) '(1 2 3)) ; '(. xs) becomes 'xs

(ut (null? ()) #t) ; quote is optional for #nil
(ut (null? '()) #t)
(ut (list? '(1)) #t)
(ut (list? '(1 2)) #t)
(ut (pair? '(1 2)) #t)
(ut (pair? '(1 . 2)) #t)
(ut (list? '(1 . 2)) #f)
(ut (pair? '(1 2 . ())) #t)
(ut (list? '(1 2 . ())) #t)
(ut (list? '(1 . (2 . ()))) #t)

(ut (vector? []) #t)
(ut (vector? [1]) #t)
(ut (vector? [1 2]) #t)

(ut (hashmap? {}) #t)
(ut (hashmap? {1 2}) #t)
(ut (hashmap? {1 2 3 4}) #t)

(ut (\%1 1) 1)
(ut (\%2 1 2) 2)
(ut (\%% 1 2 3) '(1 2 3))
(ut (\(+ 5 %1) 1) 6)

(ut ''1 '(quote 1))
(ut '`1 '(quasiquote 1))
(ut ',1 '(unquote 1))
(ut ',@1 '(unquote-splicing 1))

(ut (char? #\a) #t)
(ut (char? #\1) #t)
(ut (char? #\u0000) #t) ; java unicode syntax
(ut (char? #\U00000000) #t) ; extended unicode syntax
(ut (char? #\space) #t)
(ut (char? #\tab) #t)
(ut (char? #\newline) #t)
(ut (char? #\page) #t)
(ut (char? #\return) #t)
(ut (char? #\backspace) #t)
(ut (char? #\nul) #t)
(ut (char? #\esc) #t)
(ut (char? #\delete) #t)

(ut (string? "") #t)
(ut (string? "a") #t)
(ut (string? "\"") #t)
(ut (string? "\n\t\f\r\b") #t)
(ut (string? "\\") #t)
(ut (string? "\u0041") #t)
(ut (string? "\U0001F525") #t) ; extended unicode syntax
(ut (string? "\U0001f525") #t) ; extended unicode syntax
(ut (string? "\0") #t)
(ut (string? "\177777") #t) ; extended octal syntax

(ut (keyword? :a) #t)
(ut (keyword? :abc) #t)

(ut (number? 0) #t)
(ut (number? 0.0) #t)
(ut (number? Infinity) #t)
(ut (number? -Infinity) #t)
(ut (number? NaN) #t)
(ut (number? 1) #t)
(ut (number? 4) #t)
(ut (number? -1) #t)
(ut (number? -4) #t)
(ut (number? 1.0) #t)
(ut (number? 4.0) #t)
(ut (number? -1.0) #t)
(ut (number? -4.0) #t)
(ut (number? 1/2) #t)
(ut (number? 1/8) #t)
(ut (number? -1/2) #t)
(ut (number? -1/8) #t)
(ut (number? +i) #t)
(ut (number? -i) #t)
(ut (number? 0+2i) #t)
(ut (number? 0.0+2.0i) #t)
(ut (number? 0+1/2i) #t)
(ut (number? 0-2i) #t)
(ut (number? 0.0-2.0i) #t)
(ut (number? 0-1/2i) #t)
(ut (number? 0.0+Infinityi) #t)
(ut (number? 0.0-Infinityi) #t)
(ut (number? 1+4i) #t)
(ut (number? 2+8i) #t)
(ut (number? 1-4i) #t)
(ut (number? 2-8i) #t)
(ut (number? -1+4i) #t)
(ut (number? -2+8i) #t)
(ut (number? -1-4i) #t)
(ut (number? -2-8i) #t)
(ut (number? 1.0+4.0i) #t)
(ut (number? 2.0+8.0i) #t)
(ut (number? 1.0-4.0i) #t)
(ut (number? 2.0-8.0i) #t)
(ut (number? -1.0+4.0i) #t)
(ut (number? -2.0+8.0i) #t)
(ut (number? -1.0-4.0i) #t)
(ut (number? -2.0-8.0i) #t)
(ut (number? 1/2+3/4i) #t)
(ut (number? 3/8+5/8i) #t)
(ut (number? 1/2-3/4i) #t)
(ut (number? 3/8-5/8i) #t)
(ut (number? -1/2+3/4i) #t)
(ut (number? -3/8+5/8i) #t)
(ut (number? -1/2-3/4i) #t)
(ut (number? -3/8-5/8i) #t)
(ut (number? Infinity+1.0i) #t)
(ut (number? Infinity+4.0i) #t)
(ut (number? Infinity-1.0i) #t)
(ut (number? Infinity-4.0i) #t)
(ut (number? -Infinity+1.0i) #t)
(ut (number? -Infinity+4.0i) #t)
(ut (number? -Infinity-1.0i) #t)
(ut (number? -Infinity-4.0i) #t)
(ut (number? 1.0+Infinityi) #t)
(ut (number? 4.0+Infinityi) #t)
(ut (number? 1.0-Infinityi) #t)
(ut (number? 4.0-Infinityi) #t)
(ut (number? -1.0+Infinityi) #t)
(ut (number? -4.0+Infinityi) #t)
(ut (number? -1.0-Infinityi) #t)
(ut (number? -4.0-Infinityi) #t)
(ut (number? Infinity+Infinityi) #t)
(ut (number? -Infinity+Infinityi) #t)
(ut (number? Infinity-Infinityi) #t)
(ut (number? -Infinity-Infinityi) #t)

(ut (symbol? 'a) #t)
(ut (symbol? 'ab) #t)

; check callability
(ut ("abc" 0) #\a)
(ut ("abc" 1) #\b)
(ut ("abc" 2) #\c)
(ut ('[a b c] 0) 'a)
(ut ('[a b c] 1) 'b)
(ut ('[a b c] 2) 'c)
(ut ('{0 a 1 b 2 c} 0) 'a)
(ut ('{0 a 1 b 2 c} 1) 'b)
(ut ('{0 a 1 b 2 c} 2) 'c)

(define-interface I)
(define-class C ((->procedure) 0))
(define c (C))
(define c (C))
(ut (C? c) #t)
(ut (c) 0)

; check dot-notation
(ut I.name 'I)
(ut C.name 'C)
(ut c.class C)

(import examples.specials.module2)
(ut module2.v [])