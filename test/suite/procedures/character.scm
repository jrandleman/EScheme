;; Author: Jordan Randleman -- character.scm
;; => Tests for EScheme's primitive functions.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load (path (path-parent #path 2) "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(ut (char? #\a) #t)
(ut (char? ("a" 0)) #t)
(ut (char? 0) #f)

(ut (char-alphabetic? #\a) #t)
(ut (char-alphabetic? #\A) #t)
(ut (char-alphabetic? #\1) #f)
(ut (char-alphabetic? #\newline) #f)

(ut (char-numeric? #\a) #f)
(ut (char-numeric? #\A) #f)
(ut (char-numeric? #\1) #t)
(ut (char-numeric? #\newline) #f)
(ut (char-numeric? #\.) #f)

(ut (char-whitespace? #\a) #f)
(ut (char-whitespace? #\A) #f)
(ut (char-whitespace? #\1) #f)
(ut (char-whitespace? #\newline) #t)
(ut (char-whitespace? #\.) #f)

(ut (char-upper-case? #\A) #t)
(ut (char-upper-case? #\a) #f)

(ut (char-lower-case? #\A) #f)
(ut (char-lower-case? #\a) #t)

(ut (char-alphanumeric? #\a) #t)
(ut (char-alphanumeric? #\A) #t)
(ut (char-alphanumeric? #\1) #t)
(ut (char-alphanumeric? #\newline) #f)
(ut (char-alphanumeric? #\.) #f)

(ut (char-control? #\a) #f)
(ut (char-control? #\A) #f)
(ut (char-control? #\1) #f)
(ut (char-control? #\newline) #t)
(ut (char-control? #\.) #f)

(ut (char-punctuation? #\a) #f)
(ut (char-punctuation? #\A) #f)
(ut (char-punctuation? #\1) #f)
(ut (char-punctuation? #\newline) #f)
(ut (char-punctuation? #\.) #t)

(ut (char-graph? #\a) #t)
(ut (char-graph? #\A) #t)
(ut (char-graph? #\1) #t)
(ut (char-graph? #\newline) #f)
(ut (char-graph? #\space) #f)
(ut (char-graph? #\.) #t)

(ut (char-print? #\a) #t)
(ut (char-print? #\A) #t)
(ut (char-print? #\1) #t)
(ut (char-print? #\newline) #f)
(ut (char-print? #\space) #t)
(ut (char-print? #\.) #t)

(for-each
  (lambda (c)
    (ut (char-xdigit? c) #t))
  "1234567890abcdefABCDEF")
(ut (char-xdigit? #\g) #f)
(ut (char-xdigit? #\G) #f)

(ut (char-upcase #\a) #\A)
(ut (char-downcase #\A) #\a)

(ut (char=? #\a) #t)
(ut (char=? #\a #\a #\a) #t)
(ut (char=? #\a #\A #\a) #f)
(ut (char=? #\b #\a #\a) #f)
(ut (char=? #\a #\b #\a) #f)
(ut (char=? #\a #\a #\b) #f)
(ut (char=? #\a #\B #\c) #f)
(ut (char=? #\c #\B #\a) #f)

(ut (char<? #\a) #t)
(ut (char<? #\a #\a #\a) #f)
(ut (char<? #\a #\A #\a) #f)
(ut (char<? #\a #\b #\c) #t)
(ut (char<? #\b #\a #\c) #f)
(ut (char<? #\b #\c #\a) #f)
(ut (char<? #\a #\B #\c) #f)
(ut (char<? #\c #\B #\a) #f)

(ut (char<=? #\a) #t)
(ut (char<=? #\a #\a #\a) #t)
(ut (char<=? #\a #\A #\a) #f)
(ut (char<=? #\a #\b #\c) #t)
(ut (char<=? #\b #\a #\c) #f)
(ut (char<=? #\b #\c #\a) #f)
(ut (char<=? #\a #\B #\c) #f)
(ut (char<=? #\c #\B #\a) #f)

(ut (char>? #\a) #t)
(ut (char>? #\a #\a #\a) #f)
(ut (char>? #\a #\A #\a) #f)
(ut (char>? #\c #\b #\a) #t)
(ut (char>? #\c #\a #\b) #f)
(ut (char>? #\a #\c #\b) #f)
(ut (char>? #\a #\B #\c) #f)
(ut (char>? #\c #\B #\a) #f)

(ut (char>=? #\a) #t)
(ut (char>=? #\a #\a #\a) #t)
(ut (char>=? #\a #\A #\a) #f)
(ut (char>=? #\c #\b #\a) #t)
(ut (char>=? #\c #\a #\b) #f)
(ut (char>=? #\a #\c #\b) #f)
(ut (char>=? #\a #\B #\c) #f)
(ut (char>=? #\c #\B #\a) #f)

; check numeric comparator aliases
(ut (< #\a #\a #\a) #f)
(ut (< #\a #\A #\a) #f)
(ut (< #\a #\b #\c) #t)
(ut (< #\b #\a #\c) #f)
(ut (< #\b #\c #\a) #f)
(ut (< #\a #\B #\c) #f)
(ut (< #\c #\B #\a) #f)

(ut (<= #\a #\a #\a) #t)
(ut (<= #\a #\A #\a) #f)
(ut (<= #\a #\b #\c) #t)
(ut (<= #\b #\a #\c) #f)
(ut (<= #\b #\c #\a) #f)
(ut (<= #\a #\B #\c) #f)
(ut (<= #\c #\B #\a) #f)

(ut (> #\a #\a #\a) #f)
(ut (> #\a #\A #\a) #f)
(ut (> #\c #\b #\a) #t)
(ut (> #\c #\a #\b) #f)
(ut (> #\a #\c #\b) #f)
(ut (> #\a #\B #\c) #f)
(ut (> #\c #\B #\a) #f)

(ut (>= #\a #\a #\a) #t)
(ut (>= #\a #\A #\a) #f)
(ut (>= #\c #\b #\a) #t)
(ut (>= #\c #\a #\b) #f)
(ut (>= #\a #\c #\b) #f)
(ut (>= #\a #\B #\c) #f)
(ut (>= #\c #\B #\a) #f)

(ut (char-ci=? #\a) #t)
(ut (char-ci=? #\a #\a #\a) #t)
(ut (char-ci=? #\a #\A #\a) #t)
(ut (char-ci=? #\b #\a #\a) #f)
(ut (char-ci=? #\a #\b #\a) #f)
(ut (char-ci=? #\a #\a #\b) #f)
(ut (char-ci=? #\a #\B #\c) #f)
(ut (char-ci=? #\c #\B #\a) #f)

(ut (char-ci<? #\a) #t)
(ut (char-ci<? #\a #\a #\a) #f)
(ut (char-ci<? #\a #\A #\a) #f)
(ut (char-ci<? #\a #\b #\c) #t)
(ut (char-ci<? #\b #\a #\c) #f)
(ut (char-ci<? #\b #\c #\a) #f)
(ut (char-ci<? #\a #\B #\c) #t)
(ut (char-ci<? #\c #\B #\a) #f)

(ut (char-ci<=? #\a) #t)
(ut (char-ci<=? #\a #\a #\a) #t)
(ut (char-ci<=? #\a #\A #\a) #t)
(ut (char-ci<=? #\a #\b #\c) #t)
(ut (char-ci<=? #\b #\a #\c) #f)
(ut (char-ci<=? #\b #\c #\a) #f)
(ut (char-ci<=? #\a #\B #\c) #t)
(ut (char-ci<=? #\c #\B #\a) #f)

(ut (char-ci>? #\a) #t)
(ut (char-ci>? #\a #\a #\a) #f)
(ut (char-ci>? #\a #\A #\a) #f)
(ut (char-ci>? #\c #\b #\a) #t)
(ut (char-ci>? #\c #\a #\b) #f)
(ut (char-ci>? #\a #\c #\b) #f)
(ut (char-ci>? #\a #\B #\c) #f)
(ut (char-ci>? #\c #\B #\a) #t)

(ut (char-ci>=? #\a) #t)
(ut (char-ci>=? #\a #\a #\a) #t)
(ut (char-ci>=? #\a #\A #\a) #t)
(ut (char-ci>=? #\c #\b #\a) #t)
(ut (char-ci>=? #\c #\a #\b) #f)
(ut (char-ci>=? #\a #\c #\b) #f)
(ut (char-ci>=? #\a #\B #\c) #f)
(ut (char-ci>=? #\c #\B #\a) #t)

(ut (char-pair? #\a) #f)
(ut (char-pair? #\U1F525) #t) ; fire emoji

(ut (java-char? #\a) #t)
(ut (java-char? #\U1F525) #f)

(ut (ascii-char? #\a) #t)
(ut (ascii-char? #\U1F525) #f)

(ut (char-count #\a) 1)
(ut (char-count #\U1F525) 2)

(define eval-string (compose eval car read-string))
(define (check-char-digits digits)
  (define n (min *max-radix* (length digits)))
  (do ((i 0 (+ i 1)))
      ((>= i n))
      (let ((d (digits i)) (i+1 (+ i 1)))
        (eval-string (stringf "(ut (char-digit #\\%a %n) %n)\n" d (if (< i+1 *min-radix*) *min-radix* i+1) i)))))
(check-char-digits "0123456789abcdefghijklmnopqrstuvwxyz")
(check-char-digits "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(define eval-string (compose eval car read-string))
(define (check-char-for-digit digits)
  (define n (min *max-radix* (length digits)))
  (do ((i 0 (+ i 1)))
      ((>= i n))
      (let ((d (digits i)) (i+1 (+ i 1)))
        (eval-string (stringf "(ut (char-for-digit %a %n) %wa)\n" i (if (< i+1 *min-radix*) *min-radix* i+1) (char-downcase d))))))
(check-char-for-digit "0123456789abcdefghijklmnopqrstuvwxyz")
(check-char-for-digit "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(ut (string? (char-name #\a)) #t)

(ut (char-defined? #\a) #t)

(ut (char-high? (char-high #\U1F525)) #t)
(ut (char-high? (char-low #\U1F525)) #f)

(ut (char-low? (char-high #\U1F525)) #f)
(ut (char-low? (char-low #\U1F525)) #t)

(ut (char-codepoint (char-high #\U1F525) (char-low #\U1F525)) #\U1F525)
