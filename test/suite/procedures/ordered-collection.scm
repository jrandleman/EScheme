;; Author: Jordan Randleman -- ordered-collection.scm
;; => Tests for EScheme's primitive functions.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load (path (path-parent #path 2) "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(ut (ordered-collection? "") #t)
(ut (ordered-collection? []) #t)
(ut (ordered-collection? '()) #t)
(ut (ordered-collection? {}) #f)
(ut (ordered-collection? 0) #f)

(ut (oc? "") #t)
(ut (oc? []) #t)
(ut (oc? '()) #t)
(ut (oc? {}) #f)
(ut (oc? 0) #f)

; Chop #\U0001000X to just #\X
(define chop-char-high
  (let ((zero-val (char->integer #\0)))
    (lambda (c) 
      (if (java-char? c) 
          c 
          (integer->char (+ zero-val (- (char->integer c) #x10000)))))))

(ut (conj 1 []) [1])
(ut (conj 1 '()) '(1))
(ut (conj #\1 "") "1")
(ut (conj #\U00010001 "") "\U00010001")
(ut (conj 1 [2 4 6]) [2 4 6 1])
(ut (conj 1 '(2 4 6)) '(1 2 4 6))
(ut (conj #\1 "246") "2461")
(ut (conj #\1 "\U000100024\U00010006") "\U000100024\U000100061")
(ut (conj #\1 "2\U000100046") "2\U0001000461")
(ut (conj #\1 "\U00010002\U00010004\U00010006") "\U00010002\U00010004\U000100061")
(ut (conj #\U00010001 "\U000100024\U00010006") "\U000100024\U00010006\U00010001")
(ut (conj #\U00010001 "2\U000100046") "2\U000100046\U00010001")
(ut (conj #\U00010001 "\U00010002\U00010004\U00010006") "\U00010002\U00010004\U00010006\U00010001")

(ut (init [1]) [])
(ut (init '(1)) '())
(ut (init "1") "")
(ut (init "\U00010001") "")
(ut (init [1 2]) [1])
(ut (init '(1 2)) '(1))
(ut (init "12") "1")
(ut (init "\U000100012") "\U00010001")
(ut (init "1\U00010002") "1")
(ut (init "\U00010001\U00010002") "\U00010001")
(ut (init [1 2 3]) [1 2])
(ut (init '(1 2 3)) '(1 2))
(ut (init "123") "12")
(ut (init "\U0001000123") "\U000100012")
(ut (init "1\U000100023") "1\U00010002")
(ut (init "\U00010001\U000100023") "\U00010001\U00010002")
(ut (init "12\U00010003") "12")
(ut (init "\U000100012\U00010003") "\U000100012")
(ut (init "1\U00010002\U00010003") "1\U00010002")
(ut (init "\U00010001\U00010002\U00010003") "\U00010001\U00010002")

(ut (last [1]) 1)
(ut (last '(1)) 1)
(ut (last "1") #\1)
(ut (last "\U00010001") #\U00010001)
(ut (last [1 2]) 2)
(ut (last '(1 2)) 2)
(ut (last "12") #\2)
(ut (last "\U000100012") #\2)
(ut (last "1\U00010002") #\U00010002)
(ut (last "\U00010001\U00010002") #\U00010002)
(ut (last [1 2 3]) 3)
(ut (last '(1 2 3)) 3)
(ut (last "123") #\3)
(ut (last "\U0001000123") #\3)
(ut (last "1\U000100023") #\3)
(ut (last "\U00010001\U000100023") #\3)
(ut (last "\U000100012\U00010003") #\U00010003)
(ut (last "1\U00010002\U00010003") #\U00010003)
(ut (last "\U00010001\U00010002\U00010003") #\U00010003)

(ut (slice "" 1) "")
(ut (slice "" 0 0) "")
(ut (slice "" 1 0) "")
(ut (slice "12345" 1 0) "")
(ut (slice "12345" 1 1) "2")
(ut (slice "12345" 0 100) "12345")
(ut (slice "12345" 0 (lambda (x) #t)) "12345")
(ut (slice "12345" 0 (lambda (x) #f)) "")
(ut (slice "12345" 0 (lambda (x) (char<=? x #\3))) "123")
(ut (slice "12345" 0) "12345")
(ut (slice "12345" 1) "2345")
(ut (slice "12345" 2) "345")
(ut (slice "12345" 5) "")
(ut (slice "1\U000100023\U000100045" 1 0) "")
(ut (slice "1\U000100023\U000100045" 1 1) "\U00010002")
(ut (slice "1\U000100023\U000100045" 0 100) "1\U000100023\U000100045")
(ut (slice "1\U000100023\U000100045" 0 (lambda (x) #t)) "1\U000100023\U000100045")
(ut (slice "1\U000100023\U000100045" 0 (lambda (x) #f)) "")
(ut (slice "1\U000100023\U000100045" 0 (lambda (x) (char<=? (chop-char-high x) #\3))) "1\U000100023")
(ut (slice "1\U000100023\U000100045" 0) "1\U000100023\U000100045")
(ut (slice "1\U000100023\U000100045" 1) "\U000100023\U000100045")
(ut (slice "1\U000100023\U000100045" 2) "3\U000100045")
(ut (slice "1\U000100023\U000100045" 5) "")
(ut (slice "\U000100012\U000100034\U00010005" 1 0) "")
(ut (slice "\U000100012\U000100034\U00010005" 1 1) "2")
(ut (slice "\U000100012\U000100034\U00010005" 0 100) "\U000100012\U000100034\U00010005")
(ut (slice "\U000100012\U000100034\U00010005" 0 (lambda (x) #t)) "\U000100012\U000100034\U00010005")
(ut (slice "\U000100012\U000100034\U00010005" 0 (lambda (x) #f)) "")
(ut (slice "\U000100012\U000100034\U00010005" 0 (lambda (x) (char<=? (chop-char-high x) #\3))) "\U000100012\U00010003")
(ut (slice "\U000100012\U000100034\U00010005" 0) "\U000100012\U000100034\U00010005")
(ut (slice "\U000100012\U000100034\U00010005" 1) "2\U000100034\U00010005")
(ut (slice "\U000100012\U000100034\U00010005" 2) "\U000100034\U00010005")
(ut (slice "\U000100012\U000100034\U00010005" 5) "")
(ut (slice "\U00010001\U00010002\U00010003\U00010004\U00010005" 1 0) "")
(ut (slice "\U00010001\U00010002\U00010003\U00010004\U00010005" 1 1) "\U00010002")
(ut (slice "\U00010001\U00010002\U00010003\U00010004\U00010005" 0 100) "\U00010001\U00010002\U00010003\U00010004\U00010005")
(ut (slice "\U00010001\U00010002\U00010003\U00010004\U00010005" 0 (lambda (x) #t)) "\U00010001\U00010002\U00010003\U00010004\U00010005")
(ut (slice "\U00010001\U00010002\U00010003\U00010004\U00010005" 0 (lambda (x) #f)) "")
(ut (slice "\U00010001\U00010002\U00010003\U00010004\U00010005" 0 (lambda (x) (char<=? (chop-char-high x) #\3))) "\U00010001\U00010002\U00010003")
(ut (slice "\U00010001\U00010002\U00010003\U00010004\U00010005" 0) "\U00010001\U00010002\U00010003\U00010004\U00010005")
(ut (slice "\U00010001\U00010002\U00010003\U00010004\U00010005" 1) "\U00010002\U00010003\U00010004\U00010005")
(ut (slice "\U00010001\U00010002\U00010003\U00010004\U00010005" 2) "\U00010003\U00010004\U00010005")
(ut (slice "\U00010001\U00010002\U00010003\U00010004\U00010005" 5) "")

(ut (slice [] 1) [])
(ut (slice [] 0 0) [])
(ut (slice [] 1 0) [])
(ut (slice [1 2 3 4 5] 1 0) [])
(ut (slice [1 2 3 4 5] 1 1) [2])
(ut (slice [1 2 3 4 5] 0 100) [1 2 3 4 5])
(ut (slice [1 2 3 4 5] 0 (lambda (x) #t)) [1 2 3 4 5])
(ut (slice [1 2 3 4 5] 0 (lambda (x) #f)) [])
(ut (slice [1 2 3 4 5] 0 (lambda (x) (<= x 3))) [1 2 3])
(ut (slice [1 2 3 4 5] 0) [1 2 3 4 5])
(ut (slice [1 2 3 4 5] 1) [2 3 4 5])
(ut (slice [1 2 3 4 5] 2) [3 4 5])
(ut (slice [1 2 3 4 5] 5) [])

(ut (slice '() 1) '())
(ut (slice '() 0 0) '())
(ut (slice '() 1 0) '())
(ut (slice '(1 2 3 4 5) 1 0) '())
(ut (slice '(1 2 3 4 5) 1 1) '(2))
(ut (slice '(1 2 3 4 5) 0 100) '(1 2 3 4 5))
(ut (slice '(1 2 3 4 5) 0 (lambda (x) #t)) '(1 2 3 4 5))
(ut (slice '(1 2 3 4 5) 0 (lambda (x) #f)) '())
(ut (slice '(1 2 3 4 5) 0 (lambda (x) (<= x 3))) '(1 2 3))
(ut (slice '(1 2 3 4 5) 0) '(1 2 3 4 5))
(ut (slice '(1 2 3 4 5) 1) '(2 3 4 5))
(ut (slice '(1 2 3 4 5) 2) '(3 4 5))
(ut (slice '(1 2 3 4 5) 5) '())

(ut (reverse '()) '())
(ut (reverse []) [])
(ut (reverse "") "")
(ut (reverse '(1)) '(1))
(ut (reverse [1]) [1])
(ut (reverse "1") "1")
(ut (reverse "\U00010001") "\U00010001")
(ut (reverse '(1 2)) '(2 1))
(ut (reverse [1 2]) [2 1])
(ut (reverse "12") "21")
(ut (reverse "1\U00010002") "\U000100021")
(ut (reverse "\U000100012") "2\U00010001")
(ut (reverse "\U00010001\U00010002") "\U00010002\U00010001")
(ut (reverse '(1 2 3 4 5)) '(5 4 3 2 1))
(ut (reverse [1 2 3 4 5]) [5 4 3 2 1])
(ut (reverse "12345") "54321")
(ut (reverse "1\U000100023\U000100045") "5\U000100043\U000100021")
(ut (reverse "\U000100012\U000100034\U00010005") "\U000100054\U000100032\U00010001")
(ut (reverse "\U00010001\U00010002\U00010003\U00010004\U00010005") "\U00010005\U00010004\U00010003\U00010002\U00010001")

(define char->int
  (let ((zero-val (char->integer #\0))) 
    (lambda (ch) 
      (- (char->integer (chop-char-high ch)) zero-val))))

(define even-char? (compose even? char->int))

(define odd-char? (compose odd? char->int))

(ut (remove-first even? []) [])
(ut (remove-first even? '()) '())
(ut (remove-first even-char? "") "")
(ut (remove-first even? [1]) [1])
(ut (remove-first even? '(1)) '(1))
(ut (remove-first even-char? "1") "1")
(ut (remove-first even-char? "\U00010001") "\U00010001")
(ut (remove-first even? [2]) [])
(ut (remove-first even? '(2)) '())
(ut (remove-first even-char? "2") "")
(ut (remove-first even-char? "\U00010002") "")
(ut (remove-first even? [2 4]) [4])
(ut (remove-first even? '(2 4)) '(4))
(ut (remove-first even-char? "24") "4")
(ut (remove-first even-char? "\U000100024") "4")
(ut (remove-first even-char? "2\U00010004") "\U00010004")
(ut (remove-first even-char? "\U00010002\U00010004") "\U00010004")
(ut (remove-first even? [1 2]) [1])
(ut (remove-first even? '(1 2)) '(1))
(ut (remove-first even-char? "12") "1")
(ut (remove-first even-char? "\U000100012") "\U00010001")
(ut (remove-first even-char? "1\U00010002") "1")
(ut (remove-first even-char? "\U00010001\U00010002") "\U00010001")
(ut (remove-first even? [2 1 4]) [1 4])
(ut (remove-first even? '(2 1 4)) '(1 4))
(ut (remove-first even-char? "214") "14")
(ut (remove-first even-char? "\U000100021\U00010004") "1\U00010004")
(ut (remove-first even-char? "2\U000100014") "\U000100014")
(ut (remove-first even-char? "\U00010002\U00010001\U00010004") "\U00010001\U00010004")
(ut (remove-first odd? [2 1 4]) [2 4])
(ut (remove-first odd? '(2 1 4)) '(2 4))
(ut (remove-first odd-char? "214") "24")
(ut (remove-first odd-char? "\U000100021\U00010004") "\U00010002\U00010004")
(ut (remove-first odd-char? "2\U000100014") "24")
(ut (remove-first odd-char? "\U00010002\U00010001\U00010004") "\U00010002\U00010004")

(ut (remove-last even? []) [])
(ut (remove-last even? '()) '())
(ut (remove-last even-char? "") "")
(ut (remove-last even? [1]) [1])
(ut (remove-last even? '(1)) '(1))
(ut (remove-last even-char? "1") "1")
(ut (remove-last even-char? "\U00010001") "\U00010001")
(ut (remove-last even? [2]) [])
(ut (remove-last even? '(2)) '())
(ut (remove-last even-char? "2") "")
(ut (remove-last even-char? "\U00010002") "")
(ut (remove-last even? [2 4]) [2])
(ut (remove-last even? '(2 4)) '(2))
(ut (remove-last even-char? "24") "2")
(ut (remove-last even-char? "\U000100024") "\U00010002")
(ut (remove-last even-char? "2\U00010004") "2")
(ut (remove-last even-char? "\U00010002\U00010004") "\U00010002")
(ut (remove-last even? [1 2]) [1])
(ut (remove-last even? '(1 2)) '(1))
(ut (remove-last even-char? "12") "1")
(ut (remove-last even-char? "\U000100012") "\U00010001")
(ut (remove-last even-char? "1\U00010002") "1")
(ut (remove-last even-char? "\U00010001\U00010002") "\U00010001")
(ut (remove-last even? [2 1 4]) [2 1])
(ut (remove-last even? '(2 1 4)) '(2 1))
(ut (remove-last even-char? "214") "21")
(ut (remove-last even-char? "\U000100021\U00010004") "\U000100021")
(ut (remove-last even-char? "2\U000100014") "2\U00010001")
(ut (remove-last even-char? "\U00010002\U00010001\U00010004") "\U00010002\U00010001")
(ut (remove-last odd? [2 1 4]) [2 4])
(ut (remove-last odd? '(2 1 4)) '(2 4))
(ut (remove-last odd-char? "214") "24")
(ut (remove-last odd-char? "\U000100021\U00010004") "\U00010002\U00010004")
(ut (remove-last odd-char? "2\U000100014") "24")
(ut (remove-last odd-char? "\U00010002\U00010001\U00010004") "\U00010002\U00010004")

(ut (skip even? []) #f)
(ut (skip even? [2]) #f)
(ut (skip even? [1]) 1)
(ut (skip even? [2 4]) #f)
(ut (skip even? [2 4 1]) 1)
(ut (skip even? [1 2 4]) 1)
(ut (skip even? [1 2 4 3]) 1)
(ut (skip even? '()) #f)
(ut (skip even? '(2)) #f)
(ut (skip even? '(1)) 1)
(ut (skip even? '(2 4)) #f)
(ut (skip even? '(2 4 1)) 1)
(ut (skip even? '(1 2 4)) 1)
(ut (skip even? '(1 2 4 3)) 1)
(ut (skip even-char? "") #f)
(ut (skip even-char? "2") #f)
(ut (skip even-char? "\U00010002") #f)
(ut (skip even-char? "1") #\1)
(ut (skip even-char? "\U00010001") #\U00010001)
(ut (skip even-char? "24") #f)
(ut (skip even-char? "\U000100024") #f)
(ut (skip even-char? "2\U00010004") #f)
(ut (skip even-char? "\U00010002\U00010004") #f)
(ut (skip even-char? "241") #\1)
(ut (skip even-char? "\U000100024\U00010001") #\U00010001)
(ut (skip even-char? "2\U000100041") #\1)
(ut (skip even-char? "\U00010002\U00010004\U00010001") #\U00010001)
(ut (skip even-char? "124") #\1)
(ut (skip even-char? "\U000100012\U00010004") #\U00010001)
(ut (skip even-char? "1\U000100024") #\1)
(ut (skip even-char? "\U00010001\U00010002\U00010004") #\U00010001)
(ut (skip even-char? "1243") #\1)
(ut (skip even-char? "\U000100012\U000100043") #\U00010001)
(ut (skip even-char? "1\U000100024\U00010003") #\1)
(ut (skip even-char? "\U00010001\U00010002\U00010004\U00010003") #\U00010001)

(ut (skip-right even? []) #f)
(ut (skip-right even? [2]) #f)
(ut (skip-right even? [1]) 1)
(ut (skip-right even? [2 4]) #f)
(ut (skip-right even? [2 4 1]) 1)
(ut (skip-right even? [1 2 4]) 1)
(ut (skip-right even? [1 2 4 3]) 3)
(ut (skip-right even? '()) #f)
(ut (skip-right even? '(2)) #f)
(ut (skip-right even? '(1)) 1)
(ut (skip-right even? '(2 4)) #f)
(ut (skip-right even? '(2 4 1)) 1)
(ut (skip-right even? '(1 2 4)) 1)
(ut (skip-right even? '(1 2 4 3)) 3)
(ut (skip-right even-char? "") #f)
(ut (skip-right even-char? "2") #f)
(ut (skip-right even-char? "\U00010002") #f)
(ut (skip-right even-char? "1") #\1)
(ut (skip-right even-char? "\U00010001") #\U00010001)
(ut (skip-right even-char? "24") #f)
(ut (skip-right even-char? "\U000100024") #f)
(ut (skip-right even-char? "2\U00010004") #f)
(ut (skip-right even-char? "\U00010002\U00010004") #f)
(ut (skip-right even-char? "241") #\1)
(ut (skip-right even-char? "\U000100024\U00010001") #\U00010001)
(ut (skip-right even-char? "2\U000100041") #\1)
(ut (skip-right even-char? "\U00010002\U00010004\U00010001") #\U00010001)
(ut (skip-right even-char? "124") #\1)
(ut (skip-right even-char? "\U000100012\U00010004") #\U00010001)
(ut (skip-right even-char? "1\U000100024") #\1)
(ut (skip-right even-char? "\U00010001\U00010002\U00010004") #\U00010001)
(ut (skip-right even-char? "1243") #\3)
(ut (skip-right even-char? "\U000100012\U000100043") #\3)
(ut (skip-right even-char? "1\U000100024\U00010003") #\U00010003)
(ut (skip-right even-char? "\U00010001\U00010002\U00010004\U00010003") #\U00010003)

(define (char- c n) (- (char->int c) n))
(ut (fold-right - 0 []) 0)
(ut (fold-right - 0 [3]) 3)
(ut (fold-right - 0 [2 3]) -1)
(ut (fold-right - 0 [1 2 3]) 2)
(ut (fold-right - 0 '()) 0)
(ut (fold-right - 0 '(3)) 3)
(ut (fold-right - 0 '(2 3)) -1)
(ut (fold-right - 0 '(1 2 3)) 2)
(ut (fold-right char- 0 "") 0)
(ut (fold-right char- 0 "3") 3)
(ut (fold-right char- 0 "\U00010003") 3)
(ut (fold-right char- 0 "23") -1)
(ut (fold-right char- 0 "\U000100023") -1)
(ut (fold-right char- 0 "2\U00010003") -1)
(ut (fold-right char- 0 "\U00010002\U00010003") -1)
(ut (fold-right char- 0 "123") 2)
(ut (fold-right char- 0 "\U000100012\U00010003") 2)
(ut (fold-right char- 0 "1\U000100023") 2)
(ut (fold-right char- 0 "\U00010001\U00010002\U00010003") 2)

(define (char-- k c n) (- (char->int k) (char->int c) n))
(ut (fold-right - 0 [] []) 0)
(ut (fold-right - 0 [3] [3]) 0)
(ut (fold-right - 0 [2 3] [2 3]) 0)
(ut (fold-right - 0 [1 2 3] [1 2 3]) 0)
(ut (fold-right - 0 '() '()) 0)
(ut (fold-right - 0 '(3) '(3)) 0)
(ut (fold-right - 0 '(2 3) '(2 3)) 0)
(ut (fold-right - 0 '(1 2 3) '(1 2 3)) 0)
(ut (fold-right char-- 0 "" "") 0)
(ut (fold-right char-- 0 "3" "3") 0)
(ut (fold-right char-- 0 "\U00010003" "\U00010003") 0)
(ut (fold-right char-- 0 "23" "23") 0)
(ut (fold-right char-- 0 "\U000100023" "\U000100023") 0)
(ut (fold-right char-- 0 "2\U00010003" "2\U00010003") 0)
(ut (fold-right char-- 0 "\U00010002\U00010003" "\U00010002\U00010003") 0)
(ut (fold-right char-- 0 "123" "123") 0)
(ut (fold-right char-- 0 "\U000100012\U00010003" "\U000100012\U00010003") 0)
(ut (fold-right char-- 0 "1\U000100023" "1\U000100023") 0)
(ut (fold-right char-- 0 "\U00010001\U00010002\U00010003" "\U00010001\U00010002\U00010003") 0)

(define (char++ k c n) (+ (char->int k) (char->int c) n))
(ut (fold-right + 0 [] []) 0)
(ut (fold-right + 0 [3] [3]) 6)
(ut (fold-right + 0 [2 3] [2 3]) 10)
(ut (fold-right + 0 [1 2 3] [1 2 3]) 12)
(ut (fold-right + 0 '() '()) 0)
(ut (fold-right + 0 '(3) '(3)) 6)
(ut (fold-right + 0 '(2 3) '(2 3)) 10)
(ut (fold-right + 0 '(1 2 3) '(1 2 3)) 12)
(ut (fold-right char++ 0 "" "") 0)
(ut (fold-right char++ 0 "3" "3") 6)
(ut (fold-right char++ 0 "\U00010003" "\U00010003") 6)
(ut (fold-right char++ 0 "23" "23") 10)
(ut (fold-right char++ 0 "\U000100023" "\U000100023") 10)
(ut (fold-right char++ 0 "2\U00010003" "2\U00010003") 10)
(ut (fold-right char++ 0 "\U00010002\U00010003" "\U00010002\U00010003") 10)
(ut (fold-right char++ 0 "123" "123") 12)
(ut (fold-right char++ 0 "\U000100012\U00010003" "\U000100012\U00010003") 12)
(ut (fold-right char++ 0 "1\U000100023" "1\U000100023") 12)
(ut (fold-right char++ 0 "\U00010001\U00010002\U00010003" "\U00010001\U00010002\U00010003") 12)

(ut (key-right even? '(2)) 0)
(ut (key-right even? '(2 4)) 1)
(ut (key-right even? '(1 2 3 4 5)) 3)

(ut (drop-right [] 100) [])
(ut (drop-right '() 100) '())
(ut (drop-right "" 100) "")
(ut (drop-right [1] 100) [])
(ut (drop-right '(1) 100) '())
(ut (drop-right "1" 100) "")
(ut (drop-right "\U00010001" 100) "")
(ut (drop-right [1 2 3 4 5] 3) [1 2])
(ut (drop-right '(1 2 3 4 5) 3) '(1 2))
(ut (drop-right "12345" 3) "12")
(ut (drop-right "\U000100012\U000100034\U00010005" 3) "\U000100012")
(ut (drop-right "1\U000100023\U000100045" 3) "1\U00010002")
(ut (drop-right "\U00010001\U00010002\U00010003\U00010004\U00010005" 3) "\U00010001\U00010002")

(ut (drop-while (lambda (x) #t) []) [])
(ut (drop-while (lambda (x) #t) '()) '())
(ut (drop-while (lambda (x) #t) "") "")
(ut (drop-while (lambda (x) #t) [1 2 3]) [])1
(ut (drop-while (lambda (x) #t) '(1 2 3)) '())
(ut (drop-while (lambda (x) #t) "123") "")
(ut (drop-while (lambda (x) #t) "\U000100012\U00010003") "")
(ut (drop-while (lambda (x) #t) "1\U000100023") "")
(ut (drop-while (lambda (x) #t) "\U00010001\U00010002\U00010003") "")
(ut (drop-while (lambda (x) #f) []) [])
(ut (drop-while (lambda (x) #f) '()) '())
(ut (drop-while (lambda (x) #f) "") "")
(ut (drop-while (lambda (x) #f) [1 2 3]) [1 2 3])
(ut (drop-while (lambda (x) #f) '(1 2 3)) '(1 2 3))
(ut (drop-while (lambda (x) #f) "123") "123")
(ut (drop-while (lambda (x) #f) "\U000100012\U00010003") "\U000100012\U00010003")
(ut (drop-while (lambda (x) #f) "1\U000100023") "1\U000100023")
(ut (drop-while (lambda (x) #f) "\U00010001\U00010002\U00010003") "\U00010001\U00010002\U00010003")
(ut (drop-while (bind > 3) [1 2 3]) [3])
(ut (drop-while (bind > 3) '(1 2 3)) '(3))
(ut (drop-while (bind char>? #\3) "123") "3")
(ut (drop-while (compose (bind char>? #\3) chop-char-high) "\U000100012\U00010003") "\U00010003")
(ut (drop-while (compose (bind char>? #\3) chop-char-high) "1\U000100023") "3")
(ut (drop-while (compose (bind char>? #\3) chop-char-high) "\U00010001\U00010002\U00010003") "\U00010003")

(ut (drop-right-while (lambda (x) #t) []) [])
(ut (drop-right-while (lambda (x) #t) '()) '())
(ut (drop-right-while (lambda (x) #t) "") "")
(ut (drop-right-while (lambda (x) #t) [1 2 3]) [])
(ut (drop-right-while (lambda (x) #t) '(1 2 3)) '())
(ut (drop-right-while (lambda (x) #t) "123") "")
(ut (drop-right-while (lambda (x) #t) "\U000100012\U00010003") "")
(ut (drop-right-while (lambda (x) #t) "1\U000100023") "")
(ut (drop-right-while (lambda (x) #t) "\U00010001\U00010002\U00010003") "")
(ut (drop-right-while (lambda (x) #f) []) [])
(ut (drop-right-while (lambda (x) #f) '()) '())
(ut (drop-right-while (lambda (x) #f) "") "")
(ut (drop-right-while (lambda (x) #f) [1 2 3]) [1 2 3])
(ut (drop-right-while (lambda (x) #f) '(1 2 3)) '(1 2 3))
(ut (drop-right-while (lambda (x) #f) "123") "123")
(ut (drop-right-while (lambda (x) #f) "\U000100012\U00010003") "\U000100012\U00010003")
(ut (drop-right-while (lambda (x) #f) "1\U000100023") "1\U000100023")
(ut (drop-right-while (lambda (x) #f) "\U00010001\U00010002\U00010003") "\U00010001\U00010002\U00010003")
(ut (drop-right-while (bind < 1) [1 2 3]) [1])
(ut (drop-right-while (bind < 1) '(1 2 3)) '(1))
(ut (drop-right-while (bind char<? #\1) "123") "1")
(ut (drop-right-while (compose (bind char<? #\1) chop-char-high) "\U000100012\U00010003") "\U00010001")
(ut (drop-right-while (compose (bind char<? #\1) chop-char-high) "1\U000100023") "1")
(ut (drop-right-while (compose (bind char<? #\1) chop-char-high) "\U00010001\U00010002\U00010003") "\U00010001")

(ut (take-right [] 100) [])
(ut (take-right '() 100) '())
(ut (take-right "" 100) "")
(ut (take-right [1] 100) [1])
(ut (take-right '(1) 100) '(1))
(ut (take-right "1" 100) "1")
(ut (take-right "\U00010001" 100) "\U00010001")
(ut (take-right [1 2 3 4 5] 3) [3 4 5])
(ut (take-right '(1 2 3 4 5) 3) '(3 4 5))
(ut (take-right "12345" 3) "345")
(ut (take-right "\U000100012\U000100034\U00010005" 3) "\U000100034\U00010005")
(ut (take-right "1\U000100023\U000100045" 3) "3\U000100045")
(ut (take-right "\U00010001\U00010002\U00010003\U00010004\U00010005" 3) "\U00010003\U00010004\U00010005")

(ut (take-while (lambda (x) #t) []) [])
(ut (take-while (lambda (x) #t) '()) '())
(ut (take-while (lambda (x) #t) "") "")
(ut (take-while (lambda (x) #t) [1 2 3]) [1 2 3])
(ut (take-while (lambda (x) #t) '(1 2 3)) '(1 2 3))
(ut (take-while (lambda (x) #t) "123") "123")
(ut (take-while (lambda (x) #t) "\U000100012\U00010003") "\U000100012\U00010003")
(ut (take-while (lambda (x) #t) "1\U000100023") "1\U000100023")
(ut (take-while (lambda (x) #t) "\U00010001\U00010002\U00010003") "\U00010001\U00010002\U00010003")
(ut (take-while (lambda (x) #f) []) [])
(ut (take-while (lambda (x) #f) '()) '())
(ut (take-while (lambda (x) #f) "") "")
(ut (take-while (lambda (x) #f) [1 2 3]) [])
(ut (take-while (lambda (x) #f) '(1 2 3)) '())
(ut (take-while (lambda (x) #f) "123") "")
(ut (take-while (lambda (x) #f) "\U000100012\U00010003") "")
(ut (take-while (lambda (x) #f) "1\U000100023") "")
(ut (take-while (lambda (x) #f) "\U00010001\U00010002\U00010003") "")
(ut (take-while (bind > 3) [1 2 3]) [1 2])
(ut (take-while (bind > 3) '(1 2 3)) '(1 2))
(ut (take-while (bind char>? #\3) "123") "12")
(ut (take-while (compose (bind char>? #\3) chop-char-high) "\U000100012\U00010003") "\U000100012")
(ut (take-while (compose (bind char>? #\3) chop-char-high) "1\U000100023") "1\U00010002")
(ut (take-while (compose (bind char>? #\3) chop-char-high) "\U00010001\U00010002\U00010003") "\U00010001\U00010002")

(ut (take-right-while (lambda (x) #t) []) [])
(ut (take-right-while (lambda (x) #t) '()) '())
(ut (take-right-while (lambda (x) #t) "") "")
(ut (take-right-while (lambda (x) #t) [1 2 3]) [1 2 3])
(ut (take-right-while (lambda (x) #t) '(1 2 3)) '(1 2 3))
(ut (take-right-while (lambda (x) #t) "123") "123")
(ut (take-right-while (lambda (x) #t) "\U000100012\U00010003") "\U000100012\U00010003")
(ut (take-right-while (lambda (x) #t) "1\U000100023") "1\U000100023")
(ut (take-right-while (lambda (x) #t) "\U00010001\U00010002\U00010003") "\U00010001\U00010002\U00010003")
(ut (take-right-while (lambda (x) #f) []) [])
(ut (take-right-while (lambda (x) #f) '()) '())
(ut (take-right-while (lambda (x) #f) "") "")
(ut (take-right-while (lambda (x) #f) [1 2 3]) [])
(ut (take-right-while (lambda (x) #f) '(1 2 3)) '())
(ut (take-right-while (lambda (x) #f) "123") "")
(ut (take-right-while (lambda (x) #f) "\U000100012\U00010003") "")
(ut (take-right-while (lambda (x) #f) "1\U000100023") "")
(ut (take-right-while (lambda (x) #f) "\U00010001\U00010002\U00010003") "")
(ut (take-right-while (bind < 1) [1 2 3]) [2 3])
(ut (take-right-while (bind < 1) '(1 2 3)) '(2 3))
(ut (take-right-while (bind char<? #\1) "123") "23")
(ut (take-right-while (compose (bind char<? #\1) chop-char-high) "\U000100012\U00010003") "2\U00010003")
(ut (take-right-while (compose (bind char<? #\1) chop-char-high) "1\U000100023") "\U000100023")
(ut (take-right-while (compose (bind char<? #\1) chop-char-high) "\U00010001\U00010002\U00010003") "\U00010002\U00010003")

(define (chop-char<? a b)
  (char<? (chop-char-high a) (chop-char-high b)))

(define (chop-char<=? a b)
  (char<=? (chop-char-high a) (chop-char-high b)))

(ut (sort < '()) '())
(ut (sort < []) [])
(ut (sort char<? "") "")
(ut (sort < '(1)) '(1))
(ut (sort < [1]) [1])
(ut (sort char<? "1") "1")
(ut (sort char<? "\U00010001") "\U00010001")
(ut (sort < '(3 2 1)) '(1 2 3))
(ut (sort < [3 2 1]) [1 2 3])
(ut (sort char<? "321") "123")
(ut (sort chop-char<? "\U000100032\U00010001") "\U000100012\U00010003")
(ut (sort chop-char<? "3\U000100021") "1\U000100023")
(ut (sort chop-char<? "\U00010003\U00010002\U00010001") "\U00010001\U00010002\U00010003")
(ut (sort < '(1 2 3)) '(1 2 3))
(ut (sort < [1 2 3]) [1 2 3])
(ut (sort char<? "123") "123")
(ut (sort chop-char<? "\U000100012\U00010003") "\U000100012\U00010003")
(ut (sort chop-char<? "1\U000100023") "1\U000100023")
(ut (sort chop-char<? "\U00010001\U00010002\U00010003") "\U00010001\U00010002\U00010003")
(ut (sort < '(4 3 2 1)) '(1 2 3 4))
(ut (sort < [4 3 2 1]) [1 2 3 4])
(ut (sort char<? "4321") "1234")
(ut (sort chop-char<? "\U000100043\U000100021") "1\U000100023\U00010004")
(ut (sort chop-char<? "4\U000100032\U00010001") "\U000100012\U000100034")
(ut (sort chop-char<? "\U00010004\U00010003\U00010002\U00010001") "\U00010001\U00010002\U00010003\U00010004")
(ut (sort < '(1 2 3 4)) '(1 2 3 4))
(ut (sort < [1 2 3 4]) [1 2 3 4])
(ut (sort char<? "1234") "1234")
(ut (sort chop-char<? "\U000100012\U000100034") "\U000100012\U000100034")
(ut (sort chop-char<? "1\U000100023\U00010004") "1\U000100023\U00010004")
(ut (sort chop-char<? "\U00010001\U00010002\U00010003\U00010004") "\U00010001\U00010002\U00010003\U00010004")

(ut (sorted? char<? "") #t)
(ut (sorted? char<? "1234") #t)
(ut (sorted? char<? "4321") #f)
(ut (sorted? char<? "12234") #f)
(ut (sorted? char<=? "12234") #t)
(ut (sorted? char<? "11234") #f)
(ut (sorted? char<=? "11234") #t)
(ut (sorted? char<? "12344") #f)
(ut (sorted? char<=? "12344") #t)
(ut (sorted? chop-char<? "\U000100012\U000100034") #t)
(ut (sorted? chop-char<? "4\U000100032\U00010001") #f)
(ut (sorted? chop-char<? "\U0001000122\U000100034") #f)
(ut (sorted? chop-char<=? "\U0001000122\U000100034") #t)
(ut (sorted? chop-char<? "\U00010001\U000100012\U000100034") #f)
(ut (sorted? chop-char<=? "\U00010001\U000100012\U000100034") #t)
(ut (sorted? chop-char<? "\U000100012\U0001000344") #f)
(ut (sorted? chop-char<=? "\U000100012\U0001000344") #t)
(ut (sorted? chop-char<? "\U000100012\U000100034") #t)
(ut (sorted? chop-char<? "4\U000100032\U00010001") #f)
(ut (sorted? chop-char<? "\U0001000122\U000100034") #f)
(ut (sorted? chop-char<=? "\U0001000122\U000100034") #t)
(ut (sorted? chop-char<? "\U00010001\U000100012\U000100034") #f)
(ut (sorted? chop-char<=? "\U00010001\U000100012\U000100034") #t)
(ut (sorted? chop-char<? "\U000100012\U0001000344") #f)
(ut (sorted? chop-char<=? "\U000100012\U0001000344") #t)
(ut (sorted? chop-char<? "\U00010001\U00010002\U00010003\U00010004") #t)
(ut (sorted? chop-char<? "\U00010004\U00010003\U00010002\U00010001") #f)
(ut (sorted? chop-char<? "\U00010001\U00010002\U00010002\U00010003\U00010004") #f)
(ut (sorted? chop-char<=? "\U00010001\U00010002\U00010002\U00010003\U00010004") #t)
(ut (sorted? chop-char<? "\U00010001\U00010001\U00010002\U00010003\U00010004") #f)
(ut (sorted? chop-char<=? "\U00010001\U00010001\U00010002\U00010003\U00010004") #t)
(ut (sorted? chop-char<? "\U00010001\U00010002\U00010003\U00010004\U00010004") #f)
(ut (sorted? chop-char<=? "\U00010001\U00010002\U00010003\U00010004\U00010004") #t)

(ut (sorted? < []) #t)
(ut (sorted? < [1 2 3 4]) #t)
(ut (sorted? < [4 3 2 1]) #f)
(ut (sorted? < [1 2 2 3 4]) #f)
(ut (sorted? <= [1 2 2 3 4]) #t)
(ut (sorted? < [1 1 2 3 4]) #f)
(ut (sorted? <= [1 1 2 3 4]) #t)
(ut (sorted? < [1 2 3 4 4]) #f)
(ut (sorted? <= [1 2 3 4 4]) #t)

(ut (sorted? < '()) #t)
(ut (sorted? < '(1 2 3 4)) #t)
(ut (sorted? < '(4 3 2 1)) #f)
(ut (sorted? < '(1 2 2 3 4)) #f)
(ut (sorted? <= '(1 2 2 3 4)) #t)
(ut (sorted? < '(1 1 2 3 4)) #f)
(ut (sorted? <= '(1 1 2 3 4)) #t)
(ut (sorted? < '(1 2 3 4 4)) #f)
(ut (sorted? <= '(1 2 3 4 4)) #t)

(ut (merge char<? "" "") "")
(ut (merge < '() '()) '())
(ut (merge < [] []) [])
(ut (merge < [] "") [])
(ut (merge < '() []) '())
(ut (merge < '(1 3 5) '(2 4 6)) '(1 2 3 4 5 6))
(ut (merge < [1 3 5] [2 4 6]) [1 2 3 4 5 6])
(ut (merge char<? "135" "246") "123456")
(ut (merge chop-char<? "\U000100013\U00010005" "\U000100024\U00010006") "\U00010001\U0001000234\U00010005\U00010006")
(ut (merge chop-char<? "1\U000100035" "2\U000100046") "12\U00010003\U0001000456")
(ut (merge chop-char<? "\U00010001\U00010003\U00010005" "\U00010002\U00010004\U00010006") "\U00010001\U00010002\U00010003\U00010004\U00010005\U00010006")
(ut (merge < '(1 2 3) '()) '(1 2 3))
(ut (merge < '() '(1 2 3)) '(1 2 3))
(ut (merge < [1 2 3] []) [1 2 3])
(ut (merge < [] [1 2 3]) [1 2 3])
(ut (merge char<? "123" "") "123")
(ut (merge chop-char<? "\U000100012\U00010003" "") "\U000100012\U00010003")
(ut (merge chop-char<? "1\U000100023" "") "1\U000100023")
(ut (merge chop-char<? "\U00010001\U00010002\U00010003" "") "\U00010001\U00010002\U00010003")
(ut (merge char<? "" "123") "123")
(ut (merge chop-char<? "" "\U000100012\U00010003") "\U000100012\U00010003")
(ut (merge chop-char<? "" "1\U000100023") "1\U000100023")
(ut (merge chop-char<? "" "\U00010001\U00010002\U00010003") "\U00010001\U00010002\U00010003")

(ut (delete-neighbor-duplicates = '()) '())
(ut (delete-neighbor-duplicates = '(1)) '(1))
(ut (delete-neighbor-duplicates = '(1 1)) '(1))
(ut (delete-neighbor-duplicates = '(1 1 2 2 3 3 4 4 5 5)) '(1 2 3 4 5))
(ut (delete-neighbor-duplicates = []) [])
(ut (delete-neighbor-duplicates = [1]) [1])
(ut (delete-neighbor-duplicates = [1 1]) [1])
(ut (delete-neighbor-duplicates = [1 1 2 2 3 3 4 4 5 5]) [1 2 3 4 5])
(ut (delete-neighbor-duplicates char=? "") "")
(ut (delete-neighbor-duplicates char=? "1") "1")
(ut (delete-neighbor-duplicates char=? "\U00010001") "\U00010001")
(ut (delete-neighbor-duplicates char=? "11") "1")
(ut (delete-neighbor-duplicates char=? "\U000100011") "\U000100011")
(ut (delete-neighbor-duplicates char=? "1\U00010001") "1\U00010001")
(ut (delete-neighbor-duplicates char=? "\U00010001\U00010001") "\U00010001")
(ut (delete-neighbor-duplicates char=? "1122334455") "12345")
(ut (delete-neighbor-duplicates char=? "\U00010001\U0001000122\U00010003\U0001000344\U00010005\U00010005") "\U000100012\U000100034\U00010005")
(ut (delete-neighbor-duplicates char=? "11\U00010002\U0001000233\U00010004\U0001000455") "1\U000100023\U000100045")
(ut (delete-neighbor-duplicates char=? "\U00010001\U00010001\U00010002\U00010002\U00010003\U00010003\U00010004\U00010004\U00010005\U00010005") "\U00010001\U00010002\U00010003\U00010004\U00010005")
