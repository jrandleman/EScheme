;; Author: Jordan Randleman -- associative-collection.scm
;; => Tests for EScheme's primitive functions.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load (path (path-parent #path 2) "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPER SYNTAX
(define-syntax hashmap-has-values?
  (lambda (h . args)
    (cons 'list-has-values? (cons (list 'hashmap-values h) args))))

(define-syntax test-for-each
  (lambda (<? base-container . containers)
    (define args (map (lambda (x) (gensym)) containers))
    `(let ()
      (define c ,base-container)
      (for-each
        (lambda ,args
          ,@(map (lambda (arg) (list 'set! 'c (list 'conj arg 'c))) args))
        ,@containers)
      (ut (begin 'testing-for-each (sort ,<? c)) (sort ,<? (append ,@containers))))))

(define-syntax test-for-each-hashmap
  (lambda (<? . containers)
    (define args (map (lambda (x) (gensym)) containers))
    (define hashmap-value-lists (map (lambda (x) (cons 'list (hashmap-values x))) containers))
    `(let ()
      (define c '())
      (for-each
        (lambda ,args
          ,@(map (lambda (arg) (list 'set! 'c (list 'conj arg 'c))) args))
        ,@containers)
      (ut (begin 'testing-for-each (sort ,<? c)) (sort ,<? (append ,@hashmap-value-lists))))))

(define-syntax test-oc-set!
  (lambda (op <? soln . args)
    `(ut (sort ,<? (,op eq? ,@args)) (sort ,<? ,soln))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(ut (associative-collection? "") #t)
(ut (associative-collection? []) #t)
(ut (associative-collection? '()) #t)
(ut (associative-collection? {}) #t)
(ut (associative-collection? 0) #f)

(ut (ac? "") #t)
(ut (ac? []) #t)
(ut (ac? '()) #t)
(ut (ac? {}) #t)
(ut (ac? 0) #f)

(ut (head "1") #\1)
(ut (head [1]) 1)
(ut (head '(1)) 1)
(ut (head {:a 1}) 1)
(ut (head "12") #\1)
(ut (head [1 2]) 1)
(ut (head '(1 2)) 1)
(ut (hashmap-val? {:a 1 :b 2} (head {:a 1 :b 2})) #t)
(ut (head "123") #\1)
(ut (head [1 2 3]) 1)
(ut (head '(1 2 3)) 1)
(ut (hashmap-val? {:a 1 :b 2 :c 3} (head {:a 1 :b 2 :c 3})) #t)

(ut (tail "1") "")
(ut (tail [1]) [])
(ut (tail '(1)) '())
(ut (tail {:a 1}) {})
(ut (tail "12") "2")
(ut (tail [1 2]) [2])
(ut (tail '(1 2)) '(2))
(ut (length (tail {:a 1 :b 2})) 1)
(ut (tail "123") "23")
(ut (tail [1 2 3]) [2 3])
(ut (tail '(1 2 3)) '(2 3))
(ut (length (tail {:a 1 :b 2 :c 3})) 2)

(ut (empty? "") #t)
(ut (empty? []) #t)
(ut (empty? '()) #t)
(ut (empty? {}) #t)
(ut (empty? "1") #f)
(ut (empty? [1]) #f)
(ut (empty? '(1)) #f)
(ut (empty? {:a 1}) #f)
(ut (empty? "12") #f)
(ut (empty? [1 2]) #f)
(ut (empty? '(1 2)) #f)
(ut (empty? {:a 1 :b 2}) #f)

(ut (length "") 0)
(ut (length []) 0)
(ut (length '()) 0)
(ut (length {}) 0)
(ut (length "1") 1)
(ut (length [1]) 1)
(ut (length '(1)) 1)
(ut (length {:a 1}) 1)
(ut (length "12") 2)
(ut (length [1 2]) 2)
(ut (length '(1 2)) 2)
(ut (length {:a 1 :b 2}) 2)

(ut (length+ "") 0)
(ut (length+ []) 0)
(ut (length+ '()) 0)
(ut (length+ {}) 0)
(ut (length+ "1") 1)
(ut (length+ [1]) 1)
(ut (length+ '(1)) 1)
(ut (length+ '(1 . 0)) #f)
(ut (length+ {:a 1}) 1)
(ut (length+ "12") 2)
(ut (length+ [1 2]) 2)
(ut (length+ '(1 2)) 2)
(ut (length+ '(1 2 . 3)) #f)
(ut (length+ {:a 1 :b 2}) 2)

(define (char->int ch) (- (char->integer ch) (char->integer #\0)))
(define (char- n c) (- n (char->int c)))
(ut (fold - 0 []) 0)
(ut (fold - 0 [3]) -3)
(ut (fold - 0 [2 3]) -5)
(ut (fold - 0 [1 2 3]) -6)
(ut (fold - 0 '()) 0)
(ut (fold - 0 '(3)) -3)
(ut (fold - 0 '(2 3)) -5)
(ut (fold - 0 '(1 2 3)) -6)
(ut (fold char- 0 "") 0)
(ut (fold char- 0 "3") -3)
(ut (fold char- 0 "23") -5)
(ut (fold char- 0 '"123") -6)
; Can't test fold with "-" on hashmaps since traversal order is undefined

(define (char+ n c) (+ n (char->int c)))
(ut (fold + 0 []) 0)
(ut (fold + 0 [3]) 3)
(ut (fold + 0 [2 3]) 5)
(ut (fold + 0 [1 2 3]) 6)
(ut (fold + 0 '()) 0)
(ut (fold + 0 '(3)) 3)
(ut (fold + 0 '(2 3)) 5)
(ut (fold + 0 '(1 2 3)) 6)
(ut (fold char+ 0 "") 0)
(ut (fold char+ 0 "3") 3)
(ut (fold char+ 0 "23") 5)
(ut (fold char+ 0 '"123") 6)
(ut (fold + 0 {}) 0)
(ut (fold + 0 {:c 3}) 3)
(ut (fold + 0 {:b 2 :c 3}) 5)
(ut (fold + 0 {:a 1 :b 2 :c 3}) 6)

(define (char++ n c k) (+ n (char->int c) (char->int k)))
(ut (fold + 0 [] []) 0)
(ut (fold + 0 [3] [3]) 6)
(ut (fold + 0 [2 3] [2 3]) 10)
(ut (fold + 0 [1 2 3] [1 2 3]) 12)
(ut (fold + 0 '() '()) 0)
(ut (fold + 0 '(3) '(3)) 6)
(ut (fold + 0 '(2 3) '(2 3)) 10)
(ut (fold + 0 '(1 2 3) '(1 2 3)) 12)
(ut (fold char++ 0 "" "") 0)
(ut (fold char++ 0 "3" "3") 6)
(ut (fold char++ 0 "23" "23") 10)
(ut (fold char++ 0 "123" "123") 12)
(ut (fold + 0 {} {}) 0)
(ut (fold + 0 {:c 3} {:c 3}) 6)
(ut (fold + 0 {:b 2 :c 3} {:b 2 :c 3}) 10)
(ut (fold + 0 {:a 1 :b 2 :c 3} {:a 1 :b 2 :c 3}) 12)

(define (char* . cs) 
  (integer->char (apply * (map char->int cs))))
(ut (map (bind * 2) []) [])
(ut (map (bind * 2) [1]) [2])
(ut (map (bind * 2) [1 2]) [2 4])
(ut (map (bind * 2) [1 2 3]) [2 4 6])
(ut (map (bind * 2) '()) '())
(ut (map (bind * 2) '(1)) '(2))
(ut (map (bind * 2) '(1 2)) '(2 4))
(ut (map (bind * 2) '(1 2 3)) '(2 4 6))
(ut (map (bind char* #\2) "") "")
(ut (map (bind char* #\2) "1") "\u0002")
(ut (map (bind char* #\2) "12") "\u0002\u0004")
(ut (map (bind char* #\2) "123") "\u0002\u0004\u0006")
(ut (map (bind * 2) {}) {})
(ut (map (bind * 2) {:c 3}) {:c 6})
(ut (map (bind * 2) {:b 2 :c 3}) {:b 4 :c 6})
(ut (map (bind * 2) {:a 1 :b 2 :c 3}) {:a 2 :b 4 :c 6})

(ut (map (bind * 2) [] []) [])
(ut (map (bind * 2) [1] [1]) [2])
(ut (map (bind * 2) [1 2] [1 2]) [2 8])
(ut (map (bind * 2) [1 2 3] [1 2 3]) [2 8 18])
(ut (map (bind * 2) '() '()) '())
(ut (map (bind * 2) '(1) '(1)) '(2))
(ut (map (bind * 2) '(1 2) '(1 2)) '(2 8))
(ut (map (bind * 2) '(1 2 3) '(1 2 3)) '(2 8 18))
(ut (map (bind char* #\2) "" "") "")
(ut (map (bind char* #\2) "1" "1") "\u0002")
(ut (map (bind char* #\2) "12" "12") "\u0002\u0008")
(ut (map (bind char* #\2) "123" "123") "\u0002\u0008\u0012")
(ut (map (bind * 2) {} {}) {})
(ut (map (bind * 2) {:c 3} {:c 3}) {:c 18})
(hashmap-has-values? (map (bind * 2) {:b 2 :c 3} {:b 2 :c 3}) 8 18)
(hashmap-has-values? (map (bind * 2) {:a 1 :b 2 :c 3} {:a 1 :b 2 :c 3}) 2 8 18)

(test-for-each < [] [])
(test-for-each < [] [1])
(test-for-each < [] [1 2])
(test-for-each < [] [1 2 3])
(test-for-each < '() '())
(test-for-each < '() '(1))
(test-for-each < '() '(1 2))
(test-for-each < '() '(1 2 3))
(test-for-each char<? "" "1")
(test-for-each char<? "" "12")
(test-for-each char<? "" "123")
(test-for-each-hashmap < {})
(test-for-each-hashmap < {:a 1})
(test-for-each-hashmap < {:a 1 :b 2})
(test-for-each-hashmap < {:a 1 :b 2 :c 3})
(test-for-each-hashmap < {} {})

(test-for-each < [] [] [])
(test-for-each < [] [1] [1])
(test-for-each < [] [1 2] [1 2])
(test-for-each < [] [1 2 3] [1 2 3])
(test-for-each < '() '() '())
(test-for-each < '() '(1) '(1))
(test-for-each < '() '(1 2) '(1 2))
(test-for-each < '() '(1 2 3) '(1 2 3))
(test-for-each char<? "" "1" "1")
(test-for-each char<? "" "12" "12")
(test-for-each char<? "" "123" "123")
(test-for-each-hashmap < {:a 1} {:a 1})
(test-for-each-hashmap < {:a 1 :b 2} {:a 1 :b 2})
(test-for-each-hashmap < {:a 1 :b 2 :c 3} {:a 1 :b 2 :c 3})

(define even-char? (compose even? char->int))
(ut (filter even? []) [])
(ut (filter even? [1]) [])
(ut (filter odd? [1]) [1])
(ut (filter even? [2]) [2])
(ut (filter even? [1 2]) [2])
(ut (filter even? [1 2 3]) [2])
(ut (filter even? [1 2 3 4]) [2 4])
(ut (filter even? '()) '())
(ut (filter even? '(1)) '())
(ut (filter odd? '(1)) '(1))
(ut (filter even? '(2)) '(2))
(ut (filter even? '(1 2)) '(2))
(ut (filter even? '(1 2 3)) '(2))
(ut (filter even? '(1 2 3 4)) '(2 4))
(ut (filter even-char? "") "")
(ut (filter even-char? "1") "")
(ut (filter (compose not even-char?) "1") "1")
(ut (filter even-char? "2") "2")
(ut (filter even-char? "12") "2")
(ut (filter even-char? "123") "2")
(ut (filter even-char? "1234") "24")
(hashmap-has-values? (filter even? {}))
(hashmap-has-values? (filter even? {:a 1}))
(hashmap-has-values? (filter odd? {:a 1}) 1)
(hashmap-has-values? (filter even? {:b 2}) 2)
(hashmap-has-values? (filter even? {:a 1 :b 2}) 2)
(hashmap-has-values? (filter even? {:a 1 :b 2 :c 3}) 2)
(hashmap-has-values? (filter even? {:a 1 :b 2 :c 3 :d 4}) 2 4)

(ut (count even? []) 0)
(ut (count even? [1]) 0)
(ut (count odd? [1]) 1)
(ut (count even? [2]) 1)
(ut (count even? [1 2]) 1)
(ut (count even? [1 2 3]) 1)
(ut (count even? [1 2 3 4]) 2)
(ut (count even? '()) 0)
(ut (count even? '(1)) 0)
(ut (count odd? '(1)) 1)
(ut (count even? '(2)) 1)
(ut (count even? '(1 2)) 1)
(ut (count even? '(1 2 3)) 1)
(ut (count even? '(1 2 3 4)) 2)
(ut (count even-char? "") 0)
(ut (count even-char? "1") 0)
(ut (count (compose not even-char?) "1") 1)
(ut (count even-char? "2") 1)
(ut (count even-char? "12") 1)
(ut (count even-char? "123") 1)
(ut (count even-char? "1234") 2)
(ut (count even? {}) 0)
(ut (count even? {:a 1}) 0)
(ut (count odd? {:a 1}) 1)
(ut (count even? {:b 2}) 1)
(ut (count even? {:a 1 :b 2}) 1)
(ut (count even? {:a 1 :b 2 :c 3}) 1)
(ut (count even? {:a 1 :b 2 :c 3 :d 4}) 2)

(ut (remove even? []) [])
(ut (remove even? [1]) [1])
(ut (remove odd? [1]) [])
(ut (remove even? [2]) [])
(ut (remove even? [1 2]) [1])
(ut (remove even? [1 2 3]) [1 3])
(ut (remove even? [1 2 3 4]) [1 3])
(ut (remove even? '()) '())
(ut (remove even? '(1)) '(1))
(ut (remove odd? '(1)) '())
(ut (remove even? '(2)) '())
(ut (remove even? '(1 2)) '(1))
(ut (remove even? '(1 2 3)) '(1 3))
(ut (remove even? '(1 2 3 4)) '(1 3))
(ut (remove even-char? "") "")
(ut (remove even-char? "1") "1")
(ut (remove (compose not even-char?) "1") "")
(ut (remove even-char? "2") "")
(ut (remove even-char? "12") "1")
(ut (remove even-char? "123") "13")
(ut (remove even-char? "1234") "13")
(hashmap-has-values? (remove even? {}))
(hashmap-has-values? (remove even? {:a 1}) 1)
(hashmap-has-values? (remove odd? {:a 1}))
(hashmap-has-values? (remove even? {:b 2}))
(hashmap-has-values? (remove even? {:a 1 :b 2}) 1)
(hashmap-has-values? (remove even? {:a 1 :b 2 :c 3}) 1 3)
(hashmap-has-values? (remove even? {:a 1 :b 2 :c 3 :d 4}) 1 3)

(ut (val [1] 0) 1)
(ut (val [1 2] 0) 1)
(ut (val [1 2] 1) 2)
(ut (val [1 2 3] 0) 1)
(ut (val [1 2 3] 1) 2)
(ut (val [1 2 3] 2) 3)
(ut (val '(1) 0) 1)
(ut (val '(1 2) 0) 1)
(ut (val '(1 2) 1) 2)
(ut (val '(1 2 3) 0) 1)
(ut (val '(1 2 3) 1) 2)
(ut (val '(1 2 3) 2) 3)
(ut (val "1" 0) #\1)
(ut (val "12" 0) #\1)
(ut (val "12" 1) #\2)
(ut (val "123" 0) #\1)
(ut (val "123" 1) #\2)
(ut (val "123" 2) #\3)
(ut (val {:a 1} :a) 1)
(ut (val {:a 1 :b 2} :a) 1)
(ut (val {:a 1 :b 2} :b) 2)
(ut (val {:a 1 :b 2 :c 3} :a) 1)
(ut (val {:a 1 :b 2 :c 3} :b) 2)
(ut (val {:a 1 :b 2 :c 3} :c) 3)

(ut (key (lambda (v) (eq? v 1)) [1]) 0)
(ut (key (lambda (v) (eq? v 1)) [1 2]) 0)
(ut (key (lambda (v) (eq? v 2)) [1 2]) 1)
(ut (key (lambda (v) (eq? v 1))  [1 2 3]) 0)
(ut (key (lambda (v) (eq? v 2)) [1 2 3]) 1)
(ut (key (lambda (v) (eq? v 3)) [1 2 3]) 2)
(ut (key (lambda (v) (eq? v 1)) '(1)) 0)
(ut (key (lambda (v) (eq? v 1)) '(1 2)) 0)
(ut (key (lambda (v) (eq? v 2)) '(1 2)) 1)
(ut (key (lambda (v) (eq? v 1)) '(1 2 3)) 0)
(ut (key (lambda (v) (eq? v 2)) '(1 2 3)) 1)
(ut (key (lambda (v) (eq? v 3)) '(1 2 3)) 2)
(ut (key (lambda (v) (eq? v #\1))  "1") 0)
(ut (key (lambda (v) (eq? v #\1))  "12") 0)
(ut (key (lambda (v) (eq? v #\2)) "12") 1)
(ut (key (lambda (v) (eq? v #\1))  "123") 0)
(ut (key (lambda (v) (eq? v #\2)) "123") 1)
(ut (key (lambda (v) (eq? v #\3)) "123") 2)
(ut (key (lambda (v) (eq? v 1)) {:a 1}) :a)
(ut (key (lambda (v) (eq? v 1)) {:a 1 :b 2}) :a)
(ut (key (lambda (v) (eq? v 2)) {:a 1 :b 2}) :b)
(ut (key (lambda (v) (eq? v 1)) {:a 1 :b 2 :c 3}) :a)
(ut (key (lambda (v) (eq? v 2)) {:a 1 :b 2 :c 3}) :b)
(ut (key (lambda (v) (eq? v 3)) {:a 1 :b 2 :c 3}) :c)

; <append> should generate <'()> by default
(ut (append) '())
; test reflecting back 1 item
(ut (append '()) '())
(ut (append 1) 1)
(ut (append #f) #f)
; test dotted lists (atom after appending only lists)
(ut (append '(1) 42) '(1 . 42))
(ut (append '(1 2) 42) '(1 2 . 42))
(ut (append '(1) '(2) 42) '(1 2 . 42))
(ut (append '(1) '() '(2) '() 42) '(1 2 . 42))
; test containers
(ut (append '() '() '()) '())
(ut (append '(1) '(2) '(3)) '(1 2 3))
(ut (append '(1 2) '(3 4) '(5 6)) '(1 2 3 4 5 6))
(ut (append [] [] []) [])
(ut (append [1] [2] [3]) [1 2 3])
(ut (append [1 2] [3 4] [5 6]) [1 2 3 4 5 6])
(ut (append "" "" "") "")
(ut (append "1" "2" "3") "123")
(ut (append "12" "34" "56") "123456")
(ut (append {} {} {}) {})
(ut (append {:a 1} {:b 2} {:c 3}) {:a 1 :b 2 :c 3})
(ut (append {:a 1 :b 2} {:c 3 :d 4} {:e 5 :f 6}) {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6})

(ut eq? ++ append) ; verify alias

(ut (delete [1] 0) [])
(ut (delete [1 2] 0) [2])
(ut (delete [1 2] 1) [1])
(ut (delete [1 2 3] 0) [2 3])
(ut (delete [1 2 3] 1) [1 3])
(ut (delete [1 2 3] 2) [1 2])
(ut (delete '(1) 0) '())
(ut (delete '(1 2) 0) '(2))
(ut (delete '(1 2) 1) '(1))
(ut (delete '(1 2 3) 0) '(2 3))
(ut (delete '(1 2 3) 1) '(1 3))
(ut (delete '(1 2 3) 2) '(1 2))
(ut (delete "1" 0) "")
(ut (delete "12" 0) "2")
(ut (delete "12" 1) "1")
(ut (delete "123" 0) "23")
(ut (delete "123" 1) "13")
(ut (delete "123" 2) "12")
(ut (delete {:a 1} :a) {})
(ut (delete {:a 1 :b 2} :a) {:b 2})
(ut (delete {:a 1 :b 2} :b) {:a 1})
(ut (delete {:a 1 :b 2 :c 3} :a) {:b 2 :c 3})
(ut (delete {:a 1 :b 2 :c 3} :b) {:a 1 :c 3})
(ut (delete {:a 1 :b 2 :c 3} :c) {:a 1 :b 2})

(ut (conj 0 #\1 "") "1")
(ut (conj 0 #\2 "1") "2")
(ut (conj 1 #\2 "1") "12")
(ut (conj 0 #\3 "12") "32")
(ut (conj 1 #\3 "12") "13")
(ut (conj 2 #\3 "12") "123")
(ut (conj 0 1 []) [1])
(ut (conj 0 2 [1]) [2])
(ut (conj 1 2 [1]) [1 2])
(ut (conj 0 3 [1 2]) [3 2])
(ut (conj 1 3 [1 2]) [1 3])
(ut (conj 2 3 [1 2]) [1 2 3])
(ut (conj 0 1 '()) '(1))
(ut (conj -1 2 '(1)) '(2 1)) ; pairs can only <conj> onto the front: hence only accepts <-1> as a key
(ut (conj -1 3 '(2 1)) '(3 2 1))
(ut (conj :a 1 {}) {:a 1})
(ut (conj :a 2 {:a 1}) {:a 2})
(ut (conj :b 2 {:a 1}) {:a 1 :b 2})
(ut (conj :b 3 {:a 1 :b 2}) {:a 1 :b 3})
(ut (conj :c 3 {:a 1 :b 2}) {:a 1 :b 2 :c 3})

(ut (take "" 0) "")
(ut (take "" 100) "")
(ut (take "1" 0) "")
(ut (take "1" 1) "1")
(ut (take "12" 0) "")
(ut (take "12" 1) "1")
(ut (take "12" 2) "12")
(ut (take "12" 100) "12")
(ut (take [] 0) [])
(ut (take [] 100) [])
(ut (take [1] 0) [])
(ut (take [1] 1) [1])
(ut (take [1 2] 0) [])
(ut (take [1 2] 1) [1])
(ut (take [1 2] 2) [1 2])
(ut (take [1 2] 100) [1 2])
(ut (take '() 0) '())
(ut (take '() 100) '())
(ut (take '(1) 0) '())
(ut (take '(1) 1) '(1))
(ut (take '(1 2) 0) '())
(ut (take '(1 2) 1) '(1))
(ut (take '(1 2) 2) '(1 2))
(ut (take '(1 2) 100) '(1 2))
(ut (take {} 0) {})
(ut (take {} 100) {})
(ut (take {:a 1} 0) {})
(ut (take {:a 1} 1) {:a 1})
(ut (take {:a 1 :b 2} 0) {})
(ut (length (take {:a 1 :b 2} 1)) 1) ; can't predict which one will be picked
(ut (take {:a 1 :b 2} 2) {:a 1 :b 2})
(ut (take {:a 1 :b 2} 100) {:a 1 :b 2})

(ut (drop "" 0) "")
(ut (drop "" 100) "")
(ut (drop "1" 0) "1")
(ut (drop "1" 1) "")
(ut (drop "12" 0) "12")
(ut (drop "12" 1) "2")
(ut (drop "12" 2) "")
(ut (drop "12" 100) "")
(ut (drop [] 0) [])
(ut (drop [] 100) [])
(ut (drop [1] 0) [1])
(ut (drop [1] 1) [])
(ut (drop [1 2] 0) [1 2])
(ut (drop [1 2] 1) [2])
(ut (drop [1 2] 2) [])
(ut (drop [1 2] 100) [])
(ut (drop '() 0) '())
(ut (drop '() 100) '())
(ut (drop '(1) 0) '(1))
(ut (drop '(1) 1) '())
(ut (drop '(1 2) 0) '(1 2))
(ut (drop '(1 2) 1) '(2))
(ut (drop '(1 2) 2) '())
(ut (drop '(1 2) 100) '())
(ut (drop {} 0) {})
(ut (drop {} 100) {})
(ut (drop {:a 1} 0) {:a 1})
(ut (drop {:a 1} 1) {})
(ut (drop {:a 1 :b 2} 0) {:a 1 :b 2})
(ut (length (drop {:a 1 :b 2} 1)) 1) ; can't predict which one will be picked
(ut (drop {:a 1 :b 2} 2) {})
(ut (drop {:a 1 :b 2} 100) {})

(define even-length? (compose even? length))
(ut (any? even-length? '() [] "" {}) #t)
(ut (any? even-length? '(1) [1] "1" {:a 1}) #f)
(ut (any? even-length? '(1 2) [1] "1" {:a 1}) #t)
(ut (any? even-length? '(1) [1 2] "1" {:a 1}) #t)
(ut (any? even-length? '(1) [1] "12" {:a 1}) #t)
(ut (any? even-length? '(1) [1] "1" {:a 1 :b 2}) #t)

(ut (every? even-length? '() [] "" {}) #t)
(ut (every? even-length? '(1) [1] "1" {:a 1}) #f)
(ut (every? even-length? '(1 2) [1] "1" {:a 1}) #f)
(ut (every? even-length? '(1) [1 2] "1" {:a 1}) #f)
(ut (every? even-length? '(1) [1] "12" {:a 1}) #f)
(ut (every? even-length? '(1) [1] "1" {:a 1 :b 2}) #f)
(ut (every? even-length? '(1 2) [1 2] "12" {:a 1 :b 2}) #t)

(ut (ac->string "") "")
(ut (ac->string "1") "1")
(ut (ac->string "12") "12")
(ut (ac->string []) "")
(ut (ac->string [#\1]) "1")
(ut (ac->string [#\1 #\2]) "12")
(ut (ac->string '()) "")
(ut (ac->string '(#\1)) "1")
(ut (ac->string '(#\1 #\2)) "12")
(ut (ac->string {}) "")
(ut (ac->string {0 #\1}) "1")
(ut (ac->string {0 #\1 1 #\2}) "12")

(ut (ac->list "") '())
(ut (ac->list "1") '(#\1))
(ut (ac->list "12") '(#\1 #\2))
(ut (ac->list []) '())
(ut (ac->list [1]) '(1))
(ut (ac->list [1 2]) '(1 2))
(ut (ac->list '()) '())
(ut (ac->list '(1)) '(1))
(ut (ac->list '(1 2)) '(1 2))
(ut (ac->list {}) '())
(ut (ac->list {0 1}) '(1))
(ut (ac->list {0 1 1 2}) '(1 2))

(ut (ac->vector "") [])
(ut (ac->vector "1") [#\1])
(ut (ac->vector "12") [#\1 #\2])
(ut (ac->vector []) [])
(ut (ac->vector [1]) [1])
(ut (ac->vector [1 2]) [1 2])
(ut (ac->vector '()) [])
(ut (ac->vector '(1)) [1])
(ut (ac->vector '(1 2)) [1 2])
(ut (ac->vector {}) [])
(ut (ac->vector {0 1}) [1])
(ut (ac->vector {0 1 1 2}) [1 2])

(ut (ac->hashmap "") {})
(ut (ac->hashmap "1") {0 #\1})
(ut (ac->hashmap "12") {0 #\1 1 #\2})
(ut (ac->hashmap []) {})
(ut (ac->hashmap [1]) {0 1})
(ut (ac->hashmap [1 2]) {0 1 1 2})
(ut (ac->hashmap '()) {})
(ut (ac->hashmap '(1)) {0 1})
(ut (ac->hashmap '(1 2)) {0 1 1 2})
(ut (ac->hashmap {}) {})
(ut (ac->hashmap {0 1}) {0 1})
(ut (ac->hashmap {0 1 1 2}) {0 1 1 2})

(test-oc-set! union char<? "" "")
(test-oc-set! union char<? "" "" "")
(test-oc-set! union char<? "1" "1" "1")
(test-oc-set! union char<? "123" "12" "13")
(test-oc-set! union char<? "1234" "12" "13" "34")

(test-oc-set! intersection char<? "" "")
(test-oc-set! intersection char<? "" "" "")
(test-oc-set! intersection char<? "1" "1" "1")
(test-oc-set! intersection char<? "1" "12" "13")
(test-oc-set! intersection char<? "3" "312" "13" "34")
(test-oc-set! intersection char<? "" "12" "13" "34")
(test-oc-set! intersection char<? "31" "312" "13" "341")

(test-oc-set! difference char<? "" "")
(test-oc-set! difference char<? "" "" "")
(test-oc-set! difference char<? "" "1" "1")
(test-oc-set! difference char<? "2" "12" "13")
(test-oc-set! difference char<? "2" "312" "13" "34")
(test-oc-set! difference char<? "1" "12" "23" "34")
(test-oc-set! difference char<? "25" "3125" "13" "341")

(test-oc-set! symmetric-difference char<? "" "")
(test-oc-set! symmetric-difference char<? "" "" "")
(test-oc-set! symmetric-difference char<? "" "1" "1")
(test-oc-set! symmetric-difference char<? "23" "12" "13")
(test-oc-set! symmetric-difference char<? "234" "312" "13" "34")
(test-oc-set! symmetric-difference char<? "14" "12" "23" "34")
(test-oc-set! symmetric-difference char<? "12345" "3125" "13" "341")

(test-oc-set! union < [] [])
(test-oc-set! union < [] [] [])
(test-oc-set! union < [1] [1] [1])
(test-oc-set! union < [1 2 3] [1 2] [1 3])
(test-oc-set! union < [1 2 3 4] [1 2] [1 3] [3 4])

(test-oc-set! intersection < [] [])
(test-oc-set! intersection < [] [] [])
(test-oc-set! intersection < [1] [1] [1])
(test-oc-set! intersection < [1] [1 2] [1 3])
(test-oc-set! intersection < [3] [3 1 2] [1 3] [3 4])
(test-oc-set! intersection < [] [1 2] [1 3] [3 4])
(test-oc-set! intersection < [3 1] [3 1 2] [1 3] [3 4 1])

(test-oc-set! difference < [] [])
(test-oc-set! difference < [] [] [])
(test-oc-set! difference < [] [1] [1])
(test-oc-set! difference < [2] [1 2] [1 3])
(test-oc-set! difference < [2] [3 1 2] [1 3] [3 4])
(test-oc-set! difference < [1] [1 2] [2 3] [3 4])
(test-oc-set! difference < [2 5] [3 1 2 5] [1 3] [3 4 1])

(test-oc-set! symmetric-difference < [] [])
(test-oc-set! symmetric-difference < [] [] [])
(test-oc-set! symmetric-difference < [] [1] [1])
(test-oc-set! symmetric-difference < [2 3] [1 2] [1 3])
(test-oc-set! symmetric-difference < [2 3 4] [3 1 2] [1 3] [3 4])
(test-oc-set! symmetric-difference < [1 4] [1 2] [2 3] [3 4])
(test-oc-set! symmetric-difference < [1 2 3 4 5] [3 1 2 5] [1 3] [3 4 1])

(test-oc-set! union < '() '())
(test-oc-set! union < '() '() '())
(test-oc-set! union < '(1) '(1) '(1))
(test-oc-set! union < '(1 2 3) '(1 2) '(1 3))
(test-oc-set! union < '(1 2 3 4) '(1 2) '(1 3) '(3 4))

(test-oc-set! intersection < '() '())
(test-oc-set! intersection < '() '() '())
(test-oc-set! intersection < '(1) '(1) '(1))
(test-oc-set! intersection < '(1) '(1 2) '(1 3))
(test-oc-set! intersection < '(3) '(3 1 2) '(1 3) '(3 4))
(test-oc-set! intersection < '() '(1 2) '(1 3) '(3 4))
(test-oc-set! intersection < '(3 1) '(3 1 2) '(1 3) '(3 4 1))

(test-oc-set! difference < '() '())
(test-oc-set! difference < '() '() '())
(test-oc-set! difference < '() '(1) '(1))
(test-oc-set! difference < '(2) '(1 2) '(1 3))
(test-oc-set! difference < '(2) '(3 1 2) '(1 3) '(3 4))
(test-oc-set! difference < '(1) '(1 2) '(2 3) '(3 4))
(test-oc-set! difference < '(2 5) '(3 1 2 5) '(1 3) '(3 4 1))

(test-oc-set! symmetric-difference < '() '())
(test-oc-set! symmetric-difference < '() '() '())
(test-oc-set! symmetric-difference < '() '(1) '(1))
(test-oc-set! symmetric-difference < '(2 3) '(1 2) '(1 3))
(test-oc-set! symmetric-difference < '(2 3 4) '(3 1 2) '(1 3) '(3 4))
(test-oc-set! symmetric-difference < '(1 4) '(1 2) '(2 3) '(3 4))
(test-oc-set! symmetric-difference < '(1 2 3 4 5) '(3 1 2 5) '(1 3) '(3 4 1))

(ut (union eq? {}) {})
(ut (union eq? {} {}) {})
(ut (union eq? {:a 1} {:a 1}) {:a 1})
(ut (union eq? {:a 1 :b 2} {:a 1 :c 3}) {:a 1 :b 2 :c 3})
(ut (union eq? {:a 1 :b 2} {:a 1 :c 3} {:c 3 :d 4}) {:a 1 :b 2 :c 3 :d 4})

(ut (intersection eq? {}) {})
(ut (intersection eq? {} {}) {})
(ut (intersection eq? {:a 1} {:a 1}) {:a 1})
(ut (intersection eq? {:a 1 :b 2} {:a 1 :c 3}) {:a 1})
(ut (intersection eq? {:c 3 :a 1 :b 2} {:a 1 :c 3} {:c 3 :d 4}) {:c 3})
(ut (intersection eq? {:a 1 :b 2} {:a 1 :c 3} {:c 3 :d 4}) {})
(ut (intersection eq? {:c 3 :a 1 :b 2} {:a 1 :c 3} {:c 3 :d 4 :a 1}) {:c 3 :a 1})

(ut (difference eq? {}) {})
(ut (difference eq? {} {}) {})
(ut (difference eq? {:a 1} {:a 1}) {})
(ut (difference eq? {:a 1 :b 2} {:a 1 :c 3}) {:b 2})
(ut (difference eq? {:c 3 :a 1 :b 2} {:a 1 :c 3} {:c 3 :d 4}) {:b 2})
(ut (difference eq? {:a 1 :b 2} {:b 2 :c 3} {:c 3 :d 4}) {:a 1})
(ut (difference eq? {:c 3 :a 1 :b 2 :e 5} {:a 1 :c 3} {:c 3 :d 4 :a 1}) {:b 2 :e 5})

(ut (symmetric-difference eq? {}) {})
(ut (symmetric-difference eq? {} {}) {})
(ut (symmetric-difference eq? {:a 1} {:a 1}) {})
(ut (symmetric-difference eq? {:a 1 :b 2} {:a 1 :c 3}) {:b 2 :c 3})
(ut (symmetric-difference eq? {:c 3 :a 1 :b 2} {:a 1 :c 3} {:c 3 :d 4}) {:b 2 :c 3 :d 4})
(ut (symmetric-difference eq? {:a 1 :b 2} {:b 2 :c 3} {:c 3 :d 4}) {:a 1 :d 4})
(ut (symmetric-difference eq? {:c 3 :a 1 :b 2 :e 5} {:a 1 :c 3} {:c 3 :d 4 :a 1}) {:a 1 :b 2 :c 3 :d 4 :e 5})
