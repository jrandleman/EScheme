;; Author: Jordan Randleman -- types.scm
;; => Tests for EScheme's callable type dispatch system.
;;    Invoked by ../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load (path (path-parent #path 1) "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRIMITIVE TESTS
; Any
(ut ((lambda (:any x) x) 42) 42)

; Numerics
; => ":number" is aliased by ":complex"
(ut ((fn ((:number x) #t) ((:any x) #f)) 42) #t)
(ut ((fn ((:number x) #t) ((:any x) #f)) 42.0) #t)
(ut ((fn ((:number x) #t) ((:any x) #f)) 42.0+3.14i) #t)
(ut ((fn ((:number x) #t) ((:any x) #f)) "42") #f)
(ut ((fn ((:int x) #t) ((:any x) #f)) 42) #t)
(ut ((fn ((:int x) #t) ((:any x) #f)) 42.0) #t) ; still passes "integer?"
(ut ((fn ((:int x) #t) ((:any x) #f)) 42.5) #f)
(ut ((fn ((:int x) #t) ((:any x) #f)) "42") #f)
(ut ((fn ((:flo x) #t) ((:any x) #f)) 42) #f)
(ut ((fn ((:flo x) #t) ((:any x) #f)) 42.0) #t)
(ut ((fn ((:flo x) #t) ((:any x) #f)) "42") #f)
(ut ((fn ((:real x) #t) ((:any x) #f)) 42) #t)
(ut ((fn ((:real x) #t) ((:any x) #f)) 42.0) #t)
(ut ((fn ((:real x) #t) ((:any x) #f)) 42+5i) #f)
(ut ((fn ((:real x) #t) ((:any x) #f)) "42") #f)
(ut ((fn ((:exact x) #t) ((:any x) #f)) 42) #t)
(ut ((fn ((:exact x) #t) ((:any x) #f)) 42.0) #f)
(ut ((fn ((:exact x) #t) ((:any x) #f)) 3/7+5/6i) #t)
(ut ((fn ((:exact x) #t) ((:any x) #f)) 3.7+5.6i) #f)
(ut ((fn ((:exact x) #t) ((:any x) #f)) "42") #f)
(ut ((fn ((:inexact x) #t) ((:any x) #f)) 42) #f)
(ut ((fn ((:inexact x) #t) ((:any x) #f)) 42.0) #t)
(ut ((fn ((:inexact x) #t) ((:any x) #f)) 3/7+5/6i) #f)
(ut ((fn ((:inexact x) #t) ((:any x) #f)) 3.7+5.6i) #t)
(ut ((fn ((:inexact x) #t) ((:any x) #f)) "42") #f)

; Common Atomics
(ut ((fn ((:string x) #t) ((:any x) #f)) "42") #t)
(ut ((fn ((:string x) #t) ((:any x) #f)) 42) #f)
(ut ((fn ((:char x) #t) ((:any x) #f)) #\a) #t)
(ut ((fn ((:char x) #t) ((:any x) #f)) 42) #f)
(ut ((fn ((:key x) #t) ((:any x) #f)) :a) #t)
(ut ((fn ((:key x) #t) ((:any x) #f)) 42) #f)
(ut ((fn ((:bool x) #t) ((:any x) #f)) #t) #t)
(ut ((fn ((:bool x) #t) ((:any x) #f)) 42) #f)
(ut ((fn ((:symbol x) #t) ((:any x) #f)) 'a) #t)
(ut ((fn ((:symbol x) #t) ((:any x) #f)) 42) #f)
(ut ((fn ((:void x) #t) ((:any x) #f)) #void) #t)
(ut ((fn ((:void x) #t) ((:any x) #f)) 42) #f)

; Lists
(ut ((fn ((:nil x) #t) ((:any x) #f)) #nil) #t)
(ut ((fn ((:nil x) #t) ((:any x) #f)) '()) #t)
(ut ((fn ((:nil x) #t) ((:any x) #f)) 42) #f)
(ut ((fn ((:atom x) #t) ((:any x) #f)) 42) #t)
(ut ((fn ((:atom x) #t) ((:any x) #f)) '()) #t)
(ut ((fn ((:atom x) #t) ((:any x) #f)) '(1 . 2)) #f)
(ut ((fn ((:atom x) #t) ((:any x) #f)) '(1 2)) #f)

; Concurrency
(ut ((fn ((:thread x) #t) ((:any x) #f)) (thread (lambda () 42))) #t)
(ut ((fn ((:thread x) #t) ((:any x) #f)) 42) #f)
(ut ((fn ((:mutex x) #t) ((:any x) #f)) (mutex)) #t)
(ut ((fn ((:mutex x) #t) ((:any x) #f)) 42) #f)

; Functional
(ut ((fn ((:fn x) #t) ((:any x) #f)) (lambda () 42)) #t)
(ut ((fn ((:fn x) #t) ((:any x) #f)) ((class ((->procedure) #t)))) #t)
(ut ((fn ((:fn x) #t) ((:any x) #f)) eval) #t)
(ut ((fn ((:fn x) #t) ((:any x) #f)) if) #t)
(ut ((fn ((:fn x) #t) ((:any x) #f)) "") #t)
(ut ((fn ((:fn x) #t) ((:any x) #f)) []) #t)
(ut ((fn ((:fn x) #t) ((:any x) #f)) {}) #t)
(ut ((fn ((:fn x) #t) ((:any x) #f)) '()) #f)
(ut ((fn ((:fn x) #t) ((:any x) #f)) 42) #f)
(ut ((fn ((:procedure x) #t) ((:any x) #f)) (lambda () 42)) #t)
(ut ((fn ((:procedure x) #t) ((:any x) #f)) ((class ((->procedure) #t)))) #f)
(ut ((fn ((:procedure x) #t) ((:any x) #f)) eval) #t)
(ut ((fn ((:procedure x) #t) ((:any x) #f)) if) #t)
(ut ((fn ((:procedure x) #t) ((:any x) #f)) "") #f)
(ut ((fn ((:procedure x) #t) ((:any x) #f)) []) #f)
(ut ((fn ((:procedure x) #t) ((:any x) #f)) {}) #f)
(ut ((fn ((:procedure x) #t) ((:any x) #f)) '()) #f)
(ut ((fn ((:procedure x) #t) ((:any x) #f)) 42) #f)
(ut ((fn ((:syntax x) #t) ((:any x) #f)) (lambda () 42)) #f)
(ut ((fn ((:syntax x) #t) ((:any x) #f)) ((class ((->syntax) #t)))) #f)
(ut ((fn ((:syntax x) #t) ((:any x) #f)) eval) #f)
(ut ((fn ((:syntax x) #t) ((:any x) #f)) if) #t)
(ut ((fn ((:syntax x) #t) ((:any x) #f)) "") #f)
(ut ((fn ((:syntax x) #t) ((:any x) #f)) []) #f)
(ut ((fn ((:syntax x) #t) ((:any x) #f)) {}) #f)
(ut ((fn ((:syntax x) #t) ((:any x) #f)) '()) #f)
(ut ((fn ((:syntax x) #t) ((:any x) #f)) 42) #f)

; Objects
(import examples.specials.module1)
(ut ((fn ((:metaobj x) #t) ((:any x) #f)) (class)) #t)
(ut ((fn ((:metaobj x) #t) ((:any x) #f)) ((class))) #t)
(ut ((fn ((:metaobj x) #t) ((:any x) #f)) (interface)) #t)
(ut ((fn ((:metaobj x) #t) ((:any x) #f)) module1) #t)
(ut ((fn ((:metaobj x) #t) ((:any x) #f)) 42) #f)
(ut ((fn ((:object x) #t) ((:any x) #f)) (class)) #f)
(ut ((fn ((:object x) #t) ((:any x) #f)) ((class))) #t)
(ut ((fn ((:object x) #t) ((:any x) #f)) (interface)) #f)
(ut ((fn ((:object x) #t) ((:any x) #f)) module1) #f)
(ut ((fn ((:object x) #t) ((:any x) #f)) 42) #f)
(ut ((fn ((:class x) #t) ((:any x) #f)) (class)) #t)
(ut ((fn ((:class x) #t) ((:any x) #f)) ((class))) #f)
(ut ((fn ((:class x) #t) ((:any x) #f)) (interface)) #f)
(ut ((fn ((:class x) #t) ((:any x) #f)) module1) #f)
(ut ((fn ((:class x) #t) ((:any x) #f)) 42) #f)
(ut ((fn ((:interface x) #t) ((:any x) #f)) (class)) #f)
(ut ((fn ((:interface x) #t) ((:any x) #f)) ((class))) #f)
(ut ((fn ((:interface x) #t) ((:any x) #f)) (interface)) #t)
(ut ((fn ((:interface x) #t) ((:any x) #f)) module1) #f)
(ut ((fn ((:interface x) #t) ((:any x) #f)) 42) #f)

; I/O
(define file-name (module-path module1))
(ut ((fn ((:port x) #t) ((:any x) #f)) (open-output-file+ file-name)) #t)
(ut ((fn ((:port x) #t) ((:any x) #f)) (open-input-file file-name)) #t)
(ut ((fn ((:port x) #t) ((:any x) #f)) 42) #f)
(ut ((fn ((:inport x) #t) ((:any x) #f)) (open-output-file+ file-name)) #f)
(ut ((fn ((:inport x) #t) ((:any x) #f)) (open-input-file file-name)) #t)
(ut ((fn ((:inport x) #t) ((:any x) #f)) 42) #f)
(ut ((fn ((:outport x) #t) ((:any x) #f)) (open-output-file+ file-name)) #t)
(ut ((fn ((:outport x) #t) ((:any x) #f)) (open-input-file file-name)) #f)
(ut ((fn ((:outport x) #t) ((:any x) #f)) 42) #f)

; Modules
(ut ((fn ((:module x) #t) ((:any x) #f)) module1) #t)
(ut ((fn ((:module x) #t) ((:any x) #f)) file-name) #f)
(ut ((fn ((:module x) #t) ((:any x) #f)) 42) #f)

; Type Aliases
(ut ((fn ((:type-alias x) #t) ((:any x) #f)) (type-alias :int|char)) #t)
(ut ((fn ((:type-alias x) #t) ((:any x) #f)) 42) #f)
(ut ((fn ((:type-alias x) #t) ((:any x) #f)) "42") #f)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONTAINER TESTS
; Lists
(ut ((fn ((:pair x) #t) ((:any x) #f)) (cons 1 2)) #t)
(ut ((fn ((:pair<int> x) #t) ((:any x) #f)) (cons 1 '())) #t) ; "<type>" checks pair contents as a list
(ut ((fn ((:pair<int,int> x) #t) ((:any x) #f)) (cons 1 1)) #t) ; "<type,type>" checks "car" and "cdr"
(ut ((fn ((:pair<int,int> x) #t) ((:any x) #f)) (cons "1" 1)) #f)
(ut ((fn ((:pair<int,int> x) #t) ((:any x) #f)) (cons 1 "1")) #f)
(ut ((fn ((:pair<any,any> x) #t) ((:any x) #f)) (cons 1 "2")) #t)
(ut ((fn ((:pair<any,int> x) #t) ((:any x) #f)) (cons "1" 2)) #t)
(ut ((fn ((:pair<any,int> x) #t) ((:any x) #f)) (cons 2 "1")) #f)
(ut ((fn ((:pair<int,any> x) #t) ((:any x) #f)) (cons 2 "1")) #t)
(ut ((fn ((:pair<int,any> x) #t) ((:any x) #f)) (cons "1" 2)) #f)
(ut ((fn ((:pair x) #t) ((:any x) #f)) 42) #f)
(ut ((fn ((:list x) #t) ((:any x) #f)) '()) #t)
(ut ((fn ((:list x) #t) ((:any x) #f)) (list 1 2 3)) #t)
(ut ((fn ((:list<int> x) #t) ((:any x) #f)) '()) #t)
(ut ((fn ((:list<int> x) #t) ((:any x) #f)) (list 1 2 3)) #t)
(ut ((fn ((:list<int> x) #t) ((:any x) #f)) (list 1 "2" 3)) #f)
(ut ((fn ((:list<int> x) #t) ((:any x) #f)) (list "1" 2 3)) #f)
(ut ((fn ((:list<int> x) #t) ((:any x) #f)) (list 1 2 "3")) #f)
(ut ((fn ((:list x) #t) ((:any x) #f)) 42) #f)

; Fast Containers
(ut ((fn ((:vector x) #t) ((:any x) #f)) []) #t)
(ut ((fn ((:vector x) #t) ((:any x) #f)) (vector 1 2 3)) #t)
(ut ((fn ((:vector<int> x) #t) ((:any x) #f)) []) #t)
(ut ((fn ((:vector<int> x) #t) ((:any x) #f)) (vector 1 2 3)) #t)
(ut ((fn ((:vector<int> x) #t) ((:any x) #f)) (vector 1 "2" 3)) #f)
(ut ((fn ((:vector<int> x) #t) ((:any x) #f)) (vector "1" 2 3)) #f)
(ut ((fn ((:vector<int> x) #t) ((:any x) #f)) (vector 1 2 "3")) #f)
(ut ((fn ((:vector x) #t) ((:any x) #f)) 42) #f)
(ut ((fn ((:map x) #t) ((:any x) #f)) {}) #t)
(ut ((fn ((:map x) #t) ((:any x) #f)) (hashmap 1 2 3 4)) #t)
(ut ((fn ((:map<int> x) #t) ((:any x) #f)) {}) #t)
(ut ((fn ((:map<int> x) #t) ((:any x) #f)) (hashmap 1 2 3 4)) #t)
(ut ((fn ((:map<int> x) #t) ((:any x) #f)) (hashmap 1 "2" 3 4)) #f)
(ut ((fn ((:map<int> x) #t) ((:any x) #f)) (hashmap "1" 2 3 4)) #t) ; "<type>" only checks values
(ut ((fn ((:map<int> x) #t) ((:any x) #f)) (hashmap 1 2 "3" 4)) #t)
(ut ((fn ((:map<int> x) #t) ((:any x) #f)) (hashmap 1 2 3 "4")) #f)
(ut ((fn ((:map<int,string> x) #t) ((:any x) #f)) (hashmap 1 "2" 3 "4")) #t) ; "<type,type>" checks both keys and values
(ut ((fn ((:map<int,string> x) #t) ((:any x) #f)) (hashmap 1 2 3 "4")) #f)
(ut ((fn ((:map<int,string> x) #t) ((:any x) #f)) (hashmap 1 "2" 3 4)) #f)
(ut ((fn ((:map<any,any> x) #t) ((:any x) #f)) (hashmap 1 "2" 3 4)) #t)
(ut ((fn ((:map<any,int> x) #t) ((:any x) #f)) (hashmap "1" 2 "3" 4)) #t)
(ut ((fn ((:map<any,int> x) #t) ((:any x) #f)) (hashmap 2 "1" 4 "3")) #f)
(ut ((fn ((:map<int,any> x) #t) ((:any x) #f)) (hashmap 2 "1" 4 "3")) #t)
(ut ((fn ((:map<int,any> x) #t) ((:any x) #f)) (hashmap "1" 2 "3" 4)) #f)
(ut ((fn ((:map x) #t) ((:any x) #f)) 42) #f)

; Generic Containers
; => ":associative-collection" is aliased by ":ac"
; => ":ordered-collection" is aliased by ":oc"
(ut ((fn ((:associative-collection x) #t) ((:any x) #f)) "") #t) ; aliased by ":ac"
(ut ((fn ((:associative-collection x) #t) ((:any x) #f)) "string") #t)
(ut ((fn ((:associative-collection<char> x) #t) ((:any x) #f)) "string") #t)
(ut ((fn ((:associative-collection<int> x) #t) ((:any x) #f)) "string") #f)
(ut ((fn ((:associative-collection x) #t) ((:any x) #f)) '()) #t)
(ut ((fn ((:associative-collection x) #t) ((:any x) #f)) (list 1 2 3)) #t)
(ut ((fn ((:associative-collection<int> x) #t) ((:any x) #f)) '()) #t)
(ut ((fn ((:associative-collection<int> x) #t) ((:any x) #f)) (list 1 2 3)) #t)
(ut ((fn ((:associative-collection<int> x) #t) ((:any x) #f)) (list 1 "2" 3)) #f)
(ut ((fn ((:associative-collection<int> x) #t) ((:any x) #f)) (list "1" 2 3)) #f)
(ut ((fn ((:associative-collection<int> x) #t) ((:any x) #f)) (list 1 2 "3")) #f)
(ut ((fn ((:associative-collection x) #t) ((:any x) #f)) []) #t)
(ut ((fn ((:associative-collection x) #t) ((:any x) #f)) (vector 1 2 3)) #t)
(ut ((fn ((:associative-collection<int> x) #t) ((:any x) #f)) []) #t)
(ut ((fn ((:associative-collection<int> x) #t) ((:any x) #f)) (vector 1 2 3)) #t)
(ut ((fn ((:associative-collection<int> x) #t) ((:any x) #f)) (vector 1 "2" 3)) #f)
(ut ((fn ((:associative-collection<int> x) #t) ((:any x) #f)) (vector "1" 2 3)) #f)
(ut ((fn ((:associative-collection<int> x) #t) ((:any x) #f)) (vector 1 2 "3")) #f)
(ut ((fn ((:associative-collection x) #t) ((:any x) #f)) {}) #t)
(ut ((fn ((:associative-collection x) #t) ((:any x) #f)) (hashmap 1 2 3 4)) #t)
(ut ((fn ((:associative-collection<int> x) #t) ((:any x) #f)) {}) #t)
(ut ((fn ((:associative-collection<int> x) #t) ((:any x) #f)) (hashmap 1 2 3 4)) #t)
(ut ((fn ((:associative-collection<int> x) #t) ((:any x) #f)) (hashmap 1 "2" 3 4)) #f)
(ut ((fn ((:associative-collection<int> x) #t) ((:any x) #f)) (hashmap "1" 2 3 4)) #t) ; "<type>" only checks values
(ut ((fn ((:associative-collection<int> x) #t) ((:any x) #f)) (hashmap 1 2 "3" 4)) #t)
(ut ((fn ((:associative-collection<int> x) #t) ((:any x) #f)) (hashmap 1 2 3 "4")) #f)
(ut ((fn ((:associative-collection x) #t) ((:any x) #f)) 42) #f)
(ut ((fn ((:ordered-collection x) #t) ((:any x) #f)) "") #t) ; aliased by ":oc"
(ut ((fn ((:ordered-collection x) #t) ((:any x) #f)) "string") #t)
(ut ((fn ((:ordered-collection<char> x) #t) ((:any x) #f)) "string") #t)
(ut ((fn ((:ordered-collection<int> x) #t) ((:any x) #f)) "string") #f)
(ut ((fn ((:ordered-collection x) #t) ((:any x) #f)) '()) #t)
(ut ((fn ((:ordered-collection x) #t) ((:any x) #f)) (list 1 2 3)) #t)
(ut ((fn ((:ordered-collection<int> x) #t) ((:any x) #f)) '()) #t)
(ut ((fn ((:ordered-collection<int> x) #t) ((:any x) #f)) (list 1 2 3)) #t)
(ut ((fn ((:ordered-collection<int> x) #t) ((:any x) #f)) (list 1 "2" 3)) #f)
(ut ((fn ((:ordered-collection<int> x) #t) ((:any x) #f)) (list "1" 2 3)) #f)
(ut ((fn ((:ordered-collection<int> x) #t) ((:any x) #f)) (list 1 2 "3")) #f)
(ut ((fn ((:ordered-collection x) #t) ((:any x) #f)) []) #t)
(ut ((fn ((:ordered-collection x) #t) ((:any x) #f)) (vector 1 2 3)) #t)
(ut ((fn ((:ordered-collection<int> x) #t) ((:any x) #f)) []) #t)
(ut ((fn ((:ordered-collection<int> x) #t) ((:any x) #f)) (vector 1 2 3)) #t)
(ut ((fn ((:ordered-collection<int> x) #t) ((:any x) #f)) (vector 1 "2" 3)) #f)
(ut ((fn ((:ordered-collection<int> x) #t) ((:any x) #f)) (vector "1" 2 3)) #f)
(ut ((fn ((:ordered-collection<int> x) #t) ((:any x) #f)) (vector 1 2 "3")) #f)
(ut ((fn ((:ordered-collection x) #t) ((:any x) #f)) {}) #f)
(ut ((fn ((:ordered-collection x) #t) ((:any x) #f)) 42) #f)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLASS/INTERFACE TESTS
(define-class TestClass
  (:static smethod 
    (fn ((:TestClass c) #t)
        ((:any c) #f)))
  (method 
    (fn ((:TestClass c) #t)
        ((:any c) #f))))
(define-interface TestInterface
  (:static simethod 
    (fn ((:TestInterface c) #t)
        ((:any c) #f))))
(def classObj (TestClass))
(def interfaceObj ((class (:implements TestInterface))))

; Custom classes and interfaces match types according to <oo-is?>
(ut ((fn ((:TestClass x) #t) ((:any x) #f)) classObj) #t)
(ut ((fn ((:TestClass x) #t) ((:any x) #f)) interfaceObj) #f)
(ut ((fn ((:TestInterface x) #t) ((:any x) #f)) interfaceObj) #t)
(ut ((fn ((:TestInterface x) #t) ((:any x) #f)) classObj) #f)
(ut ((fn ((:TestClass x) #t) ((:any x) #f)) 42) #f)
(ut ((fn ((:TestInterface x) #t) ((:any x) #f)) 42) #f)

; Prove inline static and instance methods can reference their class and/or interface as a type
(ut (TestClass.smethod classObj) #t)
(ut (TestClass.smethod interfaceObj) #f)
(ut (classObj.method classObj) #t)
(ut (classObj.method interfaceObj) #f)
(ut (TestInterface.simethod interfaceObj) #t)
(ut (TestInterface.simethod classObj) #f)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UNION TESTS
(ut ((fn ((:number|string|key x) #t) ((:any x) #f)) 42) #t)
(ut ((fn ((:number|string|key x) #t) ((:any x) #f)) "42") #t)
(ut ((fn ((:number|string|key x) #t) ((:any x) #f)) :hi) #t)
(ut ((fn ((:number|string|key x) #t) ((:any x) #f)) []) #f)

(ut ((fn ((:pair<number|string|key> x) #t) ((:any x) #f)) '("1" 1 :hi)) #t)
(ut ((fn ((:pair<number|string|key> x) #t) ((:any x) #f)) '("1" 1 :hi [])) #f)
(ut ((fn ((:pair<number|string|key,number|string|key> x) #t) ((:any x) #f)) '("1" . 1)) #t)
(ut ((fn ((:pair<number|string|key,number|string|key> x) #t) ((:any x) #f)) '(:hi . 1)) #t)
(ut ((fn ((:pair<number|string|key,number|string|key> x) #t) ((:any x) #f)) '(:hi . [])) #f)
(ut ((fn ((:list<number|string|key> x) #t) ((:any x) #f)) '("1" 1 :hi)) #t)
(ut ((fn ((:list<number|string|key> x) #t) ((:any x) #f)) '("1" 1 :hi [])) #f)
(ut ((fn ((:vector<number|string|key> x) #t) ((:any x) #f)) ["1" 1 :hi]) #t)
(ut ((fn ((:vector<number|string|key> x) #t) ((:any x) #f)) ["1" 1 :hi []]) #f)
(ut ((fn ((:map<number|string|key> x) #t) ((:any x) #f)) {"1" 1 [] :hi}) #t)
(ut ((fn ((:map<number|string|key> x) #t) ((:any x) #f)) {"1" 1 :hi []}) #f)
(ut ((fn ((:map<number|string|key,number|string|key> x) #t) ((:any x) #f)) {"1" 1 [] :hi}) #f)
(ut ((fn ((:map<number|string|key,number|string|key> x) #t) ((:any x) #f)) {"1" 1 2 :hi}) #t)

(ut ((fn ((:vector<vector<int>> x) #t) ((:any x) #f)) []) #t)
(ut ((fn ((:vector<vector<int>> x) #t) ((:any x) #f)) [[]]) #t)
(ut ((fn ((:vector<vector<int>> x) #t) ((:any x) #f)) [[] []]) #t)
(ut ((fn ((:vector<vector<int>> x) #t) ((:any x) #f)) [[1 2] [3 4]]) #t)
(ut ((fn ((:vector<vector<int>> x) #t) ((:any x) #f)) [[1 "2"] [3 4]]) #f)
(ut ((fn ((:vector<vector<int>|string> x) #t) ((:any x) #f)) []) #t)
(ut ((fn ((:vector<vector<int>|string> x) #t) ((:any x) #f)) [[]]) #t)
(ut ((fn ((:vector<vector<int>|string> x) #t) ((:any x) #f)) [[] []]) #t)
(ut ((fn ((:vector<vector<int>|string> x) #t) ((:any x) #f)) [[1 2] [3 4] "hi"]) #t)
(ut ((fn ((:vector<vector<int>|string> x) #t) ((:any x) #f)) [[1 "2"] [3 4] :hi]) #f)
(ut ((fn ((:vector<vector<int>|string> x) #t) ((:any x) #f)) [[1 "2"] [3 4] "hi"]) #f)
(ut ((fn ((:vector<vector<int>>|string x) #t) ((:any x) #f)) 42) #f)
(ut ((fn ((:vector<vector<int>>|string x) #t) ((:any x) #f)) "hi") #t)
(ut ((fn ((:vector<vector<int>>|string x) #t) ((:any x) #f)) []) #t)
(ut ((fn ((:vector<vector<int>>|string x) #t) ((:any x) #f)) [[]]) #t)
(ut ((fn ((:vector<vector<int>>|string x) #t) ((:any x) #f)) [[] []]) #t)
(ut ((fn ((:vector<vector<int>>|string x) #t) ((:any x) #f)) [[1 2] [3 4] "hi"]) #f)
(ut ((fn ((:vector<vector<int>>|string x) #t) ((:any x) #f)) [[1 "2"] [3 4] :hi]) #f)
(ut ((fn ((:vector<vector<int>>|string x) #t) ((:any x) #f)) [[1 "2"] [3 4] "hi"]) #f)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LAMBDA PARAMETER SUPPORT (can't check failed type match due to fatal error)
; Required parameters
(ut ((lambda (:int x) x) 42) 42)
(ut ((lambda (:int x :string y) x) 42 "42") 42)

; Optional parameters
(ut ((lambda ((:int x 314)) x)) 314)
(ut ((lambda ((:int x 314)) x) 42) 42)
(ut ((lambda ((:int x 314) (:string y "314")) x)) 314)
(ut ((lambda ((:int x 314) (:string y "314")) x) 42) 42)
(ut ((lambda ((:int x 314) (:string y "314")) x) 42 "hi") 42)

; Mixing required and optional parameters
(ut ((lambda (:int a (:int x 314)) x) 27) 314)
(ut ((lambda (:int a (:int x 314)) x) 27 42) 42)
(ut ((lambda (:int a (:int x 314) (:string y "314")) x) 27) 314)
(ut ((lambda (:int a (:int x 314) (:string y "314")) x) 27 42) 42)
(ut ((lambda (:int a (:int x 314) (:string y "314")) x) 27 42 "hi") 42)

; Variadic parameter
(ut ((lambda (:int x . xs) xs) 42) '())
(ut ((lambda (:int x . xs) xs) 42 1 2 3) '(1 2 3))
(ut ((lambda ((:int x 314) . xs) xs)) '())
(ut ((lambda ((:int x 314) . xs) xs) 42) '())
(ut ((lambda ((:int x 314) . xs) xs) 42 1 2 3) '(1 2 3))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FN PARAMETER SUPPORT
; Required parameters
(ut ((fn ((:int x) x) ((:any x) #f)) 42) 42)
(ut ((fn ((:int x) x) ((:any x) #f)) "42") #f)
(ut ((fn ((:int x :string y) x) ((:any x :any y) #f)) 42 "hi") 42)
(ut ((fn ((:int x :string y) x) ((:any x :any y) #f)) 42 :hi) #f)

; Optional parameters
(ut ((fn (((:int x 314)) x))) 314)
(ut ((fn (((:int x 314)) x) (((:any x #f)) #f)) 42) 42)
(ut ((fn (((:int x 314)) x) (((:any x #f)) #f)) "42") #f)
(ut ((fn (((:int x 314) (:string y "314")) x) (((:any x #f) (:any y #f)) #f))) 314)
(ut ((fn (((:int x 314) (:string y "314")) x) (((:any x #f) (:any y #f)) #f)) 42) 42)
(ut ((fn (((:int x 314) (:string y "314")) x) (((:any x #f) (:any y #f)) #f)) 42 "hi") 42)
(ut ((fn (((:int x 314) (:string y "314")) x) (((:any x #f) (:any y #f)) #f)) 42 :hi) #f)

; Mixing required and optional parameters
(ut ((fn ((:int a (:int x 314)) x) ((:any a) #f)) 27) 314)
(ut ((fn ((:int a (:int x 314)) x) ((:any a :any x) #f)) 27 42) 42)
(ut ((fn ((:int a (:int x 314)) x) ((:any a :any x) #f)) 27 "42") #f)
(ut ((fn ((:int a (:int x 314) (:string y "314")) x) ((:any a :any x :any y) #f)) 27) 314)
(ut ((fn ((:int a (:int x 314) (:string y "314")) x) ((:any a :any x :any y) #f)) 27 42) 42)
(ut ((fn ((:int a (:int x 314) (:string y "314")) x) ((:any a :any x :any y) #f)) 27 42 "hi") 42)
(ut ((fn ((:int a (:int x 314) (:string y "314")) x) ((:any a :any x :any y) #f)) 27 42 :hi) #f)

; Variadic parameter
(ut ((fn ((:int x . xs) xs) ((:any x) #f)) 42) '())
(ut ((fn ((:int x . xs) xs) ((:any x) #f)) "42") #f)
(ut ((fn ((:int x . xs) xs) ((:any a :any b :any c :any d) #f)) 42 1 2 3) '(1 2 3))
(ut ((fn ((:int x . xs) xs) ((:any a :any b :any c :any d) #f)) "42" 1 2 3) #f)
(ut ((fn (((:int x 314) . xs) xs))) '())
(ut ((fn (((:int x 314) . xs) xs) ((:any x) #f)) 42) '())
(ut ((fn (((:int x 314) . xs) xs) ((:any x) #f)) "42") #f)
(ut ((fn (((:int x 314) . xs) xs) ((:any a :any b :any c :any d) #f)) 42 1 2 3) '(1 2 3))
(ut ((fn (((:int x 314) . xs) xs) ((:any a :any b :any c :any d) #f)) "42" 1 2 3) #f)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINE PARAMETER SUPPORT (inline procedure syntax)
; Required parameters
(ut ((begin (define (test-function :int x) x) test-function) 42) 42)
(ut ((begin (define (test-function :int x :string y) x) test-function) 42 "42") 42)

; Optional parameters
(ut ((begin (define (test-function (:int x 314)) x) test-function)) 314)
(ut ((begin (define (test-function (:int x 314)) x) test-function) 42) 42)
(ut ((begin (define (test-function (:int x 314) (:string y "314")) x) test-function)) 314)
(ut ((begin (define (test-function (:int x 314) (:string y "314")) x) test-function) 42) 42)
(ut ((begin (define (test-function (:int x 314) (:string y "314")) x) test-function) 42 "hi") 42)

; Mixing required and optional parameters
(ut ((begin (define (test-function :int a (:int x 314)) x) test-function) 27) 314)
(ut ((begin (define (test-function :int a (:int x 314)) x) test-function) 27 42) 42)
(ut ((begin (define (test-function :int a (:int x 314) (:string y "314")) x) test-function) 27) 314)
(ut ((begin (define (test-function :int a (:int x 314) (:string y "314")) x) test-function) 27 42) 42)
(ut ((begin (define (test-function :int a (:int x 314) (:string y "314")) x) test-function) 27 42 "hi") 42)

; Variadic parameter
(ut ((begin (define (test-function :int x . xs) xs) test-function) 42) '())
(ut ((begin (define (test-function :int x . xs) xs) test-function) 42 1 2 3) '(1 2 3))
(ut ((begin (define (test-function (:int x 314) . xs) xs) test-function)) '())
(ut ((begin (define (test-function (:int x 314) . xs) xs) test-function) 42) '())
(ut ((begin (define (test-function (:int x 314) . xs) xs) test-function) 42 1 2 3) '(1 2 3))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFN PARAMETER SUPPORT
; Required parameters
(ut ((begin (defn test-function ((:int x) x) ((:any x) #f)) test-function) 42) 42)
(ut ((begin (defn test-function ((:int x) x) ((:any x) #f)) test-function) "42") #f)
(ut ((begin (defn test-function ((:int x :string y) x) ((:any x :any y) #f)) test-function) 42 "hi") 42)
(ut ((begin (defn test-function ((:int x :string y) x) ((:any x :any y) #f)) test-function) 42 :hi) #f)

; Optional parameters
(ut ((begin (defn test-function (((:int x 314)) x)) test-function)) 314)
(ut ((begin (defn test-function (((:int x 314)) x) (((:any x #f)) #f)) test-function) 42) 42)
(ut ((begin (defn test-function (((:int x 314)) x) (((:any x #f)) #f)) test-function) "42") #f)
(ut ((begin (defn test-function (((:int x 314) (:string y "314")) x) (((:any x #f) (:any y #f)) #f)) test-function)) 314)
(ut ((begin (defn test-function (((:int x 314) (:string y "314")) x) (((:any x #f) (:any y #f)) #f)) test-function) 42) 42)
(ut ((begin (defn test-function (((:int x 314) (:string y "314")) x) (((:any x #f) (:any y #f)) #f)) test-function) 42 "hi") 42)
(ut ((begin (defn test-function (((:int x 314) (:string y "314")) x) (((:any x #f) (:any y #f)) #f)) test-function) 42 :hi) #f)

; Mixing required and optional parameters
(ut ((begin (defn test-function ((:int a (:int x 314)) x) ((:any a) #f)) test-function) 27) 314)
(ut ((begin (defn test-function ((:int a (:int x 314)) x) ((:any a :any x) #f)) test-function) 27 42) 42)
(ut ((begin (defn test-function ((:int a (:int x 314)) x) ((:any a :any x) #f)) test-function) 27 "42") #f)
(ut ((begin (defn test-function ((:int a (:int x 314) (:string y "314")) x) ((:any a :any x :any y) #f)) test-function) 27) 314)
(ut ((begin (defn test-function ((:int a (:int x 314) (:string y "314")) x) ((:any a :any x :any y) #f)) test-function) 27 42) 42)
(ut ((begin (defn test-function ((:int a (:int x 314) (:string y "314")) x) ((:any a :any x :any y) #f)) test-function) 27 42 "hi") 42)
(ut ((begin (defn test-function ((:int a (:int x 314) (:string y "314")) x) ((:any a :any x :any y) #f)) test-function) 27 42 :hi) #f)

; Variadic parameter
(ut ((begin (defn test-function ((:int x . xs) xs) ((:any x) #f)) test-function) 42) '())
(ut ((begin (defn test-function ((:int x . xs) xs) ((:any x) #f)) test-function) "42") #f)
(ut ((begin (defn test-function ((:int x . xs) xs) ((:any a :any b :any c :any d) #f)) test-function) 42 1 2 3) '(1 2 3))
(ut ((begin (defn test-function ((:int x . xs) xs) ((:any a :any b :any c :any d) #f)) test-function) "42" 1 2 3) #f)
(ut ((begin (defn test-function (((:int x 314) . xs) xs)) test-function)) '())
(ut ((begin (defn test-function (((:int x 314) . xs) xs) ((:any x) #f)) test-function) 42) '())
(ut ((begin (defn test-function (((:int x 314) . xs) xs) ((:any x) #f)) test-function) "42") #f)
(ut ((begin (defn test-function (((:int x 314) . xs) xs) ((:any a :any b :any c :any d) #f)) test-function) 42 1 2 3) '(1 2 3))
(ut ((begin (defn test-function (((:int x 314) . xs) xs) ((:any a :any b :any c :any d) #f)) test-function) "42" 1 2 3) #f)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINE-GENERATOR PARAMETER SUPPORT
; Required parameters
(ut (((begin (define-generator (test-function :int x) x) test-function) 42)) 42)
(ut (((begin (define-generator (test-function :int x :string y) x) test-function) 42 "42")) 42)

; Optional parameters
(ut (((begin (define-generator (test-function (:int x 314)) x) test-function))) 314)
(ut (((begin (define-generator (test-function (:int x 314)) x) test-function) 42)) 42)
(ut (((begin (define-generator (test-function (:int x 314) (:string y "314")) x) test-function))) 314)
(ut (((begin (define-generator (test-function (:int x 314) (:string y "314")) x) test-function) 42)) 42)
(ut (((begin (define-generator (test-function (:int x 314) (:string y "314")) x) test-function) 42 "hi")) 42)

; Mixing required and optional parameters
(ut (((begin (define-generator (test-function :int a (:int x 314)) x) test-function) 27)) 314)
(ut (((begin (define-generator (test-function :int a (:int x 314)) x) test-function) 27 42)) 42)
(ut (((begin (define-generator (test-function :int a (:int x 314) (:string y "314")) x) test-function) 27)) 314)
(ut (((begin (define-generator (test-function :int a (:int x 314) (:string y "314")) x) test-function) 27 42)) 42)
(ut (((begin (define-generator (test-function :int a (:int x 314) (:string y "314")) x) test-function) 27 42 "hi")) 42)

; Variadic parameter
(ut (((begin (define-generator (test-function :int x . xs) xs) test-function) 42)) '())
(ut (((begin (define-generator (test-function :int x . xs) xs) test-function) 42 1 2 3)) '(1 2 3))
(ut (((begin (define-generator (test-function (:int x 314) . xs) xs) test-function))) '())
(ut (((begin (define-generator (test-function (:int x 314) . xs) xs) test-function) 42)) '())
(ut (((begin (define-generator (test-function (:int x 314) . xs) xs) test-function) 42 1 2 3)) '(1 2 3))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CURRY PARAMETER SUPPORT (can't check failed type match due to fatal error)
(ut ((curry (:int a) a) 42) 42)
(ut ((curry (:int a :string b :key c) c) 42 "hi" :key) :key)
(ut (((curry (:int a :string b :key c) c) 42) "hi" :key) :key)
(ut (((curry (:int a :string b :key c) c) 42 "hi") :key) :key)
(ut ((((curry (:int a :string b :key c) c) 42) "hi") :key) :key)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLASS METHOD PARAMETER SUPPORT (instance and static methods)
(define-class TestClass
  ((method1 :int a) a)
  ((method2 :int a . xs) xs)
  ((method3 :int a :string b) b)
  ((method4 (:int a 314)) a)
  ((method5 (:int a 314) (:string b "hi")) b)
  ((method6 :key z (:int a 314)) a)
  ((method7 :key z (:int a 314) (:string b "hi")) b)
  (:static (method1 :int a) a)
  (:static (method2 :int a . xs) xs)
  (:static (method3 :int a :string b) b)
  (:static (method4 (:int a 314)) a)
  (:static (method5 (:int a 314) (:string b "hi")) b)
  (:static (method6 :key z (:int a 314)) a)
  (:static (method7 :key z (:int a 314) (:string b "hi")) b))

(def testObj (TestClass))

(ut (testObj.method1 42) 42)
(ut (testObj.method2 42 1 2 3) '(1 2 3))
(ut (testObj.method3 42 "hello") "hello")
(ut (testObj.method4) 314)
(ut (testObj.method5 42) "hi")
(ut (testObj.method6 :hi) 314)
(ut (testObj.method7 :hi 42) "hi")
(ut (TestClass.method1 42) 42)
(ut (TestClass.method2 42 1 2 3) '(1 2 3))
(ut (TestClass.method3 42 "hello") "hello")
(ut (TestClass.method4) 314)
(ut (TestClass.method5 42) "hi")
(ut (TestClass.method6 :hi) 314)
(ut (TestClass.method7 :hi 42) "hi")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERFACE PARAMETER METHOD SUPPORT (static methods)
(define-interface TestInterface
  (:static (method1 :int a) a)
  (:static (method2 :int a . xs) xs)
  (:static (method3 :int a :string b) b)
  (:static (method4 (:int a 314)) a)
  (:static (method5 (:int a 314) (:string b "hi")) b)
  (:static (method6 :key z (:int a 314)) a)
  (:static (method7 :key z (:int a 314) (:string b "hi")) b))

(ut (TestInterface.method1 42) 42)
(ut (TestInterface.method2 42 1 2 3) '(1 2 3))
(ut (TestInterface.method3 42 "hello") "hello")
(ut (TestInterface.method4) 314)
(ut (TestInterface.method5 42) "hi")
(ut (TestInterface.method6 :hi) 314)
(ut (TestInterface.method7 :hi 42) "hi")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FN RETURN SUPPORT
(ut ((fn (:int () 42))) 42)
(ut ((fn "docstring" (:int () 42))) 42)
(ut ((fn (:int () 42) (:string (x) x)) "hello") "hello")
(ut ((fn "docstring" (:int () 42) (:string (x) x)) "hello") "hello")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFN RETURN SUPPORT
(ut ((begin (defn test-function (:int () 42)) test-function)) 42)
(ut ((begin (defn test-function "docstring" (:int () 42)) test-function)) 42)
(ut ((begin (defn test-function (:int () 42) (:string (x) x)) test-function) "hello") "hello")
(ut ((begin (defn test-function "docstring" (:int () 42) (:string (x) x)) test-function) "hello") "hello")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LAMBDA RETURN SUPPORT
(ut ((lambda :int () 42)) 42)
(ut ((lambda :int () "docstring" 42)) 42)
(ut ((lambda :string (x) x) "hello") "hello")
(ut ((lambda :string (x) "docstring" x) "hello") "hello")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINE RETURN SUPPORT (inline procedure syntax)
(ut ((begin (define :int (test-function) 42) test-function)) 42)
(ut ((begin (define :int (test-function) "docstring" 42) test-function)) 42)
(ut ((begin (define :string (test-function x) x) test-function) "hello") "hello")
(ut ((begin (define :string (test-function x) "docstring" x) test-function) "hello") "hello")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CURRY RETURN SUPPORT (only type-checks once all parameters are given)
(ut ((curry :int () 42)) 42)
(ut ((curry :int () "docstring" 42)) 42)
(ut ((curry :int (a) a) 42) 42)
(ut ((curry :int (a) "docstring" a) 42) 42)
(ut ((curry :int (a b) b) 42 314) 314)
(ut ((curry :int (a b) "docstring" b) 42 314) 314)
(ut (((curry :int (a b) b) 42) 314) 314)
(ut (((curry :int (a b) "docstring" b) 42) 314) 314)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLASS METHOD RETURN SUPPORT (instance and static methods)
(define-class TestClass
  (:int (method1 a) "docstring" a)
  (:list (method2 :int a . xs) xs)
  (:string (method3 a b) b)
  (:int (method4 (a 314)) a)
  (:string (method5 (a 314) (b "hi")) b)
  (:int (method6 z (a 314)) a)
  (:string (method7 z (a 314) (b "hi")) b)
  (:static :int (method1 a) "docstring" a)
  (:static :list (method2 a . xs) xs)
  (:static :string (method3 a b) b)
  (:static :int (method4 (a 314)) a)
  (:static :string (method5 (a 314) (b "hi")) b)
  (:static :int (method6 z (a 314)) a)
  (:static :string (method7 z (a 314) (b "hi")) b))

(def testObj (TestClass))

(ut (testObj.method1 42) 42)
(ut (testObj.method2 42 1 2 3) '(1 2 3))
(ut (testObj.method3 42 "hello") "hello")
(ut (testObj.method4) 314)
(ut (testObj.method5 42) "hi")
(ut (testObj.method6 :hi) 314)
(ut (testObj.method7 :hi 42) "hi")
(ut (TestClass.method1 42) 42)
(ut (TestClass.method2 42 1 2 3) '(1 2 3))
(ut (TestClass.method3 42 "hello") "hello")
(ut (TestClass.method4) 314)
(ut (TestClass.method5 42) "hi")
(ut (TestClass.method6 :hi) 314)
(ut (TestClass.method7 :hi 42) "hi")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERFACE RETURN METHOD SUPPORT (static methods)
(define-interface TestInterface
  (:static :int (method1 a) "docstring" a)
  (:static :list (method2 a . xs) xs)
  (:static :string (method3 a b) b)
  (:static :int (method4 (a 314)) a)
  (:static :string (method5 (a 314) (b "hi")) b)
  (:static :int (method6 z (a 314)) a)
  (:static :string (method7 z (a 314) (b "hi")) b))

(ut (TestInterface.method1 42) 42)
(ut (TestInterface.method2 42 1 2 3) '(1 2 3))
(ut (TestInterface.method3 42 "hello") "hello")
(ut (TestInterface.method4) 314)
(ut (TestInterface.method5 42) "hi")
(ut (TestInterface.method6 :hi) 314)
(ut (TestInterface.method7 :hi 42) "hi")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODULE TYPE DISPATCH FOR CLASSES AND INTERFACES
(import examples.types.objects)

; Module class dispatch
(ut ((fn ((:objects.Person p) #t) ((x) #f)) 42) #f)
(ut ((fn ((:objects.Person p) #t) ((x) #f)) (objects.Person "John" 42)) #t)

; Module interface dispatch
(ut ((fn ((:objects.IPerson p) #t) ((x) #f)) 42) #f)
(ut ((fn ((:objects.IPerson p) #t) ((x) #f)) (objects.Person "John" 42)) #t)

; Module alias dispatch
(ut ((fn ((:objects.personalias p) #t) ((x) #f)) 42) #f)
(ut ((fn ((:objects.personalias p) #t) ((x) #f)) (objects.Person "John" 42)) #t)
