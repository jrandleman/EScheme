;; Author: Jordan Randleman -- stdlib.scm
;; => Defines EScheme's Special Forms & Native Standard Library

;; => IMPORTANT: MUST _NEVER_ STORE A CONTINUATION UPON INITIAL INTERPRETATION BY
;;               THE VM: CAN _ONLY_ DEFINE SPECIAL FORMS, PROCEDURES, & VARIABLES
;;               * This has to do with how we evaluate the stdlib prior receiving 
;;                 user code.

;; => IMPORTANT: IN ORDER FOR CHANGES IN THIS FILE TO AFFECT THE ESCHEME RUNTIME
;;               (IF IT WAS SERIALIZED), 1 OF 2 ACTIONS MUST BE TAKEN:
;;                 1) RUN ESCHEME 1NC WITH THE --serialize-stdlib COMMAND-LINE FLAG
;;                    ___OR___
;;                 2) REINSTALL ESCHEME VIA "installer/Installer.java"

; PROVIDES:
;   - quote
;   - define-syntax
;   - fn
;   - lambda
;   - if
;   - begin
;   - set!
;   - define def
;   - defined? def?
;   - defn
;
;   - and
;   - or
;   - delay
;   - cond
;   - let
;   - quasiquote
;   - case
;   - let*
;   - letrec
;   - letrec*
;   - unless
;   - while
;   - do
;
;   - define-parameter
;   - parameter?
;
;   - -<>
;   - curry
;
;   - *dosync-lock*
;   - *dosync-module-lock*
;   - dosync
;   - dosync-module
;   - dosync-with
;
;   - guard
;
;   - call/cc [alias]
;
;   - pprint [alias]
;   - pprintf [alias]
;   - pprint-to-string [alias]
;
;   - thread-define
;   - thread-set!
;   - thread-get
;   - thread-defined?
;
;   - scons
;   - stream-pair?
;   - stream?
;   - sc**r ... sc****r
;   - stream-ref
;   - stream->list
;   - stream-map
;   - stream-filter
;   - stream-iterate
;   - stream-constant
;   - stream-append
;   - stream-interleave
;   - stream->generator
;
;   - *generator-complete*
;   - define-generator 
;   - yield
;   - complete-all-generators!
;   - complete-n-generators!
;
;   - class
;   - define-class
;   - interface
;   - define-interface
;   - super! ; MUST BE CALLED AS THE FIRST LINE IN CONSTRUCTORS TO AVOID UNDEFINED BEHAVIOR
;
;   - import
;   - reload
;   - from

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing QUOTE: (quote <obj>)

; (define-syntax quote
;   (lambda (x)
;     (list 
;       (quote bytecode)
;       (list
;         (if (symbol? x) 
;             (quote load-symbol) 
;             (quote load))
;         x))))

(bytecode
  (load-closure 
    ((x)
      (push x)
      (push x)
      (push symbol?)
      (call -2)
      (ifn 3)
      (load-symbol load-symbol)
      (jump 2)
      (load-symbol load)
      (push)
      (push list)
      (call -3)
      (push)
      (load-symbol bytecode)
      (push)
      (push list)
      (call -3)))
  (push)
  (load-symbol quote)
  (push)
  (push escm-define-syntax)
  (call -3))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing DEFINE-SYNTAX: (define-syntax <name> <procedure>)

; (define-syntax define-syntax
;   (lambda (name procedure)
;     (list (quote escm-define-syntax) (list (quote quote) name) procedure)))

(bytecode
  (load-closure 
    ((name procedure)
      (push procedure)
      (push name)
      (load-symbol quote)
      (push)
      (push list)
      (call -3)
      (push)
      (load-symbol escm-define-syntax)
      (push)
      (push list)
      (call -4)))
  (push)
  (load-symbol define-syntax)
  (push)
  (push escm-define-syntax)
  (call -3))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing FN: (fn ((<param> ...) <body> ...) ...)
;;   => NOTE: Supports optional parameters via "(<param> <dflt-value>)" syntax

(bytecode

  ; (define (escm-fn-has-optional-parameters? params)
  ;   (if (pair? params) 
  ;       (if (list? params) 
  ;           (pair? (last params))
  ;           #f) 
  ;       #f))

  (load-closure 
    ((params)
      (push params)
      (push pair?)
      (call -2)
      (ifn 14)
      (push params)
      (push list?)
      (call -2)
      (ifn 8)
      (push params)
      (push last)
      (call -2)
      (push)
      (push pair?)
      (call -2)
      (jump 2)
      (load #f)
      (jump 2)
      (load #f)))
  (define escm-fn-has-optional-parameters?)

  ; (define (escm-fn-generate-expanded-clauses clause)
  ;   (define params (car clause))
  ;   (define body (cdr clause))
  ;   (define req-params (filter atom? params))
  ;   (define opt-params (filter pair? params))
  ;   (define param-defs (map (lambda (p) (cons (quote define) p)) opt-params))
  ;   (define n (length opt-params)) 
  ;   (define expanded-clauses (quote ()))
  ;   (define opt-param-names (map car opt-params))
  ;   (while ((>= n 0) expanded-clauses)
  ;     (set! expanded-clauses
  ;       (cons 
  ;         (cons 
  ;           (append req-params (slice opt-param-names 0 n)) 
  ;           (append (slice param-defs n) body))
  ;         expanded-clauses))
  ;     (set! n (- n 1))))

  (load-closure 
    ((clause)
      (push clause)
      (push car)
      (call -2)
      (define params)
      (push clause)
      (push cdr)
      (call -2)
      (define body)
      (push params)
      (push atom?)
      (push filter)
      (call -3)
      (define req-params)
      (push params)
      (push pair?)
      (push filter)
      (call -3)
      (define opt-params)
      (push opt-params)
      (load-closure 
        ((p) (push p) (load-symbol define) (push) (push cons) (call -3)))
      (push)
      (push map)
      (call -3)
      (define param-defs)
      (push opt-params)
      (push length)
      (call -2)
      (define n)
      (load ())
      (define expanded-clauses)
      (push opt-params)
      (push car)
      (push map)
      (call -3)
      (define opt-param-names)
      (push 0)
      (push n)
      (push >=)
      (call -3)
      (ifn 33)
      (push expanded-clauses)
      (push body)
      (push n)
      (push param-defs)
      (push slice)
      (call -3)
      (push)
      (push append)
      (call -3)
      (push)
      (push n)
      (push 0)
      (push opt-param-names)
      (push slice)
      (call -4)
      (push)
      (push req-params)
      (push append)
      (call -3)
      (push)
      (push cons)
      (call -3)
      (push)
      (push cons)
      (call -3)
      (set! expanded-clauses)
      (push 1)
      (push n)
      (push -)
      (call -3)
      (set! n)
      (jump -36)
      (load #void)
      (load expanded-clauses)))
  (define escm-fn-generate-expanded-clauses)

  ; (define (escm-fn-expand-clause clause)
  ;   (if (escm-fn-has-optional-parameters? (car clause))
  ;       (escm-fn-generate-expanded-clauses clause)
  ;       (list clause)))

  (load-closure 
    ((clause)
      (push clause)
      (push car)
      (call -2)
      (push)
      (push escm-fn-has-optional-parameters?)
      (call -2)
      (ifn 5)
      (push clause)
      (push escm-fn-generate-expanded-clauses)
      (call -2)
      (jump 4)
      (push clause)
      (push list)
      (call -2)))
  (define escm-fn-expand-clause)

  ; (define-syntax fn
  ;   (lambda clauses
  ;     (set! clauses (apply append (map escm-fn-expand-clause clauses)))
  ;     (list 
  ;       (quote bytecode)
  ;       (cons 
  ;         (quote load-closure)
  ;         (map
  ;           (lambda (c)
  ;             (cons 
  ;               (car c)
  ;               (apply append (map compile (cdr c)))))
  ;           clauses)))))

  (load-closure 
    (clauses (push clauses)
      (push escm-fn-expand-clause)
      (push map)
      (call -3)
      (push)
      (push append)
      (push apply)
      (call -3)
      (set! clauses)
      (push clauses)
      (load-closure 
        ((c)
          (push c)
          (push cdr)
          (call -2)
          (push)
          (push compile)
          (push map)
          (call -3)
          (push)
          (push append)
          (push apply)
          (call -3)
          (push)
          (push c)
          (push car)
          (call -2)
          (push)
          (push cons)
          (call -3)))
      (push)
      (push map)
      (call -3)
      (push)
      (load-symbol load-closure)
      (push)
      (push cons)
      (call -3)
      (push)
      (load-symbol bytecode)
      (push)
      (push list)
      (call -3)))
  (push)
  (load-symbol fn)
  (push)
  (push escm-define-syntax)
  (call -3))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing LAMBDA: (lambda (<param> ...) <body> ...)
(define-syntax lambda
  (fn ((params . body)
        (list (quote fn) (cons params body)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing IF: (if <condition> <consequence> <alternative>) (if <condition> <consequence>)
(define-syntax if
  (fn ((condition consequence)
        (bytecode
          (push #void)
          (push compile)
          (call -2)
          (define compiled-alternative)
          (push consequence)
          (push compile)
          (call -2)
          (define compiled-consequence))
        (append
          (cons 
            (quote bytecode) 
            (compile condition))
          (cons 
            (list (quote ifn) (+ 2 (length compiled-consequence)))
            compiled-consequence)
          (cons
            (list (quote jump) (+ 1 (length compiled-alternative)))
            compiled-alternative)))
      ((condition consequence alternative)
        (bytecode
          (push alternative)
          (push compile)
          (call -2)
          (define compiled-alternative)
          (push consequence)
          (push compile)
          (call -2)
          (define compiled-consequence))
        (append
          (cons 
            (quote bytecode) 
            (compile condition))
          (cons 
            (list (quote ifn) (+ 2 (length compiled-consequence)))
            compiled-consequence)
          (cons
            (list (quote jump) (+ 1 (length compiled-alternative)))
            compiled-alternative)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing BEGIN: (begin <expr> ...)
(define-syntax begin
  (lambda (. exprs)
    (cons 
      (quote bytecode)
      (apply append (map compile exprs)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing SET!: (set! <var> <value>)
(define-syntax set!
  (lambda (var val)
    (append 
      (cons (quote bytecode)
            (compile val))
      (list (list (quote set!) var)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing DEFINE (aliased by DEF): (define <var> <value>) (define (<fcn-name> <param> ...) <body> ...)
(define-syntax define
  (lambda (bindings . vals)
    (cons 
      (quote bytecode)
      (if (symbol? bindings)
          (append
            (compile (car vals))
            (list (list (quote define) bindings)))
          (append 
            (compile
              (cons 
                (quote lambda)
                (cons 
                  (cdr bindings)
                  vals)))
            (list (list (quote define) (car bindings))))))))

(define def define)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing DEFINED? (aliased by DEF?): (defined? <var>)
(define-syntax defined?
  (lambda (var)
    (list (quote bytecode) (list (quote defined?) var))))

(define def? defined?)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing DEFN: (defn <fcn-name> ((<param> ...) <body> ...) ...)
(define-syntax defn
  (lambda (name . clauses)
    (list (quote define) name (cons (quote fn) clauses))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing Boolean AND:  (and <obj> ...)
(define-syntax and
  (lambda (. conditions)
    (fold (lambda (acc item) (list (quote if) acc item #f))
          #t
          conditions)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing Boolean OR: (or <obj> ...)
(define-syntax or
  (lambda (. conditions)
    (define or-value (gensym 'or-value))
    (fold-right (lambda (item acc)
                  (list (list (quote lambda) (list or-value)
                              (list (quote if) or-value or-value acc))
                        item))
                #f
                conditions)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing DELAY: (delay <obj>)
(define-syntax delay
  (lambda (x)
    (define forced? (gensym 'delay-forced?))
    (define result (gensym 'delay-result))
    (list (quote let) (list (list forced? #f) (list result #f))
      (list (quote lambda) (quote ())
        (list (quote if) forced?
            result
            (list (quote begin)
              (list (quote set!) forced? #t)
              (list (quote set!) result x)
              result))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing COND: (cond (<condition> <expr> ...) ...) 
;;                    (cond (<condition> <expr> ...) ... (<condition> => <callable>) ...) 
;;                    (cond (<condition> <expr> ...) ... (else <expr> ...))
(define-syntax cond
  (lambda (. clauses)
    (define (make-condition c) (if (eq? c (quote else)) #t c))
    (define (make-consequence c) (cons (quote begin) c))
    (define (arrow-syntax? c) (and (= (length c) 3) (eq? (quote =>) (cadr c))))
    (define (arrow->let c a) 
      (define condition-result (gensym 'cond-result))
      (list (quote let) (list (list condition-result (car c)))
        (list (quote if) condition-result
            (list (caddr c) condition-result)
            a)))
    (fold-right (lambda (clause acc)
                  (if (arrow-syntax? clause)
                      (arrow->let clause acc)
                      (list (quote if) (make-condition (car clause))
                            (make-consequence (cdr clause))
                            acc)))
                (quote (if #f #f)) ; inner expression yields #void
                clauses)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing LET: (let ((<var> <value>) ...) <body> ...) (let <fcn-name> ((<param> <initial-value>) ...) <body> ...)
(define-syntax let
  (lambda (bindings . body)
    (define (get-params let-bindings) (map car let-bindings))
    (define (get-args let-bindings) (map cadr let-bindings))
    (define (get-body let-body) (cons (quote begin) let-body))
    (define (generate-anon-let)
      (cons (list (quote lambda) (get-params bindings)
                  (get-body body))
            (get-args bindings)))
    (define (generate-named-let)
      (list (list (quote lambda) (quote ())
              (list (quote begin)
                (list (quote define) bindings
                      (list (quote lambda) (get-params (car body))
                            (get-body (cdr body))))
                (cons bindings (get-args (car body)))))))
    (if (symbol? bindings)
        (generate-named-let)
        (generate-anon-let))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing QUASIQUOTE: (quasiquote <obj>) (unquote <obj>) (unquote-splicing <obj>)
(define (escm-quasiquote-tagged-list? obj tag)
  (and (eq? (car obj) tag) (not (null? (cdr obj)))))

(define (escm-quasiquote->quote lst level)
  (define (iter lst)
    (define hd (if (not (atom? lst)) (car lst)))
          ; finished parsing expression (proper list)
    (cond ((null? lst) (quote ()))
          ; quasiquote vector
          ((vector? lst)
            (list (list (quote list->vector) (escm-quasiquote->quote (vector->list lst) level))))
          ; quasiquote hashmap
          ((hashmap? lst)
            (list (list (quote list->hashmap) (escm-quasiquote->quote (hashmap->list lst) level))))
          ; finished parsing expression (dotted list)
          ((atom? lst)
            (list (list (quote quote) lst)))
          ; unquote rest of list
          ((escm-quasiquote-tagged-list? lst (quote unquote))
            (if (= level 0)
                (list (cadr lst))
                (list (list (quote list) (quote (quote unquote)) (escm-quasiquote->quote (cadr lst) (- level 1)))))) ; *there*: recursively parse, in nested quasiquote
          ; quasiquote vector
          ((vector? hd)
            (cons (list (quote list) (list (quote list->vector) (escm-quasiquote->quote (vector->list hd) level)))
                  (iter (cdr lst))))
          ; quasiquote hashmap
          ((hashmap? hd)
            (cons (list (quote list) (list (quote list->hashmap) (escm-quasiquote->quote (hashmap->list hd) level)))
                  (iter (cdr lst))))
          ; quote atom
          ((atom? hd)
            (cons (list (quote list) (list (quote quote) hd))
                  (iter (cdr lst))))
          ; unquote datum
          ((escm-quasiquote-tagged-list? hd (quote unquote))
            (if (= level 0)
                (cons (list (quote list) (cadr hd))
                      (iter (cdr lst)))
                (cons (list (quote list) (escm-quasiquote->quote hd level)) ; recursively parse, in nested quasiquote (level will be decremented *there*)
                      (iter (cdr lst)))))
          ; unquote & signal should splice element
          ((escm-quasiquote-tagged-list? hd (quote unquote-splicing))
            (if (= level 0)
                (cons (cadr hd) ; evaluate datum & append to the expression
                      (iter (cdr lst)))
                (cons (list (quote list) (escm-quasiquote->quote hd (- level 1))) ; recursively parse, in nested quasiquote
                      (iter (cdr lst)))))
          ; nested quasiquote
          ((escm-quasiquote-tagged-list? hd (quote quasiquote))
            (cons (list (quote list) (escm-quasiquote->quote hd (+ level 1))) ; recursively parse, in nested quasiquote
                  (iter (cdr lst))))
          ; quasiquote expression
          (else
            (cons (list (quote list) (escm-quasiquote->quote hd level))
                  (iter (cdr lst))))))
  (cons (quote append) (iter lst)))

(define-syntax quasiquote
  (lambda (x) (escm-quasiquote->quote x 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing CASE: (case <value> ((<key> ...) <expr> ...) ...) 
;;                    (case <value> ((<key> ...) <expr> ...) ... (else <expr> ...))
(define-syntax case
  (lambda (value . clauses)
    (define (rest-of-clause c)
      (if (and (= (length c) 3) (eq? (cadr c) (quote =>)))
          (cdr c)
          (list (cons (quote begin) (cdr c)))))
    (define cached-value (gensym 'case-key-value))
      (define converted-clauses
        (map (lambda (c) 
              (if (list? (car c))
                  (cons (list (quote member) cached-value (cons (quote list) (car c))) 
                        (rest-of-clause c))
                  c))
             clauses))
      (list (quote let) (list (list cached-value value))
        (cons (quote cond) converted-clauses))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing LET*: (let* ((<var> <value>) ...) <body> ...)
(define-syntax let*
  (lambda (bindings . body)
    (fold-right (lambda (binding acc) (list (quote let) (list binding) acc))
                (cons (quote begin) body)
                bindings)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing LETREC: (letrec ((<var> <value>) ...) <body> ...)
(define-syntax letrec
  (lambda (bindings . body)
    (append 
      (cons (quote let) 
        (cons (map (lambda (param) (list param #f)) (map car bindings))
            (map (lambda (binding) (cons (quote set!) binding)) bindings)))
      body)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing LETREC*: (letrec* ((<var> <value>) ...) <body> ...)
(define-syntax letrec*
  (lambda (bindings . body)
    (fold-right (lambda (binding acc) (list (quote letrec) (list binding) acc))
                (cons (quote begin) body)
                bindings)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing UNLESS: (unless <condition> <body> ...)
(define-syntax unless
  (lambda (condition . body)
    (list 'if condition
         #void
         (cons 'begin body))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing WHILE: (while (<condition> <return-expr> ...) <body> ...)
(define-syntax while
  (lambda (condition-returns . body)
    (define compiled-condition (compile (car condition-returns)))
    (define compiled-body (apply append (map compile body)))
    (define compiled-returns (apply append (map compile (cdr condition-returns))))
    (append
      (cons 'bytecode compiled-condition)
      (append
        (cons (list 'ifn (+ (length compiled-body) 2)) compiled-body)
        (append 
          (list
            (list 'jump (- 0 (length compiled-condition) (length compiled-body) 1))
            (list 'load #void))
          compiled-returns)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing DO: (do ((<var> <initial-val> <update-expr>) ...) ; contents are optional 
;;                      (<break-condition> <return-expr> ...)     ; contents are optional 
;;                      <body> ...)                               ; body is optional 
(define-syntax do
  (lambda (var-bindings break-returns . body)
    (define vars (map car var-bindings))
    (define vals (map cadr var-bindings))
    (define updates 
      (map (lambda (e) (list 'set! (car e) (caddr e)))
           (filter (lambda (e) (= (length e) 3)) var-bindings)))
    (define break-cond
      (if (pair? break-returns)
          (car break-returns)
          #f))
    (define return-exprs
      (if (pair? break-returns)
          (cdr break-returns)
          (quote ())))
    (define loop-procedure-name (gensym 'do-loop-name))
    (list 'letrec (list (list loop-procedure-name 
                (list 'lambda vars
                  (list 'if break-cond
                      (cons 'begin return-exprs)
                      (append (cons 'begin body) updates (list (cons loop-procedure-name vars)))))))
              (cons loop-procedure-name vals))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing DEFINE-PARAMETER: 
;; (define-parameter <symbol> <obj>)
(define-syntax define-parameter
  (lambda (sym val)
    (list 'escm-define-parameter (list 'quote sym) val)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing PARAMETER?: 
;; (parameter? <symbol>)
(define-syntax parameter?
  (lambda (sym )
    (list 'escm-parameter? (list 'quote sym))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing -<>: (-<> <expression> ...)
;;   => Example: (-<> (* 2 2) (+ <> <>) (* <> <>)) ; => 64
(define-syntax -<> ; Note: the "<>" value is cached!
  (fn
    ((a) a)
    ((a op) (list (list (quote lambda) (list (quote <>)) op) a))
    ((a op . ops) 
      (cons (quote -<>) (cons (list (list (quote lambda) (list (quote <>)) op) a) ops)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing CURRY: (curry (<param> ...) <body> ...)
;;   => APPLICATION: Both ((K 1) 2) and (K 1 2) are valid!
;;   => NOTE: It is UNDEFINED BEHAVIOR to have a VARIADIC CURRIED lambda
;;            IE: (curry (x . xs) x) ; INVALID!
(define-syntax curry 
  (lambda (params . body)
    (define curried-lambdas (gensym 'curry-lambdas))
    (cond ((null? params) 
            (cons 'lambda (cons '() body)))
          ((null? (cdr params))
            (list 'lambda '(x . xs)
              (list 'fold '(lambda (f a) (f a)) 
                    (cons 'lambda (cons params body))
                    '(cons x xs))))
          (else
            (list 'let (list (list curried-lambdas 
              (list 'lambda (list (car params)) (cons 'curry (cons (cdr params) body)))))
                (list 'lambda '(x . xs)
                  (list 'fold '(lambda (f a) (f a)) 
                        curried-lambdas
                        '(cons x xs))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing DOSYNC
(if (not (parameter? *dosync-lock*)) 
    (define-parameter *dosync-lock* (mutex "*dosync-lock*")))

(define-syntax dosync
  (lambda (. exprs)
    (list 'dynamic-wind
      '(lambda () (mutex-lock! *dosync-lock*))
      (cons 'lambda (cons '() exprs))
      '(lambda () (mutex-unlock! *dosync-lock*)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing DOSYNC-MODULE
(define *dosync-module-lock* (mutex "*dosync-module-lock*"))

(define-syntax dosync-module
  (lambda (. exprs)
    (list 'dynamic-wind
      '(lambda () (mutex-lock! *dosync-module-lock*))
      (cons 'lambda (cons '() exprs))
      '(lambda () (mutex-unlock! *dosync-module-lock*)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing DOSYNC-WITH
(define-syntax dosync-with 
  (lambda (lock-expr . exprs)
    (define lock (gensym 'dosync-with-lock))
    (list 'begin
      (list 'define lock lock-expr) ; cache the lock!
      (list 'dynamic-wind
        (list 'lambda '() (list 'mutex-lock! lock))
        (cons 'lambda (cons '() exprs))
        (list 'lambda '() (list 'mutex-unlock! lock))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing GUARD (exception handling macro)
;;   See the below for an implementation of <with-exception-handler>, <raise>, & <guard>:
;;     https://srfi.schemers.org/srfi-34/srfi-34.html
(define (escm-guard-aux-parse-else reraise expr)
  (cons 'begin (cdar expr)))

(define (escm-guard-aux-parse-test reraise expr)
  (define test (caar expr))
  (define results (cdar expr))
  (define clauses (cdr expr))
  (define temp (gensym 'guard-temp))
  (list 'let (list (list temp test))
    (list 'if temp
        (if (null? results) temp (cons 'begin results))
        (if (null? clauses) reraise (cons 'escm-guard-aux (cons reraise clauses))))))

(define-syntax escm-guard-aux
  (lambda (reraise . exprs)
    (if (eq? (caar exprs) 'else)
        (escm-guard-aux-parse-else reraise exprs)
        (escm-guard-aux-parse-test reraise exprs))))

(define-syntax guard 
  (lambda (var-and-clauses . exprs) ; (guard (var clause ...) e1 e2 ...)
    (define var (car var-and-clauses))
    (define clauses (cdr var-and-clauses))
    (define guard-k (gensym 'guard-k))
    (define condition (gensym 'guard-condition))
    (define handler-k (gensym 'guard-handler-k))
    (define args (gensym 'guard-args))
    (list (list 'call-with-current-continuation
       (list 'lambda (list guard-k)
         (list 'with-exception-handler
          (list 'lambda (list condition)
            (list (list 'call-with-current-continuation
               (list 'lambda (list handler-k)
                 (list guard-k
                  (list 'lambda '()
                    (list 'let (list (list var condition)) ; clauses may SET! var
                      (cons 'escm-guard-aux 
                        (cons 
                          (list handler-k (list 'lambda '() (list 'raise condition))) 
                          clauses)))))))))
          (list 'lambda '()
            (list 'call-with-values
             (cons 'lambda (cons '() exprs))
             (list 'lambda args
               (list guard-k (list 'lambda '() (list 'apply 'values args))))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aliasing <call-with-current-continuation>
(define call/cc call-with-current-continuation)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aliasing <pretty-print>, <pretty-printf>, & <pretty-print-to-string>
(define pprint pretty-print)

(define pprintf pretty-printf)

(define pprint-to-string pretty-print-to-string)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THREADING Dynamic Environment Macros
(define-syntax thread-define
  (fn ((var val) (list 'thread-define' (list 'quote var) val))
      ((thread var val) (list 'thread-define' thread (list 'quote var) val))))


(define-syntax thread-set!
  (fn ((var val) (list 'thread-set!' (list 'quote var) val))
      ((thread var val) (list 'thread-set!' thread (list 'quote var) val))))


(define-syntax thread-get
  (fn ((var) (list 'thread-get' (list 'quote var)))
      ((thread var) (list 'thread-get' thread (list 'quote var)))))


(define-syntax thread-defined?
  (fn ((var) (list 'thread-defined?' (list 'quote var)))
      ((thread var) (list 'thread-defined?' thread (list 'quote var)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing STREAMS
(define-syntax scons 
  (lambda (a b)
    (list (quote cons) 
          (list (quote delay) a) 
          (list (quote delay) b))))

(define (stream-pair? o)
  (and (pair? o) (procedure? (car o)) (procedure? (cdr o))))

(define (stream? o)
  (or (null? o) (stream-pair? o)))

(define (scar spair) (force (car spair)))
(define (scdr spair) (force (cdr spair)))
(define (scaar spair) (scar (scar spair)))
(define (scadr spair) (scar (scdr spair)))
(define (scdar spair) (scdr (scar spair)))
(define (scddr spair) (scdr (scdr spair)))
(define (scaaar spair) (scar (scar (scar spair))))
(define (scaadr spair) (scar (scar (scdr spair))))
(define (scadar spair) (scar (scdr (scar spair))))
(define (scaddr spair) (scar (scdr (scdr spair))))
(define (scdaar spair) (scdr (scar (scar spair))))
(define (scdadr spair) (scdr (scar (scdr spair))))
(define (scddar spair) (scdr (scdr (scar spair))))
(define (scdddr spair) (scdr (scdr (scdr spair))))
(define (scaaaar spair) (scar (scar (scar (scar spair)))))
(define (scaaadr spair) (scar (scar (scar (scdr spair)))))
(define (scaadar spair) (scar (scar (scdr (scar spair)))))
(define (scaaddr spair) (scar (scar (scdr (scdr spair)))))
(define (scadaar spair) (scar (scdr (scar (scar spair)))))
(define (scadadr spair) (scar (scdr (scar (scdr spair)))))
(define (scaddar spair) (scar (scdr (scdr (scar spair)))))
(define (scadddr spair) (scar (scdr (scdr (scdr spair)))))
(define (scdaaar spair) (scdr (scar (scar (scar spair)))))
(define (scdaadr spair) (scdr (scar (scar (scdr spair)))))
(define (scdadar spair) (scdr (scar (scdr (scar spair)))))
(define (scdaddr spair) (scdr (scar (scdr (scdr spair)))))
(define (scddaar spair) (scdr (scdr (scar (scar spair)))))
(define (scddadr spair) (scdr (scdr (scar (scdr spair)))))
(define (scdddar spair) (scdr (scdr (scdr (scar spair)))))
(define (scddddr spair) (scdr (scdr (scdr (scdr spair)))))

(define (stream->list s list-length) ; facilitates printing stream contents
  (if (>= list-length 0)
      (cons (scar s) (stream->list (scdr s) (- list-length 1)))
      (quote ())))

(define (stream-ref s index)
  (if (= 0 index)
      (scar s)
      (stream-ref (scdr s) (- index 1))))

(define (stream-map callable . streams)
  (define (stream-map streams)
    (if (null? (car streams))
        '()
        (scons
          (apply callable (map scar streams))
          (stream-map (map scdr streams)))))
  (stream-map streams))

(define (stream-filter ? s)
  (cond ((null? s) (quote ()))
        ((? (scar s)) (scons (scar s) (stream-filter ? (scdr s))))
        (else (stream-filter ? (scdr s)))))

(define (stream-iterate update-callable seed)
    (scons seed (stream-iterate update-callable (update-callable seed))))

(define (stream-constant . objs)
  (define (stream-constant obj-list)
    (if (null? obj-list)
        (stream-constant objs)
        (scons (car obj-list) (stream-constant (cdr obj-list)))))
  (if (null? objs)
      '()
      (stream-constant objs)))

(define (stream-append s . streams)
  (define (stream-append s streams)
    (if (null? s)
        (if (null? streams)
            '()
            (stream-append (car streams) (cdr streams)))
        (scons (scar s) (stream-append (scdr s) streams))))
  (if (null? streams) s (stream-append s streams)))

(define (stream-interleave stream1 stream2)
  (if (null? stream1)
      stream2
      (scons (scar stream1) (stream-interleave stream2 (scdr stream1)))))

(define (stream->generator stream-obj)
  (define s (scons #f stream-obj))
  (lambda ()
    (if (null? s)
        *generator-complete*
        (begin 
          (set! s (scdr s))
          (scar s)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing GENERATORS
(define *generator-complete* '*generator-complete*)

(define (escm-generator? obj)
  (and (pair? obj) 
       (pair? (car obj))
       (eq? 'escm-generator (caar obj))
       (procedure? (cdr obj))))

(define-syntax yield 
  (fn (()
        (define k (gensym 'yield-k))
        (list 'set! 'escm-generator-escape 
          (list 'call/cc 
            (list 'lambda (list k)
              (list 'escm-generator-escape 
                (list 'cons 
                  (list 'cons ''escm-generator #void)
                  (list 'lambda '() 
                    (list 'call/cc (list 'lambda '(escm-generator-escape) (list k 'escm-generator-escape))))))))))
      ((yielded)
        (define k (gensym 'yield-k))
        (list 'set! 'escm-generator-escape 
          (list 'call/cc 
            (list 'lambda (list k)
              (list 'escm-generator-escape 
                (list 'cons 
                  (list 'cons ''escm-generator yielded)
                  (list 'lambda '() 
                    (list 'call/cc (list 'lambda '(escm-generator-escape) (list k 'escm-generator-escape))))))))))))

(define-syntax define-generator 
  (lambda (bindings . body)
    (define generator-object (gensym 'define-generator-object))
    (list 'define bindings
      (list 'define generator-object
        (list 'cons
          (list 'cons ''escm-generator #f)
          (list 'lambda '()
            (list 'call/cc (cons 'lambda (cons '(escm-generator-escape) body))))))
      (list 'lambda '()
        (list 'if (list 'escm-generator? generator-object)
            (list 'begin 
              (list 'set! generator-object (list (list 'cdr generator-object)))
              (list 'if (list 'escm-generator? generator-object)
                  (list 'cdar generator-object)
                  generator-object))
            *generator-complete*)))))

(define (complete-all-generators! . generator-objects)
  (let loop ()
    (if (not (fold (lambda (acc result) (and acc (eq? result *generator-complete*)))
                   #t
                   (map (lambda (g) (g)) generator-objects)))
        (loop))))

(define (complete-n-generators! n . generator-objects)
  (define total-generator-objects (length generator-objects))
  (let loop ()
    (define count 0)
    (if (not (fold (lambda (acc result) 
                      (if (eq? result *generator-complete*)
                          (set! count (+ count 1)))
                      (or acc (>= count n) (= count total-generator-objects)))
                   #f
                   (map (lambda (g) (g)) generator-objects)))
        (loop))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing CLASS: 
;;   (class (:extends <super>) (:implements <interface> ...) <prop> ...)
;;
;;   <prop> ::= (<name> <value>)
;;            | ((<method-name> <param> ...) <body> ...)
;;            | (:static <name> <value>)
;;            | (:static (<method-name> <param> ...) <body> ...)
;;
;;   <super> MUST be an expression that evals to a class type.
;;   <interface> MUST be an expression that evals to an interface type.

; Returns the super object if exists, else #f 
(define (escm-oo-get-class-super x)
  (if (pair? x) ; if extending first
      (if (eq? :extends (caar x))
          (cadar x)
          (if (and (pair? (cdr x)) (eq? :extends (caadr x))) ; if extending after implementing
              (car (cdadr x))
              #f))
      #f))

; Returns the list of implemented interfaces if exists, else NIL
(define (escm-oo-get-class-interfaces x)
  (if (pair? x) ; if implementing first
      (if (eq? :implements (caar x))
          (cdar x)
          (if (and (pair? (cdr x)) (eq? :implements (caadr x))) ; if implementing after extending
              (cdadr x)
              '()))
      '()))

; Returns the list of static & instance properties
(define (escm-oo-get-class-properties x)
  (if (pair? x)
      (if (or (eq? :implements (caar x)) (eq? :extends (caar x)))
          (if (and (pair? (cdr x)) (or (eq? :implements (caadr x)) (eq? :extends (caadr x))))
              (cddr x)
              (cdr x))
          x)
      '()))

; Return whether the property (stripped of :static) is an inlined method
(define (escm-oo-class-inlined-method? property)
  (pair? (car property)))

; Given an inlined method property (stripped of :static), returns it expanded as a lambda property
(define (escm-oo-class-expand-inlined-method inlined-method-property)
  (list (caar inlined-method-property) 
    (cons 'lambda 
      (cons
        (cdar inlined-method-property) 
        (cdr inlined-method-property)))))

; Given a list of properties stripped of :static, convert all inlined methods to lambdas
(define (escm-oo-class-expand-property-inlined-methods properties)
  (map 
    (lambda (property) 
      (if (escm-oo-class-inlined-method? property)
          (escm-oo-class-expand-inlined-method property)
          property))
    properties))

; Determine if the given property is static
(define (escm-oo-static-property? property)
  (and (pair? property) (eq? (car property) :static)))

; Returns a list of static properties stripped of :static, else NIL
;   => Also converts inlined methods to lambdas!
(define (escm-oo-get-class-static-properties properties)
  (escm-oo-class-expand-property-inlined-methods
    (map cdr (filter escm-oo-static-property? properties))))

; Returns a list of non-static properties, else NIL
;   => Also converts inlined methods to lambdas!
(define (escm-oo-get-class-non-static-properties properties)
  (escm-oo-class-expand-property-inlined-methods
    (filter (lambda (property) (not (escm-oo-static-property? property))) properties)))

; Determine if given property is the class constructor
(define (escm-oo-class-constructor? property)
  (eq? (car property) 'new))

; Get property list w/o the constructor
(define (escm-oo-get-class-instance-properties instance-properties)
  (filter (lambda (property) (not (escm-oo-class-constructor? property))) instance-properties))

; Get class constructor function
(define (escm-oo-get-class-constructor-procedure instance-properties)
  (define ctor-list (filter escm-oo-class-constructor? instance-properties))
  (if (null? ctor-list)
      #f
      (cadar ctor-list)))

; Get property names as a list of quoted symbols
(define (escm-oo-get-property-names properties)
  (map (lambda (p) (list 'quote (car p))) properties))

; Get property values
(define (escm-oo-get-property-values properties)
  (map cadr properties))

; <class> macro to create anonymous classes!
(define-syntax class
  (lambda x
    ; Extract "is-a"'s
    (define class-super (escm-oo-get-class-super x))
    (define class-interfaces (escm-oo-get-class-interfaces x))
    ; Extract "has-a"'s
    (define class-properties (escm-oo-get-class-properties x))
    (define class-non-static-properties (escm-oo-get-class-non-static-properties class-properties))
    (define class-static-properties (escm-oo-get-class-static-properties class-properties))
    (define class-instance-properties (escm-oo-get-class-instance-properties class-non-static-properties))
    (define class-constructor-procedure (escm-oo-get-class-constructor-procedure class-non-static-properties))
    ; Generate the call to an internal primitive that makes our class!
    (list 'escm-oo-class class-super (cons 'list class-interfaces)
      class-constructor-procedure 
      (cons 'list (escm-oo-get-property-names class-static-properties))
      (cons 'list (escm-oo-get-property-values class-static-properties))
      (cons 'list (escm-oo-get-property-names class-instance-properties))
      (cons 'list (escm-oo-get-property-values class-instance-properties)))))

; <define-class> macro to bind names to anonymous classes!
(define-syntax define-class
  (lambda (name . class-components)
    (define obj (gensym (symbol-append name '? '-obj)))
    (list 'begin
      (list 'define (list (symbol-append name '?) obj) (list 'and (list 'object? obj) (list 'oo-is? obj name))) ; predicate generation!
      (list 'define name (cons 'class class-components)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing INTERFACE: 
;;   (interface (:extends <interface> ...) <prop> ...)
;;
;;   <prop> ::= <name>
;;            | (:static <name> <value>)
;;            | (:static (<method-name> <param> ...) <body> ...)
;;
;;   <interface> MUST be an expression that evals to an interface type.

; Returns the list of implemented interfaces if exists, else NIL
(define (escm-oo-get-interface-interfaces x)
  (if (and (pair? x) (pair? (car x)) (eq? :extends (caar x)))
      (cdar x)
      '()))

; Returns the list of static & instance properties
(define (escm-oo-get-interface-properties x)
  (if (pair? x)
      (if (and (pair? (car x)) (eq? :extends (caar x)))
          (cdr x)
          x)
      '()))

; Returns a list of non-static properties, else NIL
(define (escm-oo-get-interface-instance-properties properties)
  (filter (lambda (property) (not (escm-oo-static-property? property))) properties))

; <interface> macro to create anonymous interfaces!
(define-syntax interface
  (lambda x
    ; Extract "is-a"'s
    (define interface-interfaces (escm-oo-get-interface-interfaces x))
    ; Extract "has-a"'s
    (define interface-properties (escm-oo-get-interface-properties x))
    (define interface-static-properties (escm-oo-get-class-static-properties interface-properties))
    (define interface-instance-properties (escm-oo-get-interface-instance-properties interface-properties))
    ; Generate the call to an internal primitive that makes our interface!
    (list 'escm-oo-interface (cons 'list interface-interfaces)
                             (cons 'list (escm-oo-get-property-names interface-static-properties))
                             (cons 'list (escm-oo-get-property-values interface-static-properties))
                             (cons 'list (map (lambda (p) (list 'quote p)) interface-instance-properties)))))

; <define-interface> macro to bind names to anonymous interfaces!
(define-syntax define-interface
  (lambda (name . interface-components)
    (define obj (gensym (symbol-append name '? '-obj)))
    (list 'begin
      (list 'define (list (symbol-append name '?) obj) (list 'and (list 'object? obj) (list 'oo-is? obj name))) ; predicate generation!
      (list 'define name (cons 'interface interface-components)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing SUPER!: 
;;   Helper macro to initialize an object's super object.
;;
;;   NOTE: _MUST_ BE USED AS THE _FIRST_ STATEMENT IN OBJECT CONSTRUCTORS.
;;          -:- -:- ANY OTHER USE RESULTS IN UNDEFINED BEHAVIOR -:- -:-
;;

; (super! <param> ...)
(define-syntax super!
  (lambda params
    (cons 'set! (cons 'super (list (cons 'escm-oo-super! (cons 'self params)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing IMPORT: 
;;   (import <module-path-symbol>)
;;   (import <module-path-symbol> :as <module-alias-symbol>)
;;   (import <filepath-string> <module-path-symbol>)
;;   (import <filepath-string> <module-path-symbol> :as <module-alias-symbol>)
(define-syntax import
  (fn ((module-path)
        (list 'define (escm-get-module-name module-path) (list 'escm-load-module (list 'quote module-path))))
      ((filepath-string module-path)
        (list 'define (escm-get-module-name module-path) (list 'escm-load-module filepath-string (list 'quote module-path))))
      ((module-path as-keyword module-alias)
        (list 'define module-alias (list 'escm-load-module (list 'quote module-path))))
      ((filepath-string module-path as-keyword module-alias)
        (list 'define module-alias (list 'escm-load-module filepath-string (list 'quote module-path))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing RELOAD: 
;;   (reload <module-alias-symbol>)
(define-syntax reload
  (lambda (module-alias-symbol)
    (list 'set! module-alias-symbol (list 'escm-reload-module module-alias-symbol))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing FROM: 
;; (from <module-path-symbol> :import <obj1-symbol> <obj2-symbol> ...)
;; (from <module-path-symbol> :import <obj1-symbol> <obj2-symbol> ... :as <alias1-symbol> <alias2-symbol> ...)
;; (from <filepath-string> <module-path-symbol> :import <obj1-symbol> <obj2-symbol> ...)
;; (from <filepath-string> <module-path-symbol> :import <obj1-symbol> <obj2-symbol> ... :as <alias1-symbol> <alias2-symbol> ...)
(define (escm-from-parse-arguments a b c xs)
  (if (string? a)
      (list a b xs)
      (list #f a (cons c xs))))

(define (escm-from-get-objects-and-aliases xs)
  (let ((objs '()) (aliases '()) (prior-as? #t))
    (for-each
      (lambda (x)
        (cond ((eq? x :as) (set! prior-as? #f))
              (prior-as? (set! objs (cons x objs)))
              (else (set! aliases (cons x aliases)))))
      xs)
    (if (null? aliases)
        (cons objs objs)
        (cons objs aliases))))

(define-syntax from
  (lambda (a b c . xs)
    (define args (escm-from-parse-arguments a b c xs))
    (let ((filepath-string (car args)) (module-path (cadr args)) (objs-and-aliases (caddr args)))
      (define fields (escm-from-get-objects-and-aliases objs-and-aliases))
      (define hidden-module-name (gensym))
      (cons 'begin
        (cons
          (list 
            'define 
            hidden-module-name 
            (cons 'escm-load-module 
              (if filepath-string 
                  (list filepath-string (list 'quote module-path)) 
                  (list (list 'quote module-path)))))
          (map 
            (lambda (obj alias)
              (list 'define alias (symbol-append hidden-module-name '. obj)))
            (car fields) 
            (cdr fields)))))))
