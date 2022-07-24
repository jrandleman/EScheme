;; Author: Jordan Randleman -- stdlib.scm
;; => Defines EScheme's Special Forms & Native Standard Library

;; => IMPORTANT: MUST _NEVER_ STORE A CONTINUATION UPON INITIAL INTERPRETATION BY
;;               THE VM: CAN _ONLY_ DEFINE SPECIAL FORMS, PROCEDURES, & VARIABLES

; PROVIDES:
;   - quote
;   - define-syntax
;   - fn
;   - lambda
;   - if
;   - begin
;   - set!
;   - define def
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
;   - while
;
;   - *dosync-lock*
;   - dosync
;   - dosync-with
;
;   - guard
;
;   - call/cc [alias]
;
;   - pprint [alias]
;
;   - thread-define
;   - thread-set!
;   - thread-get
;
;   - scons
;   - stream-pair?
;   - stream?
;   - sc**r ... sc****r
;   - stream->list
;   - stream-map
;   - stream-filter
;   - stream-ref
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
      (push list)
      (load-symbol bytecode)
      (push)
      (push list)
      (push symbol?)
      (push x)
      (call 2)
      (ifn 3)
      (load-symbol load-symbol)
      (jump 2)
      (load-symbol load)
      (push)
      (push x)
      (call 3)
      (push)
      (call 3)))
  (define-syntax quote))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing DEFINE-SYNTAX: (define-syntax <name> <procedure>)

; (define-syntax define-syntax
;   (lambda (name procedure)
;     (cons 
;       (quote bytecode)
;       (append 
;         (compile procedure)
;         (list (list (quote define-syntax) name))))))

(bytecode
  (load-closure 
    ((name procedure)
      (push cons)
      (load-symbol bytecode)
      (push)
      (push append)
      (push compile)
      (push procedure)
      (call 2) ; compile
      (push)
      (push list)
      (push list)
      (load-symbol define-syntax)
      (push)
      (push name)
      (call 3) ; list
      (push)
      (call 2) ; list
      (push)
      (call 3) ; append
      (push)
      (call 3))) ; cons
  (define-syntax define-syntax))


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
      (push pair?)
      (push params)
      (call 2)
      (ifn 14)
      (push list?)
      (push params)
      (call 2)
      (ifn 8)
      (push pair?)
      (push last)
      (push params)
      (call 2)
      (push)
      (call 2)
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
  ;           (append req-params (sublist opt-param-names 0 n)) 
  ;           (append (sublist param-defs n) body))
  ;         expanded-clauses))
  ;     (set! n (- n 1))))

  (load-closure 
    ((clause)
      (push car)
      (push clause)
      (call 2)
      (define params)
      (push cdr)
      (push clause)
      (call 2)
      (define body)
      (push filter)
      (push atom?)
      (push params)
      (call 3)
      (define req-params)
      (push filter)
      (push pair?)
      (push params)
      (call 3)
      (define opt-params)
      (push map)
      (load-closure ((p) (push cons) (load-symbol define) (push) (push p) (call 3)))
      (push)
      (push opt-params)
      (call 3)
      (define param-defs)
      (push length)
      (push opt-params)
      (call 2)
      (define n)
      (load ())
      (define expanded-clauses)
      (push map)
      (push car)
      (push opt-params)
      (call 3)
      (define opt-param-names)
      (push >=)
      (push n)
      (push 0)
      (call 3)
      (ifn 33)
      (push cons)
      (push cons)
      (push append)
      (push req-params)
      (push sublist)
      (push opt-param-names)
      (push 0)
      (push n)
      (call 4)
      (push)
      (call 3)
      (push)
      (push append)
      (push sublist)
      (push param-defs)
      (push n)
      (call 3)
      (push)
      (push body)
      (call 3)
      (push)
      (call 3)
      (push)
      (push expanded-clauses)
      (call 3)
      (set! expanded-clauses)
      (push -)
      (push n)
      (push 1)
      (call 3)
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
      (push escm-fn-has-optional-parameters?)
      (push car)
      (push clause)
      (call 2)
      (push)
      (call 2)
      (ifn 5)
      (push escm-fn-generate-expanded-clauses)
      (push clause)
      (call 2)
      (jump 4)
      (push list)
      (push clause)
      (call 2)))
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
    (clauses
      (push apply)
      (push append)
      (push map)
      (push escm-fn-expand-clause)
      (push clauses)
      (call 3)
      (push)
      (call 3)
      (set! clauses)
      (push list)
      (load-symbol bytecode)
      (push)
      (push cons)
      (load-symbol load-closure)
      (push)
      (push map)
      (load-closure 
        ((c)
          (push cons)
          (push car)
          (push c)
          (call 2)
          (push)
          (push apply)
          (push append)
          (push map)
          (push compile)
          (push cdr)
          (push c)
          (call 2)
          (push)
          (call 3)
          (push)
          (call 3)
          (push)
          (call 3)))
      (push)
      (push clauses)
      (call 3)
      (push)
      (call 3)
      (push)
      (call 3)))
  (define-syntax fn))


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
          (push compile)
          (push #void)
          (call 2)
          (define compiled-alternative)
          (push compile)
          (push consequence)
          (call 2)
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
          (push compile)
          (push alternative)
          (call 2)
          (define compiled-alternative)
          (push compile)
          (push consequence)
          (call 2)
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

(define-syntax def
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
    (define or-value (gensym))
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
    (define forced? (gensym))
    (define result (gensym))
    (list (quote let) (list (list forced? #f) (list result #f))
      (list (quote lambda) (quote ())
        (list (quote if) forced?
            result
            (list (quote begin)
              (list (quote set!) forced? #t)
              (list (quote set!) result x)
              result))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing COND: (cond (<condition> <expr> ...) ...) (cond (<condition> <expr> ...) ... (else <expr> ...))
(define-syntax cond
  (lambda (. clauses)
    (define (make-condition c) (if (eq? c (quote else)) #t c))
    (define (make-consequence c) (cons (quote begin) c))
    (fold-right (lambda (clause acc)
                  (list (quote if) (make-condition (car clause))
                        (make-consequence (cdr clause))
                        acc))
                (quote (if #f #f)) ; innermost expr yields a <void> value
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
          ; finished parsing expression (dotted list)
          ((atom? lst)
            (list (list (quote quote) lst)))
          ; unquote rest of list
          ((escm-quasiquote-tagged-list? lst (quote unquote))
            (if (= level 0)
                (list (cadr lst))
                (list (list (quote list) (quote (quote unquote)) (escm-quasiquote->quote (cadr lst) (- level 1)))))) ; *there*: recursively parse, in nested quasiquote
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
;; Implementing CASE: (case <value> ((<key> ...) <expr> ...) ...) (case <value> ((<key> ...) <expr> ...) ... (else <expr> ...))
(define-syntax case
  (lambda (value . clauses)
    (define (make-consequence c) (cons (quote begin) c))
    (define (make-condition c) 
      (if (eq? c (quote else)) 
          #t 
          (list (quote member) value (cons (quote list) c))))
    (fold-right (lambda (clause acc)
                  (list (quote if) (make-condition (car clause))
                        (make-consequence (cdr clause))
                        acc))
                (quote (if #f #f)) ; innermost expr yields a <void> value
                clauses)))


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
;; Implementing WHILE: (while (<condition> <return-expr> ...) <body> ...)
(define-syntax while
  (lambda (condition-returns . body)
    (define compiled-condition (compile (car condition-returns)))
    (define compiled-body (apply append (map compile body)))
    (define compiled-returns (apply append (map compile (cdr condition-returns))))
    `(bytecode 
      ,@compiled-condition
      (ifn ,(+ (length compiled-body) 2))
      ,@compiled-body
      (jump ,(- 0 (length compiled-condition) (length compiled-body) 1))
      (load #void)
      ,@compiled-returns)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing DOSYNC
(define *dosync-lock* (mutex "*dosync-lock*"))

(define-syntax dosync
  (lambda (. exprs)
    `(dynamic-wind
      (lambda () (mutex-lock! *dosync-lock*))
      (lambda () ,@exprs)
      (lambda () (mutex-unlock! *dosync-lock*)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing DOSYNC-WITH
(define-syntax dosync-with 
  (lambda (lock-expr . exprs)
    (define lock (gensym))
    `(begin
      (define ,lock ,lock-expr) ; cache the lock!
      (dynamic-wind
        (lambda () (mutex-lock! ,lock))
        (lambda () ,@exprs)
        (lambda () (mutex-unlock! ,lock))))))


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
  (define temp (gensym))
  `(let ((,temp ,test))
    (if ,temp
        ,(if (null? results) temp (cons 'begin results))
        ,(if (null? clauses) reraise (cons 'escm-guard-aux (cons reraise clauses))))))

(define-syntax escm-guard-aux
  (lambda (reraise . exprs)
    (if (eq? (caar exprs) 'else)
        (escm-guard-aux-parse-else reraise exprs)
        (escm-guard-aux-parse-test reraise exprs))))

(define-syntax guard 
  (lambda (var-and-clauses . exprs) ; (guard (var clause ...) e1 e2 ...)
    (define var (car var-and-clauses))
    (define clauses (cdr var-and-clauses))
    (define guard-k (gensym))
    (define condition (gensym))
    (define handler-k (gensym))
    (define args (gensym))
    `((call-with-current-continuation
       (lambda (,guard-k)
         (with-exception-handler
          (lambda (,condition)
            ((call-with-current-continuation
               (lambda (,handler-k)
                 (,guard-k
                  (lambda ()
                    (let ((,var ,condition)) ; clauses may SET! var
                      (escm-guard-aux 
                        (,handler-k (lambda () (raise ,condition))) 
                        ,@clauses))))))))
          (lambda ()
            (call-with-values
             (lambda () ,@exprs)
             (lambda ,args
               (,guard-k (lambda () (apply values ,args))))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aliasing <call-with-current-continuation>
(define call/cc call-with-current-continuation)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aliasing <pretty-print>
(define pprint pretty-print)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THREADING Dynamic Environment Macros
(define-syntax thread-define
  (fn ((var val) `(thread-define' ',var ,val))
      ((thread var val) `(thread-define' ,thread ',var ,val))))


(define-syntax thread-set!
  (fn ((var val) `(thread-set!' ',var ,val))
      ((thread var val) `(thread-set!' ,thread ',var ,val))))


(define-syntax thread-get
  (fn ((var) `(thread-get' ',var))
      ((thread var) `(thread-get' ,thread ',var))))


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

(define (stream-map f s)
  (if (null? s)
      (quote ())
      (scons (f (scar s))
             (stream-map f (scdr s)))))

(define (stream-filter ? s)
  (cond ((null? s) (quote ()))
        ((? (scar s)) (scons (scar s) (stream-filter ? (scdr s))))
        (else (stream-filter ? (scdr s)))))

(define (stream-ref s index)
  (if (= 0 index)
      (scar s)
      (stream-ref (scdr s) (- index 1))))

(define (stream->generator stream-obj)
  (define s (scons #f stream-obj))
  (lambda ()
    (set! s (scdr s))
    (scar s)))


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
        (define k (gensym))
        `(set! escm-generator-escape 
          (call/cc 
            (lambda (,k)
              (escm-generator-escape 
                (cons 
                  (cons 'escm-generator #void)
                  (lambda () 
                    (call/cc (lambda (escm-generator-escape) (,k escm-generator-escape))))))))))
      ((yielded)
        (define k (gensym))
        `(set! escm-generator-escape 
          (call/cc 
            (lambda (,k)
              (escm-generator-escape 
                (cons 
                  (cons 'escm-generator ,yielded)
                  (lambda () 
                    (call/cc (lambda (escm-generator-escape) (,k escm-generator-escape))))))))))))

(define-syntax define-generator 
  (lambda (bindings . body)
    (define generator-object (gensym))
    `(define ,bindings
      (define ,generator-object
        (cons
          (cons 'escm-generator #f)
          (lambda ()
            (call/cc (lambda (escm-generator-escape) ,@body)))))
      (lambda ()
        (if (escm-generator? ,generator-object)
            (begin 
              (set! ,generator-object ((cdr ,generator-object)))
              (if (escm-generator? ,generator-object)
                  (cdar ,generator-object)
                  ,generator-object))
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
  `(,(caar inlined-method-property) 
    (lambda ,(cdar inlined-method-property) 
      ,@(cdr inlined-method-property))))

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
    (define obj (gensym))
    `(begin
      (define (,(symbol-append name '?) ,obj) (oo-is? ,obj ,name)) ; predicate generation!
      (define ,name (class ,@class-components)))))


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
    (define obj (gensym))
    `(begin
      (define (,(symbol-append name '?) ,obj) (oo-is? ,obj ,name)) ; predicate generation!
      (define ,name (interface ,@interface-components)))))


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
