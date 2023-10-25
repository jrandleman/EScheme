<!-- specials.md -->

# Primitive Special Forms
### Special forms implemented in Java accessible by EScheme source code!


------------------------
## Inline Bytecode:
```scheme
(bytecode <instruction> ...)
```


------------------------
## Define Special Forms:
```scheme
(define-syntax <macro-name> <callable>)

(quote <obj>)
(quasiquote <obj>) ; includes (unquote <obj>) and (unquote-splicing <obj>)
```


------------------------
## Bind Variables:
```scheme
(define <variable-name> <value>)
(define <variable1> <variable2> ... <variableN> <N-length-list-expression>)
(define (<procedure-name> <parameter> ...) <body> ...)

(defined? <variable-name>)

(set! <variable-name> <value>)
(set! <variable1> <variable2> ... <variableN> <N-length-list-expression>)
```


------------------------
## Procedures:
```scheme
(fn ((<param> ...) <body> ...) 
    ((<param> ... . <variadic-param>) <body> ...)
    (<variadic-param> <body> ...) 
    ...)

(defn <procedure-name> (<parameters> <body> ...) ...)

(lambda (<param> ...) <body>)
(lambda (<param> ... . <variadic-param>) <body>)
(lambda <variadic-param> <body>)

(curry (<param> ...) <body> ...)
```


------------------------
## Combine Expressions:
```scheme
(begin <expression> ...)
```


------------------------
## Conditional Branching:
```scheme
(if <condition> <consequence>)
(if <condition> <consequence> <alternative>)

(unless <condition> <body> ...)

(when <condition> <body> ...)

(cond (<condition> <body> ...) 
      (<condition>  => <callable>)
      (else <body> ...)
      ...)

(case <value> 
      ((<key> ...) <expression> ...) 
      ((<key> ...)  => <callable>)
      (else <expression> ...)
      ...)

(and <obj> ...)
(or <obj> ...)
```


------------------------
## Catch Exceptions:
```scheme
(guard 
  (<raised-variable> 
    (<condition> <guard-body> ...) ...
    (else <guard-body>)) 
  <body> ...)
```


------------------------
## Locally Bound Variables:
```scheme
(let ((<variable-name> <value>) ...) <body> ...)
(let <procedure-name> ((<parameter-name> <initial-value>) ...) <body> ...)

(let* ((<variable-name> <value>) ...) <body> ...)

(letrec ((<variable-name> <value>) ...) <body> ...)

(letrec* ((<variable-name> <value>) ...) <body> ...)

(let-values (((<var> ...) <'values'-expression>) ...) <body> ...)
```


------------------------
## Locally Bound and Threaded Variables:
```scheme
(-<> <expression-using-'<>'> ...)
```


------------------------
## Loops:
```scheme
(while (<condition>) <body> ...)
(while (<condition> <return-expression> ...) <body> ...)

(do ((<var> <initial-val> <update-expression>) ...)
      (<break-condition> <return-expression> ...)
      <body> ...)
```


------------------------
## Delay Execution:
```scheme
(delay <expression>)
```


------------------------
## Create a Stream Pair:
```scheme
(scons <obj> <obj>)
```


------------------------
## Define Generators:
```scheme
(define-generator (<generator-constructor-name> <param> ...) <body> ...)

(yield)
(yield <value>)
```


------------------------
## Manipulate Dynamic Thread Environments:
```scheme
(thread-define <variable-name> <value>)
(thread-define <thread> <variable-name> <value>)

(thread-set! <variable-name> <value>)
(thread-set! <thread> <variable-name> <value>)

(thread-get <variable-name>)
(thread-get <thread> <variable-name>)

(thread-defined? <variable-name>)
(thread-defined? <thread> <variable-name>)
```


------------------------
## Synchronize Concurrent Access:
```scheme
(dosync <expression> ...)
(dosync-module <expression> ...)
(dosync-with <mutex> <expression> ...)
```


------------------------
## Manipulate Parameterized Variables:
```scheme
(define-parameter <variable-name> <obj>)

(set-parameter! <variable-name> <obj>)

(get-parameter <variable-name>)

(parameter? <variable-name>)
```


------------------------
## Create Classes:
```scheme
(class (:extends <super>) (:implements <interface> ...) <prop> ...)

(define-class <class-name> (:extends <super>) (:implements <interface> ...) <prop> ...)

;  <prop> ::= (<name> <value>)
;           | ((<method-name> <param> ...) <body> ...)
;           | (:static <name> <value>)
;           | (:static (<method-name> <param> ...) <body> ...)
```


------------------------
## Create Interfaces:
```scheme
(interface (:extends <interface> ...) <prop> ...)

(define-interface <interface-name> (:extends <interface> ...) <prop> ...)

;  <prop> ::= <name>
;           | (:static <name> <value>)
;           | (:static (<method-name> <param> ...) <body> ...)
```


------------------------
## Invoke Super Object Constructors:
```scheme
(super! <arg> ...)

(apply-super! <arg-list>)
```


------------------------
## Manipulate Modules:
```scheme
(import <module-path-symbol>)
(import <module-path-symbol> :as <module-alias-symbol>)
(import <filepath-str> <module-path-symbol>)
(import <filepath-str> <module-path-symbol> :as <module-alias-symbol>)

(reload <module-alias-symbol>)

(from <module-path-symbol> :import <obj1-symbol> <obj2-symbol> ...)
(from <module-path-symbol> :import <obj1-symbol> <obj2-symbol> ... :as <alias1-symbol> <alias2-symbol> ...)
(from <filepath-str> <module-path-symbol> :import <obj1-symbol> <obj2-symbol> ...)
(from <filepath-str> <module-path-symbol> :import <obj1-symbol> <obj2-symbol> ... :as <alias1-symbol> <alias2-symbol> ...)
```
