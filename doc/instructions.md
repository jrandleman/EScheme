# An Introduction to EScheme's Bytecode

## Brief Insight
EScheme's bytecode structure can be thought of as a chain of instruction sets 
serving as function bodies.

The instructions (as outlined below) are designed __exclusively__ to serve as a 
means by which to perform control flow and bind items to the current environment. 

___All logical operations are abstracted away via user-defined & primitive functions!___ 

Such is the crux of our incredibly reduced instruction set, and the fact that 
we can get away with having all of our instructions be either unary or nullary.
For example, unlike most flavors of assembly, there is no `add` instruction. 
Rather, to us, `+` is called like any other function.


## Data Manipulation
All data is handled via 2 mediums in our bytecode: the "current value register" 
(CVR), and the function stack.

The CVR psuedo-register is never explicitely referenced in EScheme bytecode, 
rather it is implicitly used by certain instructions. For example, compiling 
`(define life 42)` yields the bytecode `(load 42) (define life)`. The `load` 
instruction implicitly sets CVR to its argument, and `define` implicitly binds 
its argument to CVR in the environment.

The function stack is referred to via the `push`, `pop`, and `call` instructions.
`call` affects the stack because, due to our lack of multiple registers, we push 
our arguments (and the function we're calling) onto the stack rather than putting 
them in registers. More details on the `call` instruction may be found below.


## Why Write Bytecode?
The vast majority of the bytecode that the interpreter works with is the result of
compiling EScheme expressions. However, it may be desirable to write out bytecode 
by hand under very specific circumstances. 

For example, while EScheme certainly supports iteration via recursion, there is no 
"true" iteration by default a la `for`/`while` loop in Java or C++. However, by 
leveraging inlined bytecode, we can write a `while` macro that __does__ have true
iteration (such is outlined as an example below)!


## Writing Bytecode
In order to write out bytecode, simply follow the instruction syntax outlined below.
Note that any arguments specified as `<symbol>` or `<integer>` MUST be symbolic or 
integer literals respectively. For example, `jump` and `call` both accept integers, 
but whereas `jump` requires that it's argument is an integer literal, `call` also 
accepts variables that evaluate to integers.

### NIL
A note on writing NIL literals: use `()`. For example, `(define n ())`
compiles to `(load ()) (define n)`. You may alternatively use the `#nil` reader
literal.

### VOID
A note on writing VOID literals: use `#void`. For example, `(define n #void)`
compiles to `(load #void) (define n)`.

### Closures
A note on writing closures: you may nest the `load-closure` syntax as needed in order
to denote closures. The `load-closure` syntax is as follows:

`(load-closure <optional-docstring> ((<argument> ...) <instruction> ...) ...)`

This syntax will load the equivalent of: `(fn <optional-docstring> ((<argument> ...) <instruction> ...) ...)`
into CVR. Further note that closure expressions by default return the value left in 
CVR upon terminating execution. `<docstring>` may be optionally provided to yield
further information on the closure's intended purpose in the `help` menu.

For example, you may write:
```scheme
(define (my-function y) 
  (map (lambda (x) (* x y)) (list 1 2 3)))
```

As:
```
(load-closure 
  ((y)
    (push map)
    (load-closure 
      ((x)
        (push *)
        (push x)
        (push y)
        (call 3)))
    (push)
    (push list)
    (push 1)
    (push 2)
    (push 3)
    (call 4)
    (push)
    (call 3)))
(define my-function)
```

### Object Access Chains
The escm VM has built-in support for interpretting object access chains, hence
`(define obj.prop1.prop2)` is perfectly valid bytecode syntax.

As such, all instructions that set or evaluate a symbolic datum support this 
syntax. These include: `define`, `set!`, `load`, `call`, `push`, & `return`.




------------------------------------------------------------------------------
# The EScheme Bytecode Instruction Set

```
(define <symbol>) ; bind <symbol> to CVR [sets CVR to <void>]

(set! <symbol>) ; set! <symbol> to CVR [sets CVR to <void>]

(defined? <symbol>) ; determine if <symbol> is defined as a variable [sets CVR to the boolean result]

(ifn <integer>) ; if CVR is NOT truthy, jump <integer> instructions [sets CVR to <void>]

(jump <integer>) ; jump <integer> instructions

(quote <datum>) ; quote <datum> and load it into CVR. may recurse infinitely for cyclic vectors/hashmaps.

(load <datum>) ; evaluate <datum> and load it into CVR

(call <datum>) ; <datum> must evaluate to an integer. get the fcn & arguments being applied from the stack. 
               ; positive <integer> denotes pushes from left to right & negative denotes pushes from right to left (when compiling the application expressions). 
               ; pops (abs <datum>) items off of the stack after the call, and places the returned value of the fcn application in CVR.

(push)         ; push CVR to the stack
(push <datum>) ; push <datum> to the stack

(pop) ; pop a value off of the stack into CVR

(return)         ; returns the value in CVR (effectively jumps to the end of the function)
(return <datum>) ; returns the value in <datum> (effectively jumps to the end of the function)
```

------------------------------------------------------------------------------
## Helper `bytecode` Syntax
### Psuedo-instruction(s) converted into "real" instruction(s) by the assembler

```
(load-closure <optional-docstring> ((<argument> ...) <instruction> ...) ...) ; syntax to load a closure
```




------------------------------------------------------------------------------
# Examples

```scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Demoing the compilation of closures

;; Source Code
(define (my-function y) 
  (map (lambda (x) (* x y)) (list 1 2 3)))

;; Compiled Bytecode
(load-closure 
  ((y)
    (push map)
    (load-closure 
      ((x)
        (push *)
        (push x)
        (push y)
        (call 3)))
    (push)
    (push list)
    (push 1)
    (push 2)
    (push 3)
    (call 4)
    (push)
    (call 3)))
(define my-function)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defining a "(while (<condition> <return-expr> ...) <body> ...)" macro using
;; inlined bytecode & compilation

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
;; Implementing "apply" in bytecode (impossible in native Scheme!)

(load-closure 
  ((f args-list)
    (load 1)
    (define count)
    (push f)
    (push null?)
    (push args-list)
    (call 2)
    (ifn 2)
    (jump 15)
    (push car)
    (push args-list)
    (call 2)
    (push) ; save an extracted value
    (push cdr)
    (push args-list)
    (call 2)
    (set! args-list)
    (push +)
    (push 1)
    (push count)
    (call 3)
    (set! count)
    (jump -18)
    (call count)))
(define apply)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing factorial (because of course we do so)

;; Source Code
(define (factorial n)
  (if (< n 2)
      1
      (* n (factorial (- n 1)))))

;; Compiled Bytecode
(load-closure 
  ((n)
    (push <)
    (push n)
    (push 2)
    (call 3)
    (ifn 2)
    (return 1)
    (push *)
    (push n)
    (push factorial)
    (push -)
    (push n)
    (push 1)
    (call 3)
    (push)
    (call 2)
    (push)
    (call 3)))
(define factorial)
```
