<!-- types.md -->

# Types in EScheme

### Describes EScheme's optional type system!

## Overview

EScheme denotes types with keywords, and "union types" via `|` syntax.

- EX: `:str|num` represents either a string or a number.

EScheme types are typically either a "primitive" or "collection" type.
If a type is neither a primitive nor a collection, it is presumed to
represent some class, interface, or type-alias: if the type doesn't
resolve to a valid class, interface, or type-alias during a runtime
type-check, an error is thrown.

- Note that EScheme supports referencing classes/interfaces/aliases
  in modules! Hence `:Module.ClassName` is a valid type.

EScheme types are parsed and converted to Java functional interface
predicates internally during compilation, with the predicates being
applied at runtime. Types are supported for function parameters and
return values.

### EScheme Primitive Types

Primitive types represent an intrinsic atomic EScheme type. Their
type-checks are typically as fast as a single `instanceof` check,
with a few exceptions like `:int` requiring slightly more work.

EScheme's primitive types include:

```scheme
:any

:num ; aliased by ":complex"
:int ; matches floats without fractionals too
:flo
:real
:exact
:inexact

:str
:char
:key ; keyword
:bool
:sym ; symbol
:void

:thread
:mutex

:nil
:atom

:fn ; all callables
:procedure
:syntax

:metaobj ; includes objects, classes, and interfaces
:object
:class
:interface

:dottable ; includes objects, classes, interfaces, and modules
:module

:port
:inport
:outport

:type-alias
```

### EScheme Collection Types

Collection types represent an intrinsic EScheme collection type. By
default, collections are just type-checked to match whatever type
of collection the keyword stands for. However, collections may be
parameterized by adding the `<type>` suffix in order to type-check
their contents as well.

For example, `:list<str|sym>` is a list where each element is either
a string or symbol.

- For either a list that only has strings OR a list that only has
  symbols, use `:list<str>|list<sym>`.
- Furthermore, `:pair` and `:map` may also be parameterized with the
  `<type,type>` suffix in order to type-check their keys and values.

EScheme's collection types include:

```scheme
:vec ; vector
:map ; hashmap

:pair
:list

:ac ; associative-collection (hashmaps, vectors, lists, strings)
:oc ; ordered-collection (vectors, lists, strings)
```

---

## Type Syntax

Notes on optional and variadic parameters:

- Optional parameters only type-check user args, _not_ their default values
  - Hence `(:int a "hello")` is a valid optional parameter clause
- Variadic values cannot be typed (they're implicitly `:list<any>`)

### `fn` and `defn`

- `defn` uses the same type syntax as `fn`

```scheme
(fn
  ; Typed <:int> return and <:list>/<:char> parameters
  (:int (:list a :char b . rest-args)
    (length (cons b (cons a rest-args))))

  ; Typeless return, required <:flo> parameter and optional <:int> parameter
  ((:flo a (:int b 42))
    (+ a b)))


(defn function-name
  ; Typed <:int> return and <:list>/<:char> parameters
  (:int (:list a :char b . rest-args)
    (length (cons b (cons a rest-args))))

  ; Typeless return, required <:flo> and optional <:int> parameters
  ((:flo a (:int b 42))
    (+ a b)))
```

### `lambda`

```scheme
; Typed <:int> return and <:list>/<:char> parameters
(lambda :int (:list a :char b . rest-args)
  (length (cons b (cons a rest-args))))

; Typeless return, required <:flo> and optional <:int> parameters
(lambda (:flo a (:int b 42))
  (+ a b))
```

### `define`

```scheme
; Typed <:int> return and <:list>/<:char> parameters
(define :int (function-name :list a :char b . rest-args)
  (length (cons b (cons a rest-args))))

; Typeless return, required <:flo> and optional <:int> parameters
(define (function-name :flo a (:int b 42))
  (+ a b))
```

### `define-generator`

- Only supports typed parameters, not typed returns, to account for
  `*generator-complete*` being returned from finite generators.

```scheme
; Required <:flo> and optional <:int> parameters
(define-generator (generator-factory-name :flo a (:int b 42))
  (let loop ((i b))
    (yield (+ i a))
    (loop (+ i 1))))
```

### `curry`

- Only type-checks the return value once all parameters have been applied.

```scheme
; Typed <:int> return and <:list>/<:char> parameters
(curry :int (:list a :char b)
  (length (cons b a)))
```

### `class`/`define-class` and `interface`/`define-interface`

- `class` supports types on instance and static methods
- `interface` supports types on instance method signatures and static methods
- `define-class` uses the same type syntax as `class`
  - as `define-interface` does with `interface`

```scheme
(define-class ClassName
  ; Instance: typed <:int> return and <:list>/<:char> parameters
  (:int (method-name-1 :list a :char b . rest-args)
    (length (cons b (cons a rest-args))))

  ; Instance: typeless return, required <:flo> and optional <:int> parameters
  ((method-name-2 :flo a (:int b 42))
    (+ a b))

  ; Static: typed <:int> return and <:list>/<:char> parameters
  (:static :int (method-name-1 :list a :char b . rest-args)
    (length (cons b (cons a rest-args))))

  ; Static: typeless return, required <:flo> and optional <:int> parameters
  (:static (method-name-2 :flo a (:int b 42))
    (+ a b)))


(define-interface InterfaceName
  ; Instance signature: typed <:int> return and <:list>/<:char> parameters
  (:int (method-name-1 :list a :char b . rest-args))

  ; Instance signature: typeless return, required <:flo> and optional <:int> parameters
  ((method-name-2 :flo a (:int b 42)))

  ; Static: typed <:int> return and <:list>/<:char> parameters
  (:static :int (method-name-1 :list a :char b . rest-args)
    (length (cons b (cons a rest-args))))

  ; Static: typeless return, required <:flo> and optional <:int> parameters
  (:static (method-name-2 :flo a (:int b 42))
    (+ a b)))


; Mandating that <function-name> returns either <ClassName> or <InterfaceName>
(define :ClassName|InterfaceName (function-name)
  (ClassName))
```

---

## Type Aliases

Type aliases reference a preexisting keyword type, typically to
mask type complexity. For example, when implementing a `UserProfile`
class, it might be nicer to define a `:phone-number` type instead of
always using `:str|list<int>`.

Type aliases can be created by using `define-type` (aliased by `deftype`).
Type alias primitive helper functions include:

- `(type-alias? <obj>)` returns whether `<obj>` is a type alias
- `(type-alias-source <type-alias>)` returns the original keyword type
  that `<type-alias>` references

### Example

```scheme
; Create a type-alias and dispatch on it
(define-type phone-number :str|list<int>)

(defn function-name
  ((:phone-number x) #t)
  ((:any x) #f))

(function-name "555-555-5555") ; #t
(function-name '(555 555 5555)) ; #t
(function-name 5555555555) ; #f
```

---

## Type Primitives

- `(type? <type-keyword>)` returns if `<type-keyword>` is a valid type
- `(type? <obj> <type-keyword>)` returns if `<obj>` is a `<type-keyword>`
- `(type=? <type-keyword> ...)` returns if `<type-keyword>`s are equivalent
