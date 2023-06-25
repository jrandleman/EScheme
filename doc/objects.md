<!-- objects.md -->

# Object Orientation in EScheme
### Describes escm's OO system!


## Object System Overview

* Single inheritance for classes, multiple inheritance for interfaces
* Static support:
  - `:static` fields & methods for class/interface-local properties
    * implied when `define`ing a new class/interface property!
* `self` semantics:
  - instance methods have `self` dynamically bound to the calling object
  - static methods have `self` bound to the class datum they belong to
* `super` semantics:
  - instance methods have `super` statically bound to the super object
  - static methods have `super` bound to the super class
* Referring to static props in instance methods:
  - `<classname>.<static-prop>`
  - `self.class.<static-prop>`
* Special object properties:
  - `new` pseudo-method constructor syntax
    * Doesn't correlate to an actual method named `new`
  - `class` field to access the class of the current object
  - `->procedure` method to overload application for objects
    * Applicable objects with this method are called "functor"s
* Special class and interface property:
  - `name`: the symbolic name of the class/interface
    * Only present if the class/interface is NOT anonymous!
* All instance & static methods have the following variables automatically defined:
  - `self`  ; the polymorphic calling object
  - `super` ; the super object if exists, else #f
* Bytecode-level instruction support for `(define obj.prop)` `(set! obj.prop)` syntax



------------------------
## Named Class Syntax:
### Also generates a `(<class-name>? <obj>)` predicate procedure!

```scheme
(define-class <class-name> 
  (:extends <class>) (:implements <interface> ...) ; both ":extends" and ":implements" are optional
  (:static <name> <value>)
  (:static (<method-name> <param> ...) <body> ...)
  (<name> <value>)
  ((<method-name> <param> ...) <body> ...))
```


------------------------
## Anonymous Class Syntax:
```scheme
(class (:extends <class>) (:implements <interface> ...) ; both ":extends" and ":implements" are optional
  (:static <name> <value>)
  (:static (<method-name> <param> ...) <body> ...)
  (<name> <value>)
  ((<method-name> <param> ...) <body> ...))
```


------------------------
## Named Interface Syntax:
### Also generates an `(<interface-name>? <obj>)` predicate procedure!

```scheme
(define-interface <interface-name> 
  (:extends <interface> ...) ; ":extends" is optional
  (:static <name> <value>)
  (:static (<method-name> <param> ...) <body> ...)
  <name>) ; required property name(s) for a class to have
```


------------------------
## Anonymous Interface Syntax:
```scheme
(interface (:extends <interface> ...) ; ":extends" is optional
  (:static <name> <value>)
  (:static (<method-name> <param> ...) <body> ...)
  <name>) ; required property name(s) for a class to have
```


------------------------
## Initializing Super Object Constructors

The `(super! <param> ...)` macro may be used within object constructors to
initialize an object's super class with a set of parameters!
  * Super objects with a "nullary" ctor are ctor'd automatically
  * `super!` ___must___ be used ___immediately___ in ctors to avoid undefined behavior!

