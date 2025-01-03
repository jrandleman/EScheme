<!-- objects.md -->

# Objects in EScheme

### Describes EScheme's optional object system!

## Object System Overview

EScheme has a totally optional object system, including support for:

- Single inheritance for classes, multiple inheritance for interfaces
- Static support:
  - `:static` fields & methods for class/interface-local properties
    - implied when `define`ing a new class/interface property!
- `self` semantics:
  - instance methods have `self` dynamically bound to the calling object
  - static methods have `self` bound to the class datum they belong to
- `super` semantics:
  - instance methods have `super` statically bound to the super object
  - static methods have `super` bound to the super class
- Referring to static props in instance methods:
  - `<classname>.<static-prop>`
  - `self.class.<static-prop>`
- Special object properties:
  - `new` pseudo-method constructor syntax
    - Doesn't correlate to an actual method named `new`
  - `class` field to access the class of the current object
  - `->procedure` method to overload application for objects
    - Applicable objects with this method are called "functor"s
- Special class and interface property:
  - `name`: the symbolic name of the class/interface
    - Only present if the class/interface is NOT anonymous!
- All methods have the following variables automatically defined:
  - `self` ; the polymorphic calling object
  - `super` ; the super object if exists, else #f
- Bytecode-level support for `(define obj.prop)` `(set! obj.prop)` syntax

---

## Named Class Syntax:

### Also generates a `(<class-name>? <obj>)` predicate procedure!

### May use `defclass` to alias `define-class`!

```scheme
(define-class <class-name>
  (:extends <class>) (:implements <interface> ...) ; both are optional
  <optional-docstring>
  (:static <name> <value>)
  (:static (<method-name> <param> ...) <body> ...)
  (<name> <value>)
  ((<method-name> <param> ...) <body> ...))
```

---

## Anonymous Class Syntax:

```scheme
(class (:extends <class>) (:implements <interface> ...) ; both are optional
  <optional-docstring>
  (:static <name> <value>)
  (:static (<method-name> <param> ...) <body> ...)
  (<name> <value>)
  ((<method-name> <param> ...) <body> ...))
```

---

## Named Interface Syntax:

### Also generates an `(<interface-name>? <obj>)` predicate procedure!

### May use `definterface` to alias `define-interface`!

```scheme
(define-interface <interface-name>
  (:extends <interface> ...) ; ":extends" is optional
  <optional-docstring>
  (:static <name> <value>)
  (:static (<method-name> <param> ...) <body> ...)
  <name> ; required property name(s) for a class to have
  ((<method-name> <param> ...))) ; required method signature(s) for a class to have
```

---

## Anonymous Interface Syntax:

```scheme
(interface (:extends <interface> ...) ; ":extends" is optional
  <optional-docstring>
  (:static <name> <value>)
  (:static (<method-name> <param> ...) <body> ...)
  <name> ; required property name(s) for a class to have
  ((<method-name> <param> ...))) ; required method signature(s) for a class to have
```

---

## Initializing Super Object Constructors

The `(super! <param> ...)` macro may be used within object constructors to
initialize an object's super class with a set of parameters.

- Super objects with a "nullary" constructor are automatically constructed.
- `super!` must be used immediately in constructors to avoid undefined behavior!
- Use `(apply-super! <param-list>)` to initialize the super object with a list.
