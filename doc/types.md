<!-- types.md -->

# Types in EScheme

### Describes EScheme's optional type system!

## Overview

EScheme denotes types with keywords, and "compound types" via `|` syntax.

- EX: `:string|number` represents either a string or a number.

EScheme types are typically either a "primitive" or "container" type.
If a type is neither a primitive nor a container, it is presumed to
represent some class, interface, or type-alias: if the type doesn't
resolve to a valid class, interface, or type-alias during a runtime
type-check, an error is thrown.

- Note that EScheme supports referencing classes/interfaces/aliases
  in modules! Hence `:Module.ClassName` is a valid type.

EScheme types are parsed and converted to Java functional interface
predicates internally during compilation, with the predicates being
applied at runtime.

### EScheme Primitive Types

Primitive types represent an intrinsic atomic EScheme type. Their
type-checks are typically as fast as a single `instanceof` check,
with a few exceptions like `:int` requiring slightly more work.

EScheme's primitive types include:

```
:any

:number ; aliased by ":complex"
:int
:flo
:real
:exact
:inexact

:string
:char
:key
:bool
:symbol
:void

:thread
:mutex

:nil
:atom

:fn ; all callables
:procedure
:syntax

:metaobj ; includes modules
:object
:class
:interface

:port
:inport
:outport

:module

:type-alias
```

### EScheme Container Types

Container types represent an intrinsic EScheme collection type. By
default, collections are just checked to match whatever type of
collection the keyword stands for (without regard for the types of
its contents). However, containers may be parameterized by adding
the `<type>` suffix in order to type-check its contents as well.

For example, `:list<string|symbol>` is a list where each element is either a
string or symbol.

- For either a list that only has strings OR a list that only has
  symbols, use `:list<string>|list<symbol>`.
- Furthermore, `:pair` and `:map` may also be parameterized with the
  `<type,type>` suffix in order to type-check their keys and values.

EScheme's collection types include:

```
:vector
:map

:pair
:list

:associative-collection ; aliased by ":ac"
:ordered-collection ; aliased by ":oc"
```
