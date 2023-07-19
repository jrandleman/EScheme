<!-- modules.md -->

# Modules in EScheme
### Describes EScheme's module system

## Motivation and Overview
The value created by an `import` expression.

Modules present an alternative to Scheme's usual inter-file semantics.

The `load` function has always served as a means by which to execute the code
of another Scheme file in the calling file's global environment. This simplicity
is a double edged sword though: while making `load` easy to implement, it leaves
much to be desired with respect to enabling encapsulation of code across
coordinated Scheme files.

As such, in addition to `load`, EScheme offers a minimalistic module system
that strives to enable file-specific encapsulation of EScheme code. Files
processed via the `import` macro are defined as `module` objects within the
enclosing enivironment. See the `import` details below for more information on
how EScheme evaluates modules.

Each module has its own isolated global environment, and has automatic access
to EScheme's standard library. Note that this means that operations that depend
on global variables (e.g. `load-once`) are hence only able to operate on
a module-relative basis.

* Note that `dosync` notably works across modules, since its internal lock was 
  created via `define-parameter`. Use `dosync-module` for module-relative locking 
  behavior.

Both variables and macros can be access from a module by using EScheme's
dot-notation: for example, to access variable `PersonClass` from module `Mod`,
we write `Mod.PersonClass`.

Further note that imported modules are cached! Importing the same module
multiple times does not cause multiple evaluations of that module. Use the
`reload` macro if you'd like to forcefully re-evaluate the module in question.

Additionally, the `from` macro may be used to load specific variables within
a given module, without defining that module itself as a local variable.

Modules may be introspected upon by 2 primitives:

1. `module-source`: yield the absolute file path to the module (this is what
   distinguishes one module from another under the hood).
2. `module-bindings`: yield a list of the symbols defined in a module (beware:
   every module redefines the entire EScheme standard library!).

Note that the 'meta-thread's environment (see `thread-define`) is module 
independant!

Use the `*import*` variable to determine if the current file was `import`ed.
Can combine with `unless` to mimic Python's `if __name__=='__main__':` pattern:

```scheme
  (unless *import*
    <execute-main-escheme-code-here> ...)
```

Lastly, note that the concept of 'parameter' variables exist in order to
have global state shared across modules. See the `define-parameter` and
`parameter?` 'help' entries for more details.

------------------------
## Accessing variable `<var>` from module `<module>`
```scheme
<module>.<var>
```


------------------------
## Regular Imports
```scheme
(import <module-path-symbol>)
(import <module-path-symbol> :as <module-alias-symbol>)
(import <filepath-string> <module-path-symbol>)
(import <filepath-string> <module-path-symbol> :as <module-alias-symbol>)
```

Import `<module-path-symbol>` as a module variable in the current environment,
where `<module-path-symbol>` represents an Escheme (or `serialize`d!) file.
Seeks the module from `<filepath-string>`, if provided.

Module variables are named `<module-path-symbol>` by default, though there are
2 caveats:

 1. `<module-alias-symbol>` is provided: this overrides the default name and
    will be what the module is loaded into the environment as.

 2. `<module-path-symbol>` is a dotted list of symbols: this is how we denote
    access to a module that is in a different folder than the current
    directory. For example, suppose we have the following directory layout:
      ```
      Root
      |____ Folder1
      |     |_______ Module.scm
      |
      |____ Folder2
            |_______ Main.scm
      ```

    For `Main.scm` to import `Module.scm`, it would contain:

      ```scheme
      (import Root.Folder1.Module) ; within 'Main.scm'
      ```

    In this example, the module variable would be named `Module`. Note that
    the file extension of the target module file is left out from the `import`
    expression.

With regards to locating the file pointed to by `<module-path-symbol>`, EScheme
will first attempt to find it along the file path tree from the location that
invoked the `import` expression. Note that this makes executing `(import #path Module)`
redundant, in contrast to the `load` function.

Imported modules are also cached across modules, so every instance of `(import Module)`
will reference the same `Module` module object. Note that you can force a module
to be reloaded via the `reload` special form.

See the `from` details below for an alternative to `import` that extracts specific
fields from the `<module-path-symbol>` module, without adding the module itself to
your current environment's namespace.


------------------------
## Reloading Modules
```scheme
(reload <module-alias-symbol>)
```

Forcefully re-evaluate `<module-alias-symbol>` (note that `import` caches
module by default).


------------------------
## Loading Module Variables
```scheme
(from <module-path-symbol> :import <obj1-symbol> <obj2-symbol> ...)
(from <module-path-symbol> :import <obj1-symbol> <obj2-symbol> ... :as <alias1-symbol> <alias2-symbol> ...)
(from <filepath-string> <module-path-symbol> :import <obj1-symbol> <obj2-symbol> ...)
(from <filepath-string> <module-path-symbol> :import <obj1-symbol> <obj2-symbol> ... :as <alias1-symbol> <alias2-symbol> ...)
```

Import `<obj1-symbol> <obj2-symbol> ...` from `<module-path-symbol>`, without
exposing the module itself as a variable within the current environment.

Equivalent to:
 
 ```scheme
 (begin
   (import <module-path-symbol> :as <hidden-name>)
   (define <obj1-symbol> <hidden-name>.<obj1-symbol>)
   (define <obj2-symbol> <hidden-name>.<obj2-symbol>)
   ...)
 ```

Or, if `<alias1-symbol> ...` is provided:

 ```scheme
 (begin
   (import <module-path-symbol> :as <hidden-name>)
   (define <alias1-symbol> <hidden-name>.<obj1-symbol>)
   (define <alias2-symbol> <hidden-name>.<obj2-symbol>)
   ...)
 ```

If given `<filepath-string>`, `from` simply adds it to the above `import`
statement.


------------------------
## Module Introspection Primitives

### Module Predicate
```scheme
(module? <obj>)
```

### Module Source Location
```scheme
(module-source <module>)
```

The absolute file path string of the module file's location, what
distinguishes one module from another under the hood.

### Module Variable Bindings
```scheme
(module-bindings <module>)
```

A list of variable symbols defined in `<module>`. Beware that all modules load
all of EScheme's standard library by default!
