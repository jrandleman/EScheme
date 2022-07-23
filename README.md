<!-- README.md -->

## Installing and Running EScheme:
* Installation: Compile and run `Installer.java` within `EScheme/installer`
* Execution: Run `java EScheme/src/Main`

## On Reserved Symbols:
* Anything with the `escm-` prefix is considered reserved for use by the runtime.

## On Threading & Continuations:
* Threads each have their own stack, and hence their own set of continuations.
* Continuations are delimited by the execution of the thread that created them.
* Hence a child thread's continuation will never "continue" back into the parent 
  thread, rather, it will terminate where the child thread terminated.

## On Threading & Dynamic Environments:
* Each thread has a so-called "dynamic environment", wherein a set of variable 
  bindings is kept globally within the thread while being hidden from other threads.
* After querying for a variable that doesn't exist in a thread's dynamic environment, 
  the "meta thread"'s dynamic environment is checked. If an entry is found in the 
  meta thread's dynamic environment, a local copy is cached into the current thread's 
  dynamic environment and the querying operation continues.
* This allows for us to have "environment-global-thread-local" variables! State can
  be shared & operated upon by many procedures without having to lock, since each
  thread only ever operates on a local copy of the state!
  * This is used by `dynamic-wind` in order to maintain thread-local winds!

## EScheme-Specific Concepts (Scheme Extension/Deviations)

1. Support for `bytecode`, `compile`, `eval-bytecode`
   - `bytecode`: special form to have the compiler reflect the given bytecode
   - `compile`: convert a quoted escm expression into a quoted bytecode list
   - `eval-bytecode`: evaluate the given quoted bytecode list in the global environment
     * Hence `eval` is equivalent to `(compose eval-bytecode compile)`!
2. `#void` `#nil` reader literals for their respective values
3. Keyword primitive types (like symbols, but prefixed with `:`, & they always evaluate to themselves)
4. Immutable core strings & pairs (mutable `<pair>` alternative may be implemented by users via escm's object system)
5. `\` reader lambda literal support with 1-indexed params of index `i` via `%i` syntax, and a variadic param via `%%`
6. `(. <obj>)` is equivalent to `<obj>` for the reader
7. Compile-time procedural macro system (no run-time bindings, all global in scope, deletable for localization)
8. Object System (classes, interfaces, and objects!)
9. Multi-arity and optional-parameter support via `fn`
10. No `eqv?`, `memv`, or `assv`: only `eq?` and `equal?` are present.
