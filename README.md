<!-- README.md -->

# Eerina's Scheme

------------------------------------------------------------------------------
## Installing and Running EScheme:
### Dependencies:
* EScheme depends on [Java](https://adoptium.net) and [`git`](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git). Make sure they're both installed first!
  - Note that EScheme supports both Java 11 and 17 (LTS releases)!

### Installation:
* Via the command-line, go to the directory you'd like to install `EScheme` in.
* Run: `git clone https://github.com/jrandleman/EScheme`
* Go to the `EScheme/installer` directory.
* Run: `javac Installer.java`
* Run: `java Installer`
  - Use the `-v` or `--verbose` command-line flag to get print extra progress messages.
* ___Optional:___ if you're using `bash` or `zsh` as your shell:
  - `bash`:
    1. Copy the `alias='...'` output by the installer.
    2. Add it to your `~/.bashrc` (and/or `~/.bash_aliases`) file.
    3. Run: `. ~/.bashrc` (and/or `~/.bash_aliases`)
  - `zsh`:
    1. Copy the `alias='...'` output by the installer.
    2. Add it to your `~/.zshrc` file.
    3. Run: `. ~/.zshrc`
  - You can now run `escm` anywhere in the command-line to launch EScheme!

### Execution:
* If you didn't do the optional step during installation:
  - Run `java Main` within the `EScheme/bin` directory.
* Else:
  - Run `escm` anywhere in your terminal!



------------------------------------------------------------------------------
## On Reserved Symbols:
* Anything with the `escm-` prefix is considered reserved for use by the runtime.



------------------------------------------------------------------------------
## On Threading & Continuations:
* Threads each have their own stack, and hence their own set of continuations.
* Continuations are delimited by the execution of the thread that created them.
* Hence a child thread's continuation will never "continue" back into the parent 
  thread, rather, it will terminate where the child thread terminated.



------------------------------------------------------------------------------
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



------------------------------------------------------------------------------
## EScheme-Specific Concepts (Scheme Extension/Deviations)

1. Support for `bytecode`, `compile`, `eval-bytecode`
   - `bytecode`: special form to have the compiler reflect the given bytecode
   - `compile`: convert a quoted escm expression into a quoted bytecode list
   - `eval-bytecode`: evaluate the given quoted bytecode list in the global environment
     * Hence `eval` is equivalent to `(compose eval-bytecode compile)`!
2. `#void` `#nil` reader literals for their respective values
3. Keyword primitive types
   - Like symbols, but prefixed with `:`, & they always evaluate to themselves
4. Immutable core strings & pairs:
   - Mutable `<pair>` alternative may be implemented by users via escm's object system
5. `\` reader lambda literal support:
   - 1-indexed params of index `i` via `%i` syntax, and a variadic param via `%%`
   - `\%1` => `(lambda (%1) %1)`, `\(+ 3.14 %2)` => `(lambda (%1 %2) (+ 3.14 %2))`
6. `(. <obj>)` is equivalent to `<obj>` for the reader
7. Compile-time procedural macro system:
   - No run-time bindings, all global in scope, deletable for localization!
8. Object System (classes, interfaces, and objects!)
9. Multi-arity and optional-parameter support via `fn`
10. No `eqv?`, `memv`, or `assv`: only `eq?` and `equal?` are present.