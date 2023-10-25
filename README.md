<!-- README.md -->

# Eerina's Scheme (🚧 Work In Progress 🚧)

------------------------------------------------------------------------------
## Installing and Running EScheme:
### Dependencies:
* EScheme depends on [Java](https://adoptium.net). Make sure it's installed first!
  - Note that EScheme supports both Java 11 and 17 (LTS releases)!

### Installation:
* Via the command-line, go to the directory you'd like to install `EScheme` in.
* Run: `git clone https://github.com/jrandleman/EScheme`
* Go to the `EScheme/installer` directory.
* Run: `javac Installer.java`
* Run: `java Installer`
  - Use the `-v` or `--verbose` command-line flag to print extra progress messages
* ___Optional:___ if you're using `bash` or `zsh` as your shell:
  - `bash`:
    1. Copy the `alias='...'` output by the installer.
    2. Add it to your `~/.bashrc` (and/or `~/.bash_aliases`) file.
    3. Run: `. ~/.bashrc` (and/or `. ~/.bash_aliases`)
  - `zsh`:
    1. Copy the `alias='...'` output by the installer.
    2. Add it to your `~/.zshrc` file.
    3. Run: `. ~/.zshrc`
  - You can now run `escm` anywhere in the command-line to launch EScheme!

### Execution:
* If you did the optional step during installation:
  - Run `escm` anywhere in your terminal!
* Else:
  - Run `java Main` within the `EScheme/bin` directory!



------------------------------------------------------------------------------
## Command-line flags may be used to modify EScheme's behavior:
1. `-v`, `--version`
   * Print EScheme version information
2. `-h`, `--help`
   * Print this information
3. `-q`, `--quiet`
   * Launch the REPL without ASCII art
4. `-l <script> <arg1> ...`, `--load <script> <arg1> ...`
   * Load `<script>` with `<arg> ...` as `*argv*` into the REPL
5. `-i <module> <arg1> ...`, `--import <module> <arg1> ...`
   * Import `<module>` with `<arg> ...` as `*argv*` into the REPL
6. `<script> <arg1> ...`
   * Interpret `<script>` with `<arg> ...` as `*argv*`
7. \[no arguments\]
   * Launch the REPL


------------------------------------------------------------------------------
## On Reserved Symbols:
* Anything with the `escm-` prefix is considered reserved for use by the runtime.



------------------------------------------------------------------------------
## On Threading & Continuations:
* Threads each have their own stack, and hence their own set of continuations.
* Continuations are delimited by the execution of the thread that created them.
  - Hence a child thread's continuation will never "continue" back into the parent 
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
## EScheme-Specific Concepts (Scheme Extensions/Deviations)

1. Object System (classes, interfaces, and objects!)
2. Module System (file-specific variables & macros!)
3. Multithreading Support (threads and reentrant locks!)
4. Compile-time Procedural Macros
5. Multi-arity and optional-parameter function support via `fn`!
6. Stackless function calls (no stack overflow from recursion!)
7. Trivially `define`/`set!` mulitple values: `(define a b '(1 2))`
8. Generic algorithmic collection primitives (string, list, vector, hashmap) 
9. Vector literals have been reworked
   - Uses `[<obj> ...]` instead of `#(<obj> ...)`
   - Quotes aren't always required: 
     * `[a b c]` compiles to vector of `a`, `b`, & `c` evaluated as variables
   - Quotes can be used though to get expected results: 
     * `(quote [a b c])` => `[(quote a) (quote b) (quote c)]`
10. Hashmap literals have been added in
    - Use `{<key> <value> ...}`
    - A note on hashing:
      * Immutable values hash based on contents (think numbers, symbols, pairs, etc.)
      * Mutable values hash based on identity (e.g. ___not___ their contents)
        - This includes vectors, hashmaps, objects, classes, interfaces, etc.
    - Quotes aren't always required: 
      * `{a 42}` compiles to a hashmap with key `a` evaluated as variable
    - Quotes can be used though to get expected results: 
      * `(quote {a 42})` => `{(quote a) (quote 42)}`
11. `#(` reader lambda literal support
    - 1-indexed params of index `i` via `%i` syntax, and a variadic param via `%%`
    - `#(+ 3.14 %2)` => `(lambda (%1 %2) (+ 3.14 %2))`
12. Immutable pairs & strings
    - Mutable pairs may be implemented by users via the object system!
13. Keyword primitive types
    - Like symbols, but prefixed with `:`, & they always evaluate to themselves
14. Reader extensions:
    - `(. <obj>)` is equivalent to `<obj>` for the reader
    - `#eof`, `#void`, `#nil` reader literals yield their respective values
    - `#path` reader literal expands to the current file's parent path string
15. Support for `bytecode`, `compile`, `eval-bytecode`
    - `bytecode`: special form to have the compiler reflect the given bytecode
    - `compile`: convert a quoted escm expression into a quoted bytecode list
    - `eval-bytecode`: evaluate the given quoted bytecode list in the global environment
      * Hence `eval` is equivalent to `(compose eval-bytecode compile)`!
16. No `eqv?`: only `eq?` and `equal?`
17. And so much more! :)