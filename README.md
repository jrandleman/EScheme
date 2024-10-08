<!-- README.md -->

# Eerina's Scheme (🚧 Work In Progress 🚧)

---

## Installing and Running EScheme:

### Dependencies:

- EScheme depends on [Java](https://adoptium.net). Make sure it's installed first!
  - Note that EScheme supports Java 21 (LTS release)!

### Installation:

- Via the command-line, go to the directory you'd like to install `EScheme` in.
- Run: `git clone https://github.com/jrandleman/EScheme`
- Go to the `EScheme/installer` directory.
- Run: `javac Installer.java`
- Run: `java Installer`
  - Use the `-v` or `--verbose` command-line flag to print extra progress messages
- **_Optional:_** if you're using `bash` or `zsh` as your shell:
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

- If you did the optional step during installation:
  - Run `escm` anywhere in your terminal!
- Else:
  - Run `java Main` within the `EScheme/bin` directory!

---

## EScheme Command-Line Flags:

1. `-v`, `--version`
   - Print EScheme version information
2. `-h`, `--help`
   - Print this information
3. `-q`, `--quiet`
   - Launch the REPL without ASCII art
4. `-l <script> <arg1> ...`, `--load <script> <arg1> ...`
   - Load `<script>` with `<arg> ...` as `*argv*` into the REPL
5. `-i <module> <arg1> ...`, `--import <module> <arg1> ...`
   - Import `<module>` with `<arg> ...` as `*argv*` into the REPL
6. `<script> <arg1> ...`
   - Interpret `<script>` with `<arg> ...` as `*argv*`
7. \[no arguments\]
   - Launch the REPL

---

## Learning EScheme:

EScheme builds on R4RS Scheme: designed to be the C++ to it's C, EScheme adapts
the LISP for contemporary use by supporting many paradigms programmers have come
to expect from modern tools.

Despite being inspired by a broad variety of languages (including Clojure, Scala,
Smalltalk, Python, JavaScript, Java, & C++), EScheme's base draws most heavily on
R4RS and R5RS Scheme. You can find tutorials for:

1. [R4RS here!](https://people.csail.mit.edu/jaffer/r4rs_toc.html)
2. [R5RS here!](https://groups.csail.mit.edu/mac/ftpdir/scheme-reports/r5rs-html/r5rs_toc.html)

[While EScheme deviates from R(4/5)RS in some key ways](#escheme-specific-concepts-scheme-extensionsdeviations), having read the tutorials
above, you'll be broadly ready to jump directly into learning EScheme! However,
if you'd like a bit more practice first, you can first check out this textbook:
"The Structure and Interpretation of Computer Programs" (or _SICP_ for short).
Click on the links below for:

- [SICP 1st Edition Homepage](https://mitp-content-server.mit.edu/books/content/sectbyfn/books_pres_0/6515/sicp.zip/index.html): This has an outdated version of the textbook,
  but it still links to solid programming exercises you can try out.
- [SICP 2nd Edition Full Text](https://web.mit.edu/6.001/6.037/sicp.pdf): You should follow along this text with a
  compliant Scheme implementation at the ready, it's the best way to learn!
- [Chez Scheme's Homepage](https://cisco.github.io/ChezScheme/), and [Chez Scheme's Source Code](https://github.com/cisco/ChezScheme): Chez Scheme is a
  fantastic compliant Scheme implementation. Follow the former link for
  general information, and the latter to build Chez Scheme on your own system.
  - Note: if building from source isn't your thing, just google how to install
    Chez with any package manager of your choice (it's pretty popular).
- [SICP Video Lecture Series by Scheme's Inventor](https://ocw.mit.edu/courses/6-001-structure-and-interpretation-of-computer-programs-spring-2005/video_galleries/video-lectures/): This gem of a series comes
  thanks to MIT's "OpenCourseWare" program. Despite only following along
  SICP's 1st edition, the books are similar enough to translate very well!

[Beyond README.md's information on EScheme's R4RS extensions](#escheme-specific-concepts-scheme-extensionsdeviations), you can find more
information on EScheme's broader extensions in the `doc` directory:

- [`primitives.md`](https://github.com/jrandleman/EScheme/tree/main/doc/primitives.md): Broad overview of EScheme's primitive functions.
- [`concurrency.md`](https://github.com/jrandleman/EScheme/tree/main/doc/concurrency.md): Threading semantics and dynamic environments.
- [`modules.md`](https://github.com/jrandleman/EScheme/blob/main/doc/modules.md): Module system, as well as its reflection.
- [`objects.md`](https://github.com/jrandleman/EScheme/blob/main/doc/objects.md): Single-inheritance multiple-interfaces object system.
- [`instructions.md`](https://github.com/jrandleman/EScheme/blob/main/doc/instructions.md): Inline bytecode, and learn EScheme's instruction set.
- [`examples`](https://github.com/jrandleman/EScheme/tree/main/doc/examples): Directory with sample EScheme files.

Having learned R4RS Scheme and EScheme's deviations, you're set to code now! 🙌

Keep reading this README for a table of contents on EScheme's language features!
For an interactive solution, EScheme draws from Python with [a `help` function](https://github.com/jrandleman/EScheme/tree/main/doc/primitives.md#help):

1. `(help)`: Launch EScheme's interactive "help" menu to explore the language
2. `(help <obj>)`: Get introspected information on the `<obj>` EScheme datum
   - **_Tip: Always pass functions and macros to `help` the first time you use them!_**

---

## Reserved Symbols:

Anything with the `escm-` prefix is considered reserved for use by the runtime.

---

## EScheme-Specific Concepts (Scheme Extensions/Deviations):

1. Type System (optional procedural runtime types, union and parameterized types)
2. Object System (classes, interfaces, and objects)
3. Module System (file-specific variables & macros)
4. Multithreading Support (promises, threads, and mutexes)
5. Compile-time procedural macros
6. Multiple dispatch procedures via `fn` with optional parameter support
7. Stackless function calls (no stack overflow from recursion)
8. Trivially `define`/`set!` multiple values: `(define a b '(1 2))`
9. Generic collection algorithm primitives (string, list, vector, hashmap)
10. Vector literals have been reworked
    - Uses `[<obj> ...]` instead of `#(<obj> ...)`
    - Quotes aren't always required:
      - `[a b c]` compiles to vector of `a`, `b`, & `c` evaluated as variables
    - Quotes can be used though to get expected results:
      - `(quote [a b c])` => `[(quote a) (quote b) (quote c)]`
11. Hashmap literals have been added in
    - Use `{<key> <value> ...}`
    - A note on hashing:
      - Immutable values hash based on contents (think numbers, symbols, pairs, etc.)
      - Mutable values hash based on identity (e.g. **_not_** their contents)
        - This includes vectors, hashmaps, objects, classes, interfaces, etc.
    - Quotes aren't always required:
      - `{a 42}` compiles to a hashmap with key `a` evaluated as variable
    - Quotes can be used though to get expected results:
      - `(quote {a 42})` => `{(quote a) (quote 42)}`
12. `#(` reader lambda literal support
    - 1-indexed params of index `i` via `%i` syntax, and a variadic param via `%%`
    - `#(+ 3.14 %2)` => `(lambda (%1 %2) (+ 3.14 %2))`
13. Immutable pairs and strings
    - Mutable pairs may be implemented by users via the object system
14. Keyword primitive types
    - Like symbols, but prefixed with `:`, & they always evaluate to themselves
15. Reader extensions:
    - `(. <obj>)` is equivalent to `<obj>` for the reader
    - `#eof`, `#void`, `#nil` reader literals yield their respective values
    - `#path` reader literal expands to the current file's parent path string
16. Support for `bytecode`, `compile`, `eval-bytecode`
    - `bytecode`: special form to have the compiler reflect the given bytecode
    - `compile`: convert a quoted escm expression into a quoted bytecode list
    - `eval-bytecode`: evaluate the given quoted bytecode list in the global environment
      - Hence `eval` is equivalent to `(compose eval-bytecode compile)`
17. No `eqv?`: only `eq?` and `equal?`
18. And so much more! :)

---

## Automatically Generated Language Documentation:

[doc/primitives.md](https://github.com/jrandleman/EScheme/blob/main/doc/primitives.md) contains the result of executing [`(help-markdown)`](https://github.com/jrandleman/EScheme/blob/main/doc/primitives.md#help-markdown), which
yields a string of the [`(help)`](https://github.com/jrandleman/EScheme/tree/main/doc/primitives.md#help) directory's contents in alphabetical order as
a markdown file.

Note that [primitives.md](https://github.com/jrandleman/EScheme/blob/main/doc/primitives.md) is not intended to be a Scheme programming tutorial: the internet
is already rife with many wonderful alternatives. Hence, its documentation is only
intended to be searched by those who already know what they are looking for, with
EScheme's most unique features being referenced from traditional Scheme entries.
