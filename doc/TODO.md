<!-- TODO.md -->

## MORE


[ ] at some point generic algorithms for primitive data structures?
    [X] => TEST ___ALL AC PRIMITIVES___ USING LISTS, THEN DO WITH VECTOR, ETC.
           [X] => LISTS
           [X] => VECTORS
           [X] => STRINGS
           [X] => HASHMAPS
    [X] => BUILD ON EXISTING `AC` PRIMS TO ADD IN `any?`, `every?`
    [X] => BUILD OUT `OC` PRIMS
           [X] => LISTS
           [X] => VECTORS
           [X] => STRINGS
    [ ] => RM/ADJUST EXISTING PRIMS & ___TAKE OFF `ac-`/`oc-` PREFIX!___
           [ ] => REANALYZE PLACE OF CURRENT LIST/STRING/VECTOR/HASHMAP PRIMTIVES W/IN ESCM
               [ ] => WILL LIKELY CLIP SOME & KEEP SOME (ESP. FOR HASHMAP KEY INTROSPECTION & STRING OPS)
    [ ] => ADD TO/ADJUST `primitives.md`
    [ ] => ADD TO/ADJUST `help`
    [ ] => UPDATE GITHUB + TEST ON SCU SERVERS





[ ] => WHAT ABOUT PRIMITIVES LIKE `unfold` ??? LOOK INTO THIS FROM _HEIST_ !!!






[ ] INVESTIGATE POTENTIAL ISSUE WITH `(stringf "%w..." '(1 2 3))`

[ ] CONSIDER CHANGING '= TO OVERLOAD TO 'eq? FOR NON-NUMERICS & HAVE '>= (ETC.) OVERLOAD FOR 'char>=? AND 'string>=? AS NEEDED

[ ] ADD IN `(file-remove-extension <file-path-string> <extension-string>)` (ALSO REMOVES THE ".")







- MAKE SURE ALL "Stream" & "File" & "Reader" & "Writer" INSTANCES ARE PROPERLY CLOSED IN JAVA
  * MAKE SURE MOVE AS MANY "CLOSE" OPERATIONS AS POSSIBLE PRIOR THROWING ERRORS FOR FILES!






- CONSIDER HAVING `eval` & `bytecode-eval` & `load` & `load-from` & `load-serialized` & `serialize` (ETC. AS NEEDED) SUPPORT SANDBOXING FUNCTIONALITY TO EVAL CODE IN A SEPERATE GLOBAL ENVIRONMENT
  => OPTIONAL ARG TO THE EXISTING FCNS DENOTING WHETHER TO SANDBOX THE EVALUATION (#f BY DEFAULT)





- have anonymous lambdas print some unique id for slightly easier debugging






- CHECK (& RECORD HERE) IPHONE NOTES ON ESCM !!!





====================================================================================
====================================================================================
====================================================================================
====================================================================================


"module" statement processed (MUST be @ top of the file) signifies we are exporting new symbols to the set of "public" symbols.

```clj
(module <symbol> ...) ; ignored if being processed by <load>, only <import> is affected here. "<symbol> ..." are public in scope. <load> forces everything to be public.
(module) ; everything in the file is private (but still loaded)
<code> ...
```

When reading in (importing!) a module:
Each symbol is given a private environment pointer that links to the private environment of the module they were defined in?
  => when setting/defining/getting a symbol -- WITHIN THE GLOBAL SCOPE -- first check whether the symbol is public. If so, 
     go ahead and operate on it in the global scope. Otherwise, do it in their attached "private global environment" scope.


support `import-from` `import-once` `import-once-from`
  => hence ideal "best practice" would be to use `(import-once-from #path <filename>)`
  => ALTERNATIVELY: CONSIDER HAVING THERE BE `import` (the current `import-once`) and `import!` (the current `import`)
     * THEN the ideal would be `(import-from #path <filename>)`
       - ORR WHAT IF FOR IMPORTING WE SUPPORT `(import <path-part-1> <path-part-2> ...)`
         => THEN INTERNALLY IMPORT EQV OF `(string-join (list <path-part-1> <path-part-2> ...) *file-separator*)`
         => THEN COULD DO `(import #path <filename>)`
            - but does this hide too much??? could it be deceptively simple?
            - maybe keep the concept of `-once` in, but support the "path-pasting" instead of `-from`
              * do the same for `load`?





--
thread-local global stack of filename symbol origin prefixes (or any module-specific prefix) to use as a hash of private variables during environment binding/refing
  => no prefix needed for public variables
  => should be flexible to add modules imported halfway through a module file to affect subsequent potential public variables in said module file
  => using a stack enables us to import additional modules in 1 thread. import = push, finish importing = pop. peek = the current thread's module's hash-prefix data.
----------------------------------------------------------------------------------------------------------------------------------------------------------------------




PROBLEM: How can we intermix the notion of serialization and modules?
  => MAYBE: if during serialization we detect a `module` clause, an error is signaled?
  => OR WAIT: IS THIS EVEN REALLY STILL AN ISSUE IF WE USE THE "PRIVATE ENV" METHOD?





-> modules: every `load` instance offers a "module" opportunity
   
   * by default, `load` has it such that every symbol is "public" in the "module" (file)
   * however, as soon as at least 1 file is exposed as public from the module, then all other global symbols are private by default
   * defining a module w/ an empty parameter list makes every global symbol private
   * The `#!module` syntax:
     Contrary to most `#!` syntax instances, `#!module` denotes an operation that takes place at compile-time rather than read-time.
     `#!module` tells the compiler to hash any symbol not exposed as "public" in the module with a prefix to 
     obfuscate it in the global scope (ORR MAYBE GET TRICKY WITH HOW CAN MANIPULATE ENVIRONMENT POINTER USE, IE ONE ONLY TO PUBLIC & THE OTHER TO PRIVATE).

     Moreover, modules _ALSO_ tell the compiler to compile and run each of the internal expressions entirely one after another, in order to be able to 
               control exposure semantics of macros
               - why do we care about this though? Can't this symbol obfuscation not just be reader?
                 * then revert to `#module` (instead of `#!module`) since a reader-op (not a compile-op)
                 * issue: don't want reader to obfuscate already "public" symbols in the process (ie don't hash "define")
                   - maybe need to scrap multiple scoped modules & instead "#module" expression must be the first thing in a file
                   - then have "load" always get public symbols (effectively ignores the module declaration)
                     * then support `import` that accounts for module declarations (otherwises loaded non-module files regularly)

               => MAYBE instead of having modules make things private by default, should have them be public, then 
                  their parameter-list has which symbols to make private, making hashing WAY more simple over-all

               - what about potential creative obsfuscation-related abilities gained by having named modules ?


     ```clj
     ; <public-symbol> ::= <local-name> :as <exposed-name> ; exposed as <exposed-name> in files that load this file
     ;                   | <local-name> ; equivalent to: "<local-name> :as <local-name>"
     (module (<public-symbol> ...) <expression> ...)
     ```

====================================================================================
====================================================================================
====================================================================================
====================================================================================










- CONSIDER OPTIONAL SUPPORT FOR ANSI COLORS!
  * have a flag to disable these
  * NOTE: ERRORS/WARNINGS MIGHT BE ABLE TO GET AWAY WITH USING EMOJIS INSTEAD
  * `-n`, `--nansi`










- CHECK WHETHER SHOULD ALWAYS SET LOCAL TO ENGLISH OR NOT WHEN RUNNING ESCM (DOING SO PROGRAMMATICALLY)









- CONSIDER A BREAKPOINT-SETTING FCN/SPECIAL FORM/INSTRUCTION TO HELP WITH DEBUGGING?
  * `(breakpoint <optional-string-descriptor>)`
    => ONLY ACTUALLY BREAKS IF RUNNING THE INTERPRETER IN `-d`, `--debug` MODE 
       => ADD TO BOTH `README` & `command-line` SECTION IN `help`

    => ENTERS IN A `debug>` MENU (similar to `help`)
       => ENABLE SAFE QUERYING OF ENVIRONMENT VALUES
       => IF GIVEN `<optional-string-descriptor>`, PRINT A PREAMBLE TO THE `debug>` PROMPT TO SAY `debug> BREAKPOINT REACHED: <optional-string-descriptor>`

       * MAYBE EFFECTIVELY LAUNCH A REPL IN THE CURRENT ENVIRONMENT THAT, ONCE QUIT OUT OF VIA `#eof`, RESUMES EXECUTION OF THE CURRENT ESCM SESSION FROM WHERE PAUSED
         => NOTE THAT USERS MAY ENTER `(exit)` IN THE BREAKPOINT MENU TO TERMINATE ALL OF ESCM'S EXECUTION

  * ADD IN PRIMITIVE VARIABLE `*debug*` => SET THIS TO WHETHER GIVEN `-d`, `--debug` FLAG @ CMD-LINE

  * ADD IN A PRIMTIIVE `(call-stack)` FUNCTION THAT RETURNS A LIST OF THE CURRENT CALL-STACK (FUNCTION NAMES ARE STRINGS) 
    => CONSIDER HOW TO ALSO RETURN DATA ABOUT SOURCE INFO IF AVAILABLE?
       * RETURN AN ALIST THAT STARTS WITH THE FCN NAM EIN THE CALLSTACK, FOLLOWED BY A LIST (OR NIL) HOLDING DOURCE FILENAME, NEWLINE INTEGER, & COLUMN LINE INTEGER





  



- ADD JSON SUPPORT PRIMITIVES
  => `json->scm`, `scm->json`, `object->json`, `json-datum?`, `json-string?`
  => USE HASHMAPS AND VECTORS AS APPROPRIATE

- ADD CSV SUPPORT PRIMITIVES









- GENERATE DETAILED DOCUMENTION FOR `README.md` AUTOMATICALLY VIA THE `help` ENTRIES !!!
  * should also generate internal links correctly !!!







- RUNTIME TYPING ___!!! THIS WOULD WARRANT A VERSION UPGRADE TO `8.0` !!!___
  
  * CHECK TO MAKE SURE ALL TYPES ARE BEING ENCAPSULATED

  * NOTE: THE BELOW SHOULD ALSO BE AVAILABLE FOR CLASS METHODS !!!
    - GO THRU OTHER SPECIAL FORMS AND SEE WHERE ELSE COULD BE USED !!!

  * CONSIDER: `(define-type <keyword> <unary-predicate-callable>)` `(type? <keyword>)` `(delete-type! <keyword>)` `(type-predicate <keyword>)`

  BASICS: `(def :string (f (:string s) (:any a)))` use keywords to denote basic type checks
                ^           ^           ^
            return value   string       any-type!

          ***NOTE:*** SUPPORT FALSE-ABLE TYPES (SIMILAR TO NULLABILITY) VIA: `:string?`
          
          ARBITRARY PREDICATE TYPING: `(def (g (:type even? arg-name)))` `:type <predicate?-fcn>` => check validity of arg based on predicate. ALSO SUPPORT FALSABLE `:type?`

          COULD THEN HAVE:
            ```scheme
            (defn fun
              (:list ((:string s)) (list s)) ; !!! can dispatch on type !!!
              (:integer ((:number n)) n))
            ```

          NEW LAMBDA SYNTAX: `(lambda <return-type> (<parameter> ...) <body>)`
          NEW FN SYNTAX:     `(fn (<return-type> (<parameter> ...) <body> ...))`
          NEW DEFINE SYNTAX: `(define <return-type> (<procedure-name> <parameter> ...) <body> ...)`
            => `<parameter>` ::= `<type-keyword> <name-symbol>` `(<type-keyword> <name-symbol> <default-value>)`
                                                                  ^
                                                                  will NOT check the type of the `<default-value>`, ONLY new bound args

          ***!!! MUST COMPARE FCN CALL SPEEDS IN HOT LOOPS TO VERIFY THIS DOENSN'T CRIPPLE US !!!***

          PRIMTIIVE TYPES (REMEMBER TO MAKE FALSEABLE `:<name>?` ALTERNATIVES TO EACH OF THE BELOW AS WELL):
            ```scheme
            :number
            :integer
            :real
            :complex
            :exact
            :inexact

            :string
            :keyword
            :boolean
            :symbol
            :void

            :vector
            :hashmap

            :thread
            :mutex

            :nil
            :pair
            :list
            :atom

            :procedure
            :callable
            :functor

            :meta-object
            :object
            :class
            :interface

            :port
            :input-port
            :output-port

            :ac ; :associative-collection
            :oc ; :ordered-collection
            ```










[ ] FROM THE CMD LINE USING `escm` AS A CMD:
    [ ] BOOT SCRIPTS STORED IN A FILE NAMED "`.escm-boot-scripts`"
    [ ] ALLOW THE ADDING OF NEW FILES TO ESCM'S SET OF FILES TO LOAD UP UPON AN INITIAL ESCM PROGRAM LAUNCH
        (BASICALLY JUST A WAY FOR USERS TO ADD THEIR OWN "stdlib"S TO BE LOADED UPON UPON BOOT-TIME)
        `$ escm -bs-ls,  --boot-script-list`
        `$ escm -bs-mk,  --boot-script-make <filename> ...` // moves `<filename>` to the end of the boot-script list if it already exists (can't have 1 file 2+ times)
        `$ escm -bs-rm,  --boot-script-remove <filename> ...` // returns whether successfully deleted all of them.
        `$ escm -bs-s,   --boot-script-swap <src-filename> <dest-filename>` // put `<src>` into `<dest>`'s position, & `<dest>` into `<src>`'s if `<src>` already exists
        `$ escm -bs-e,   --boot-script-exists <filename> ...` // returns whether each filename is a boot script (`#t` vs `#f`)
        `$ escm -bs-cat, --boot-script-concatenate <filename>` // appends all of the boot-script entries in `<filename>` to the current boot-script list.
        [ ] ALSO HAVE A WAY TO DO THIS PROGRAMATICALLY:
            `(boot-script-list)`
            `(boot-script-make! <filename-str> ...)`
            `(boot-script-remove! <filename-str> ...)`
            `(boot-script-swap! <src-filename-str> <dest-filename-str>)`
            `(boot-script-exists? <filename-str> ...)`
            `(boot-script-concatenate! <filename-str>)`
    [ ] HENCE IN THE INSTALL JAVA SCRIPT:
        `$ escm --serialize-stdlib`
        `$ escm --boot-script-make ../bin/stdlib.ser`










- AFTER RUNTIME TYPING, CONSIDER RUNTIME ACCESS MODIFIERS FOR OBJECTS (NOTE THAT `:public` MUST BE ___FAST___ !!!)





- ADD IN MORE DIVERSE/COMPLEX SAMPLE FILES TO THE `doc/example` DIRECTORY





- LOGIC PROGRAMMING FUNCTIONS ???

- NETWORKING PRIMITIVES

- GUI PRIMITIVES
  
  * GET TO THE POINT WHERE CAN GENERATE A FRACTAL !!!

  * CONSIDER A -i, --interactive FLAG TO LAUNCH THE REPL IN A GUI WINDOW THAT DETECTS KEYSTROKES, STORES HISTORY VIA ^v, ETC. 

- LOOK BACK AT HEIST-SCHEME FOR ANY OTHER FEATURES TO IMPLEMENT

- CHECK IPHONE `ESCM TODOS` NOTE