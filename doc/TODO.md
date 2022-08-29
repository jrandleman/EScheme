<!-- TODO.md -->

## MORE



- CONSIDER HAVING `load` & `load-from` ALSO POPULATE THE `load-once` BUFFER TOO !!!






====================================================================================
====================================================================================
====================================================================================
====================================================================================

-> CHANGE THE READER MACRO `#path` TO BE `#!path`
   * USE `STRFIND` TO VERIFY INSTANCE & CHANGE AS NEEDED ACCORDINGLY

   - WHAT ABOUT CHANGING `#nil` => `#!nil`
                         `#void` => `#!void`
                         `#eof` => `#!eof`
                         `#path` => `#!path`



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
               - why do we give a fuck about this though? Can't this symbol obfuscation not just be reader?
                 * then revert to `#module` (instead of `#!module`) since a reader-op (not a compile-op)
                 * issue: don't want reader to obfuscate already "public" symbols in the process (ie don't hash "define")
                   - maybe need to scrap multiple scoped modules & instead "#module" expression must be the first thing in a file
                   - then have "load" always get public symbols (effectively ignores the module declaration)
                     * then support `import` that accounts for module declarations (otherwises loaded non-module files regularly)
               <!-- - not sure, shee-it jack this may be harder than anticipated (rip) ;; => oof ;; => nah actually though -->

               => MAYBE instead of having modules make things private by default, should have them be public, then 
                  their parameter-list has which symbols to make private, making hashing WAY more simple over-all

               - what about potential creative obsfuscation-related abilities gained by having named modules ?


     ```lisp
     ; <public-symbol> ::= <local-name> :as <exposed-name> ; exposed as <exposed-name> in files that load this file
     ;                   | <local-name> ; equivalent to: "<local-name> :as <local-name>"
     (#!module (<public-symbol> ...) <expression> ...)
     ```

====================================================================================
====================================================================================
====================================================================================
====================================================================================







- ADD IN `(file-remove-extension <file-path-string> <extension-string>)` (ALSO REMOVES THE ".")







- MAKE SURE ALL "Stream" & "File" & "Reader" & "Writer" INSTANCES ARE PROPERLY CLOSED IN JAVA
  * MAKE SURE MOVE AS MANY "CLOSE" OPERATIONS AS POSSIBLE PRIOR THROWING ERRORS FOR FILES!









- CONSIDER HAVING `eval` & `bytecode-eval` & `load` & `load-from` & `load-serialized` & `serialize` (ETC. AS NEEDED) SUPPORT SANDBOXING FUNCTIONALITY TO EVAL CODE IN A SEPERATE GLOBAL ENVIRONMENT
  => OPTIONAL ARG TO THE EXISTING FCNS DENOTING WHETHER TO SANDBOX THE EVALUATION (#f BY DEFAULT)











- have anonymous lambdas print some unique id for slightly easier debugging













- CHECK (& RECORD HERE) IPHONE NOTES ON ESCM !!!













- SUPPORT OTHER ESCAPE JAVA CHARS IN STRINGS AS NEEDED
  * ESCPECIALLY UNICODE (`\u`) SUPPORT !!!
    => SEE `StringParser.java`
       * ___UPDATE IN `json` AS WELL___


       `Character.toString(unicodeCodePointInt)` 
        => NOTE: CONSIDER HOW ESCM SHOULD SUPPORT THIS: want to be able to support 32 bit unicode printing.
           BUT doing `(string-split <string> "")` splits across 16bit chars

        => MAYBE HAVE escm.type.String BE MORE COMPLEX? 
           => ARRAY OF PAIRS OF CODEPOINTS ?

           => ___ISSUE: CAN'T LEVERAGE JAVA REGEX FUNCTIONS IFF USING THIS ADVANCED STRUCTURE___

           => ___POSSIBLE SOLUTION: POTENTIALLY SUPPORT `\uXXXX` FOR TRUE JAVA CHARACTERS___
              * ___ALSO SUPPORT `\Uxxxxxxxx` FOR 32-BIT CHARSEQS W/IN A STRING (MENTION THIS CORRELATES TO A CHAR SEQUENCE, NOT A CHAR!)___


  * ADD DETAILS TO `README` & `help` REGARDING WHAT ESCAPE CHARACTERS ARE / AREN'T SUPPORTED IN STRINGS !!!









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

  BASICS: `(def :string (f :string s :any a))` use keywords to denote basic type checks
                ^          ^         ^
            return value  string     any-type!

          ***NOTE:*** SUPPORT FALSE-ABLE TYPES (SIMILAR TO NULLABILITY) VIA: `:string?`
          
          ARBITRARY PREDICATE TYPING: `(def (g :type even? arg-name))` `:type <predicate?-fcn>` => check validity of arg based on predicate. ALSO SUPPORT FALSABLE `:type?`

          COULD THEN HAVE:
            ```scheme
            (defn fun
              (:list (:string s) (list s)) ; !!! can dispatch on type !!!
              (:integer (:number n) n))
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
            ```










- AFTER RUNTIME TYPING, CONSIDER RUNTIME ACCESS MODIFIERS FOR OBJECTS (NOTE THAT `:public` MUST BE ___FAST___ !!!)





- ADD IN MORE DIVERSE/COMPLEX SAMPLE FILES TO THE `doc/example` DIRECTORY





- LOGIC PROGRAMMING FUNCTIONS ???

- NETWORKING PRIMITIVES

- GUI PRIMITIVES
  
  * GET TO THE POINT WHERE CAN GENERATE A FRACTAL !!!

  * CONSIDER A -i, --interactive FLAG TO LAUNCH THE REPL IN A GUI WINDOW THAT DETECTS KEYSTROKES, STORES HISTORY VIA ^v, ETC. 

- LOOK BACK AT HEIST-SCHEME FOR ANY OTHER FEATURES TO IMPLEMENT

- CHECK IPHONE `ESCM TODOS` NOTE