<!-- TODO.md -->

## MORE

====================================================================================
====================================================================================
====================================================================================
====================================================================================



[ ] => LOOK INTO HOW PYTHON HANDLES MODULES:
       * https://docs.python.org/3/tutorial/modules.html
       * https://docs.python.org/3/tutorial/classes.html#tut-scopes
    => "NAMESPACE OBJECTS" ?




.
=====
# CURRENT DIRECTIVES:

[ ] IMPLEMENT MODULES
[ ] MENTION THAT `dosync` OPERATES RELATIVE TO MODULES (EACH MODULE GETS ITS OWN NEW UNDERLYING LOCK USED BY `dosync` IN THAT MODULE'S CONTEXT)
[ ] UPDATE DOCUMENTATION (`help` + FILES IN `/doc/` + `README.md`) WITH PROGRESS
    [ ] MENTION THAT MACROS ARE ALSO MACRO-SPECIFIC
[ ] DELETE OLD NOTES IN THIS "TODO" FILE
[ ] TACKLE "AFTER MODULE TODOS"
[ ] UPDATE DOCUMENTATION (`help` + FILES IN `/doc/`) WITH PROGRESS
=====




--

ISSUE: NEED TO TRACK MACROS SEPERATELY IN SEPERATE MACROS
  => HENCE CAN'T JUST TRACK THEM ALL GLOBALLY
  => GIVE THE COMPILER THE HASHMAP TO STORE MACROS IN??
     -> THEN MANAGE THAT HASHMAPS RELATIONSHIP TO OTHER HASHMAPS SEPERATELY??
     -> HAVE A WAY TO REFER TO MACROS IN OTHER MODULES
        * NOTE THAT `(import <mod>.<sym> :as <alias>)` SHOULD WORK ON BOTH VARIABLES && MACROS!
        * WILL LIKELY INVOLVE SENDING ENV PTRS TO THE `Compiler.run` FCN TOO, IN ORDER TO HAVE MACROS EXPAND RELATIVE TO MODULES
        * ___NOTE___: THIS CHANGE WILL LIKELY CHANGE A FEW VARIABLES FROM BEING STORED IN `GlobalState` TO BE STAORED A THREAD-LOCAL VARIABLE IN THE CURRENT THREAD!
          - E.G. `loadedOnceFiles`
==============

NEED TO CONSIDER IMPORTING SEMANTICS FOR FILE EXTENSIONS: SHOULD THEY BE IGNORED? 
  => E.G. YOU'D IMPORT "file.scm" AND "file.escm" AS BOTH `(import file)`
==============

ORR ONLY HAVE MODULE LOGIC BE IMPLEMENTED BY `escm-import` JAVA PRIMITIVE THAT IMPLEMENTS THE `import` LOGIC!
  => E.G. DON'T BOTHER TRYING TO CREATE A MODULE FOR THE CURRENT FILE. SIMPLY HAVE LOADING THE ESCM JAVA/ESCM PRIMITIVES DO SO IN ANOTHER SEPERATE ENV VARIABLE THAT ACTS AS THE "NEXT" PTR TO THE ENVIRONMENT OF THE MAIN FILE BEING EVAL'D (OR THE "REPL"). 
  => HENCE MODULES ARE ___ONLY___ CREATED BY USING `import`: THE IMPORTED FILE IS SYNCHRONOUSLY EVALUATED IN A SEPERATE GLOBAL ENV VARIABLE WHO'S NEXT PTR IS THE ENV VARIABLE WITH ALL OF THE ESCM PRIMITIVES DEFINED
  => THEN DEFINE THAT MODULE AS A VARIABLE IN THE "importing" FILE !!!
     => this ought to avoid the need for a "stack" of modules in the current thread, or any BS like that
.
==============

NEED SOME META-MODULE TO CONTAIN THE DEFNS OF PRIMITIVES FROM `stdlib.scm` SO THAT MODULES IMPORTED INTO THEIR OWN CONTEXT STILL HAVE ACCESS TO PRIMITIVE VARIABLES
==============

SEE IPHONE PIC
==============

WHAT IF `(import <module>.<symbol>)` DEFINED A READER MACRO `<symbol>` THAT EXPANDED TO `<module>.<symbol>` 
  => (THEN `(import <module>.<symbol> :as <symbol2>)`) WOULD DEFN A READER MACRO `<symbol2>` FOR `<module>.<symbol>`

  ISSUES FROM SUCH:
    1) `<module>` WOULD THEN ALSO BE DEFINED AS A VARIABLE WITHIN THE CURRENT SCOPE
       => MAYBE THIS IS OK IFF WE EXPLAIN SUCH IN THE DOCUMENTATION AS EXPECTED BEHAVIOR?
    2) QUOTING `'<symbol>` WOULD PRINT AS `'<module>.<symbol>`
    3) ==== THIS MAY NOT ACTUALLY WORK W/O COMPLICATIONS ==== IMAGINE A FILE LOADING A MODULE WITH AN ALIAS, THEN IMPORTING THAT FILE INTO ANOTHER FILE THAT WANTS TO REFER TOT HE ALIAS 
       =>  ***** HENCE WOULD HAVE TO MERGE READER MACROS? *****

NOTE: MAYBE HAVE `(import <module>.<symbol>)` YIELD `(begin (import <module>) (def <symbol> <module>.<symbol>))`
  THIS IS ONLY AN ISSUE IF SAY WE HAVE TO SHARE STATE BETWEEN MODULES ACROSS SEVERAL FILES
  => suppose module "m" with global variable "i". file "f1" imports "m" and increments "i". file "f2" imports "m" and prints i (gets the unincremented version)
==============

NEED TO PREVENT ISSUES W/ CIRUCLAR IMPORTS (SHOULD BE HANDLED BY THE DEFAULT `import` MACRO, E.G. DON'T HAVE A SEPERATE `import-once` ALT): REFER TO CURRENT SOLUTIONS FOR CIRCULAR LOADS
==============

ALSO SUPPORT IMPORTING SERIALIZED FILES (REFER TO HOW `load`  OPERATES)
==============

OR JUST DEFINE `(import <module>.<sym>)` IN THE DOCUMENTATION TO BIND SUCH TO `<sym>` IN THE CURRENT MODULE
  => NOTE: WE'D LIKE TO SUPPORT `(import folder.innerFolder.module)` & `(import folder.innerFolder.module.obj)` SYNTAX TOO
     * ISSUE: how to determine if the last symbol in the dot-chain is supposed to be a module getting entirely imported or if its an object within a module (which would be the 2nd-to-last symbol in the dot-chain)
       - SOLN: if 2nd to last symbol is a folder & last symbol is a file, import as a module
               - else: treat as if 2nd to last symbol is a file & last symbol is an entity to bind as its own name in the current module
==============


.
=====
# TODOS AFTER IMPLEMENTING MODULES: 

[ ] CONSIDER A READER MACRO `#imported` THAT EVALS TO `#t` OR `#f` DEPENDING ON WHETHER THE CURRENT MACRO IS BEING IMPORTED OR NOT!
    => DETERMINE HOW TO HANDLE THIS WHEN `load`ING FILES THOUGH!

[ ] CHANGE `loadedOnceFiles` TO BE A VARIABLE STORED IN THE CURRENT ENVIRONMENT: `*escm-loaded-once-files*`

[ ] MENTION IN README/HELP THAT META-THREAD ENVIORNMENTS ARE ___NOT___ MODULE-SPECIFIC!

[ ] SEEK & ALIMINATE ALL REFERENCES TO `tdwf`
    => REMENANT FROM OLD TESTING

[ ] CHANGE DOCUMENTATION IN `help`/README/ETC. (SEE ALL OTHER DOCS) TO MENTION THAT MACROS + VARIABLES ARE IN THE SAME NAMESPACE, AND THAT MACROS ARE VARIABLES IN A MODULE JUST LIKE ANY RUN-TIME VALUE
    => ONLY DO THIS AFTER CHANGING MACROS TO BE BOUND INSIDE OF THE GLOBAL ENVIRONMENT PASSED AS AN ARG TO THE COMPILER, RATHER THAN BEING IN A SEPERATE REGISTRY

[ ] TEST WITH SERIALIZED AND INTERPRETED VERSIONS OF THE STDLIB (DETERMINED BY INSTALLATION)

[ ] REMEMBER TO UPDATE `modules.md` IN `doc` PRIOR COMMITTING TO GITHUB TOO!
    => CONSIDER ALSO MVING `test.scm` INTO SOMEWHERE ELSE BESIDE `EScheme` TOO ... SHOULDN'T BE HAVING THAT ON THE GITHUB

[ ] AFTER MODULES ARE COMPLETED, ACCOUNT FOR SUCH IN ALL `help` DOCUMENTATION (E.G. ADJUST REFERENCES TO "THE GLOBAL SCOPE" TO ACCOUNT FOR MODULES)

[ ] CONSIDER CMD-LINE ARGUMENTS `--import <folder>.<filename>` `-i <folder>.<filename>` TO ACT LIKE `load` BUT AS AN IMPORTED MODULE

[ ] CHANGE THE ESCM VERSION TO BE `9` AFTER SOLVING MODULES!
=====






Walk Module Ideas:
  => Modules as extending meta-objects?
     => OR just as a thin wrapper around a hashmap like a meta-object, but w/o the notion of `<super>` or implemented interfaces (doesn't make sense in this context).

     => NOTE: MAY WANT TO CHANGE <MetaObject> TO _NOT_ IMPLEMENT THE <get> ETC. FCNS, THEN HAVE ANOTHER ABSTRACT CLASS FOR OBJECTS THAT EXTENDS IT IN ORDER TO PROVIDE SUCH AS A BASE FOR CLASSES/INTERFACES/OBJECTS/ETC.
        => REASON: CURRENT <MetaObject> DOES BINDING WITH <self> TO METHODS, SOMETHING WE ___DO_NOT_WANT___ FOR OUR MODULES TO DO (POINTLESS + EXPENSIVE)

    => NOTE: Consider having <MACRO_REGISTRY> from <Compiler.java> be module-specific too?

  => No "global env", only "current module"
     => Instead of global env being thread-independant, have "current-module" be a thread-dependant variable (in case a child thread imports)
  => Should have a global set of "imported modules"
     => Each module can only be imported once, future imports from the same module reference objects created during the evaluation of the module in its first import
  => `(load "filename.scm")` then is equivalent to `(import :* :from "filename.scm")`
  => CAN ALSO DO: `(import "filename.scm")` THEN REFERENCE ANY OBJECT W/IN IT VIA `filename.obj` ?

     => NOTE: PRIOR FINALIZING ON THE BELOW SYNTAX/ITS EFFECTS, CONSIDER EXPERIMENTING W/ PYTHON TO SEE HOW IT HANDLES LOCALIZATION!
              [ ] => ALTHOUUUUUUUUGH WHO SAYS ONLY PYTHON IS RIGHT? MAYBE ALSO SUPPORT THE ORIGINAL SYNTAX (BELOW) BY BEING INSPIRED BY JAVA?
              [ ] => Python seems to only allow importing modules, even with dots: can't do `import mod.function`
                     Then would only need to support the following syntax:
                      `(import <module-name>)`
                      `(import <module-name> :as <module-alias-name>)`
              [ ] => ALSO WANT TO CONSIDER DIRECTORY SUPPORT? SOMETHING LIKE JAVA'S PACKAGE SYSTEM?
                     [ ] => NAMSPACES TO HELP WITH THIS? TOP-LEVEL NAMSPACE DESIGNATIONS?
                            `(namspace types.numbers.exact)`
                            `(package types.numbers.exact)`
                            #!package types.numbers.exact

     => General Syntax Ideas:
        `(import <module-name>)`
        `(import <module-name> :as <module-name-alias>)`
        `(import <module-name>.<obj1>)`
        `(import <module-name>.<obj1> :as <obj1-name>)`
        `(import <obj1> <obj2> ... :from <module-name>)`
        `(import <obj1> <obj2> ... :from <module-name> :as <obj1-name> <obj2-name> ...)`

        [ ] => BEWARE WITH "import as" SINCE CAN'T JUST REDEFINE/BIND-DEFINE IN THE LOCAL MODULE SINCE `set!`ING THAT REDEFINED SYMBOL WOULDN'T AUTO CHANGE THE MODULE'S SYMBOL TOO!
               [ ] => ISSUE WITH DOING A "READER MACRO SWAP" SINCE:
                      ```scheme
                      (import mod.obj :as alias)
                      (display 'alias) ; => reader macro would make this print <mod.obj> (invalid)
                                       ; => hence need to track MORE metadata with symbols? a "anchored <ObjectAccessChain> instance?"
                      ```
               [ ] => ORRRR do we even really care about this? should we enable setting/defining via the alias? or could we just have the README define the action of aliasing an imported object as binding that imported object to a locally defined variable? hence CAN still set/define such, but only by doing it on the explicit object chain (note this is potentially even benficial for clarity's sake, given how serious of a change to the runtime such an action can cause)
                      [ ] => HENCE `(import mod.obj)` IS EQUIVALENT TO `(let ((gs (gensym))) (eval `(begin (import mod :as ,gs) (define obj ,(symbol-append gs '. 'obj)))))`

  => Since every file is hence module, there's no need for a `module` special form.
  => Allow module introspection?
     => Could be getting this for free by just having modules extend the "meta-object" class, else implement module-specific features.
  => Then maybe need restrictions on escm file naming conventions?
     => ESCM file names must also be valid ESCM variable names to work as a module properly (no spaces, parenthesis/brackets/braces, periods, etc.)
     => Automatically have the extension of the file removed when defining the module name 
        => e.g. "filename.scm" as a module is referred to as `filename`
  => Support importing serialized files

--





















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






[ ] IMPROVE ASCII ART AGAIN (specifically wording under the intro credit)!








[ ] INVESTIGATE `pprint`ING ISSUE WHERE ` ` GETS PRINTED BY THE BELOW CODE WHEN RUN IN SUBLIME TEXT:

    ```scheme
    (pprint '(define (escm-quasiquote->quote lst level)
      (define (iter lst)
        (define hd (if (not (atom? lst)) (car lst)))
              ; finished parsing expression (proper list)
        (cond ((null? lst) (quote ()))
              ; quasiquote vector
              ((vector? lst)
                (list (list (quote list->vector) (escm-quasiquote->quote (vector->list lst) level))))
              ; quasiquote hashmap
              ((hashmap? lst)
                (list (list (quote list->hashmap) (escm-quasiquote->quote (hashmap->list lst) level))))
              ; finished parsing expression (dotted list)
              ((atom? lst)
                (list (list (quote quote) lst)))
              ; unquote rest of list
              ((escm-quasiquote-tagged-list? lst (quote unquote))
                (if (= level 0)
                    (list (cadr lst))
                    (list (list (quote list) (quote (quote unquote)) (escm-quasiquote->quote (cadr lst) (- level 1)))))) ; *there*: recursively parse, in nested quasiquote
              ; quasiquote vector
              ((vector? hd)
                (cons (list (quote list) (list (quote list->vector) (escm-quasiquote->quote (vector->list hd) level)))
                      (iter (cdr lst))))
              ; quasiquote hashmap
              ((hashmap? hd)
                (cons (list (quote list) (list (quote list->hashmap) (escm-quasiquote->quote (hashmap->list hd) level)))
                      (iter (cdr lst))))
              ; quote atom
              ((atom? hd)
                (cons (list (quote list) (list (quote quote) hd))
                      (iter (cdr lst))))
              ; unquote datum
              ((escm-quasiquote-tagged-list? hd (quote unquote))
                (if (= level 0)
                    (cons (list (quote list) (cadr hd))
                          (iter (cdr lst)))
                    (cons (list (quote list) (escm-quasiquote->quote hd level)) ; recursively parse, in nested quasiquote (level will be decremented *there*)
                          (iter (cdr lst)))))
              ; unquote & signal should splice element
              ((escm-quasiquote-tagged-list? hd (quote unquote-splicing))
                (if (= level 0)
                    (cons (cadr hd) ; evaluate datum & append to the expression
                          (iter (cdr lst)))
                    (cons (list (quote list) (escm-quasiquote->quote hd (- level 1))) ; recursively parse, in nested quasiquote
                          (iter (cdr lst)))))
              ; nested quasiquote
              ((escm-quasiquote-tagged-list? hd (quote quasiquote))
                (cons (list (quote list) (escm-quasiquote->quote hd (+ level 1))) ; recursively parse, in nested quasiquote
                      (iter (cdr lst))))
              ; quasiquote expression
              (else
                (cons (list (quote list) (escm-quasiquote->quote hd level))
                      (iter (cdr lst))))))
      (cons (quote append) (iter lst))))
    (newline)
    ```









[ ] CONSIDER SUPPORTING THE CONCEPT OF THE `.escm-paths` FILE:
  => THIS FILE WOULD HOLD A LIST OF PATHS TO CHECK IF THE DEFAULT `load`/`import` behavior can't find the file
     * EFFECTIVELY LIKE THE `$PATH` ENV VARIABLE, BUT IT IS ONLY USED BY ESCM
     * SHOULD ALSO WORK FOR OTHER FILE READING OPTIONS (LIKE `file-read`, `read`, ETC.)
  => ALSO INCLUDES ADDING IN ALL OF THE CMD-LINE AND FUNCTION PRIMITIVE SUPPORT REQUIRED IN ORDER TO MODIFY SUCH
     * `$ escm --add-path <path>` | `$ escm -ap <path>` // every argument afterwards would be used to create the path, and would immediately kill the process after adding the path (like how `help` kills the process after printing its relevant information)
     * `$ escm --list-paths` | `$ escm -lp`
     * `$ escm --remove-path <path>` | `$ escm -rp <path>`
     * NOTE: WOULD WANT TO ADD ALL OF THESE COMMANDS TO THE `help` MENU && `README.md` !!!











[ ] MAKE SURE ALL "Stream" & "File" & "Reader" & "Writer" INSTANCES ARE PROPERLY CLOSED IN JAVA
    * MAKE SURE MOVE AS MANY "CLOSE" OPERATIONS AS POSSIBLE PRIOR THROWING ERRORS FOR FILES!










- CONSIDER CHANGING SYNTAX FOR READER SHORTHAND LAMBDAS FROM `\` TO `#`: `#(* %1 2)`
  * would make this more ide-friendly for existing scheme pkgs












- CONSIDER A BREAKPOINT-SETTING FCN/SPECIAL FORM/INSTRUCTION TO HELP WITH DEBUGGING?
  * `(breakpoint <optional-string-descriptor>)`
    => ONLY ACTUALLY BREAKS IF RUNNING THE INTERPRETER IN `-d`, `--debug` MODE 
       => ADD TO BOTH `README` & `command-line` SECTION IN `help`

    => ENTERS IN A `debug>` MENU (similar to `help`)
       => ENABLE SAFE QUERYING OF ENVIRONMENT VALUES
       => IF GIVEN `<optional-string-descriptor>`, PRINT A PREAMBLE TO THE `debug>` PROMPT TO SAY `debug> BREAKPOINT REACHED: <optional-string-descriptor>`
          - ALSO INCLUDE THE FILE/LINE/COL DATA STORED FROM THE `<breakpoint>` SYMBOL IN THE `debug>` PREAMBLE

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





====================================================================================
====================================================================================
====================================================================================
====================================================================================

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

====================================================================================
====================================================================================
====================================================================================
====================================================================================








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









- CHECK (& RECORD HERE) IPHONE NOTES ON ESCM !!!

- CONSIDER HAVING `eval` & `bytecode-eval` & `load` & `load-from` & `load-serialized` & `serialize` (ETC. AS NEEDED) SUPPORT SANDBOXING FUNCTIONALITY TO EVAL CODE IN A SEPERATE GLOBAL ENVIRONMENT
  => OPTIONAL ARG TO THE EXISTING FCNS DENOTING WHETHER TO SANDBOX THE EVALUATION (#f BY DEFAULT)

- have anonymous lambdas print some unique id for slightly easier debugging

- CONSIDER OPTIONAL SUPPORT FOR ANSI COLORS!
  * have a flag to disable these
  * NOTE: ERRORS/WARNINGS MIGHT BE ABLE TO GET AWAY WITH USING EMOJIS INSTEAD
  * `-n`, `--nansi`

- CHECK WHETHER SHOULD ALWAYS SET LOCAL TO ENGLISH OR NOT WHEN RUNNING ESCM (DOING SO PROGRAMMATICALLY)









- ADD IN MORE DIVERSE/COMPLEX SAMPLE FILES TO THE `doc/example` DIRECTORY







- NETWORKING PRIMITIVES

- GUI PRIMITIVES

  * GET TO THE POINT WHERE CAN GENERATE A FRACTAL !!!
  * CONSIDER A -i, --interactive FLAG TO LAUNCH THE REPL IN A GUI WINDOW THAT DETECTS KEYSTROKES, STORES HISTORY VIA ^v, ETC. 

- LOGIC PROGRAMMING FUNCTIONS ???

- LOOK BACK AT HEIST-SCHEME FOR ANY OTHER FEATURES TO IMPLEMENT

- CHECK IPHONE `ESCM TODOS` NOTE