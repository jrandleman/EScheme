<!-- TODO.md -->

## MORE


- ADD IN `(file-remove-extension <file-path-string> <extension-string>)` (ALSO REMOVES THE ".")







- MAKE SURE ALL "Stream" & "File" & "Reader" & "Writer" INSTANCES ARE PROPERLY CLOSED IN JAVA
  * MAKE SURE MOVE AS MANY "CLOSE" OPERATIONS AS POSSIBLE PRIOR THROWING ERRORS FOR FILES!







- CONSIDER HAVING `eval` & `bytecode-eval` & `load` & `load-from` & `load-serialized` & `serialize` (ETC. AS NEEDED) SUPPORT SANDBOXING FUNCTIONALITY TO EVAL CODE IN A SEPERATE GLOBAL ENVIRONMENT
  => OPTIONAL ARG TO THE EXISTING FCNS DENOTING WHETHER TO SANDBOX THE EVALUATION (#f BY DEFAULT)









- ADD IN THE `Character` TYPE PRIOR TO IMMUTABLE GENERIC ALGOS (REQUIRED FOR ITERATION IN STRING CONTAINERS)
  => `#\` PREFIX AS PER REGULAR SCHEME (REPRESENTS AN INT UNDER THE HOOD)

  ```clj
  (list->string '(#\h #\e #\l #\l #\o #\! #\newline)) ; => "hello!\n"
  HEX CHARACTER CODES: #\xa ; equivalent to #\newline
  ```

  => LIKE HASHMAPS & VECTORS, _HAVE STRINGS BE UNARY CALLABLES_ ACCEPTING AN INDEX AS ITS ARG TO GET A CHAR @ THAT POSITION







- CONCEPT OF GENERIC ALGO PRIMTIIVES THAT OPERATE ON `AssociativeCollection` (`AC` alias) & `OrderedCollection` (`OC` alias) INTERFACES

```clj
(define-interface AssociativeCollection
  (:static (valueOf obj)
    (cond ((AssociativeCollection? obj)
            obj)
          ((string? obj)
            (escm-string->OrderedCollection obj))
          ((list? obj)
            (escm-list->OrderedCollection obj))
          ((vector? obj)
            (escm-vector->OrderedCollection obj))
          ((hashmap? obj)
            (escm-hashmap->AssociativeCollection obj))
          (else
            (errorf "Invalid non-associative collection given: %wa of type %a" obj (typeof obj)))))
  ref ; (lambda (keyValue) ...)
  ; ETC.
  )


; Ordered collections are "associative" w/ idxs as their keys
(define-interface OrderedCollection (:extends AssociativeCollection)
  (:static (valueOf obj)
    (cond ((OrderedCollection? obj)
            obj)
          ((string? obj)
            (escm-string->OrderedCollection obj))
          ((list? obj)
            (escm-list->OrderedCollection obj))
          ((vector? obj)
            (escm-vector->OrderedCollection obj))
          (else
            (errorf "Invalid non-ordered collection given: %wa of type %a" obj (typeof obj)))))
  slice ; (fn ((start) ...) ((start end) ...) ((start end skip-callable) ...))
  ; ... ETC.
  )


(define AC AssociativeCollection)
(define AC? AssociativeCollection?)

(define OC OrderedCollection)
(define OC? OrderedCollection?)
```


```clj

;;;;;;;;;;;;;;;;
;;;; IN ESCHEME:

; Predicates
(AssociativeCollection? <object>) ; for objects (aliased by <AC?>)
(OrderedCollection? <object>) ; aliased by <OC?>

(associative-collection? <obj>) ; for objs (aliased by <ac?>)
(ordered-collection? <obj>) ; aliased by <oc?>


; ACs
(empty? <ac>)
(length <ac>)
(length+ <list>)

(fold <callable> <seed> <ac> ...)

(map <callable> <ac> ...)
(for-each <callable> <ac> ...)
(filter <callable> <ac>)

(count <predicate?> <ac>)
(remove <predicate?> <ac>)

(ref <ac> <key>)

(append <ac> ...) ; all args must be of the same type

(delete <ac> <key>) ; return copy of <ac> w/o <key>

(any <predicate?> <ac> ...)
(every <predicate?> <ac> ...)

(conj <key> <val> <ac>) ; return version of <ac> with <val> associated to <key>

(associative-collection->list <ac>) ; (aliased by ac->list)
(associative-collection->string <ac>) ; (aliased by ac->string)
(associative-collection->vector <ac>) ; (aliased by ac->vector)
(associative-collection->hashmap <ac>) ; (aliased by ac->hashmap)

(union <elt=?> <ac> ...)
(intersection <elt=?> <ac> ...)
(difference <elt=?> <ac> ...)
(symmetric-difference <elt=?> <ac> ...)

(merge <ac> ...)

(delete-duplicates <ac>)


; OCs
(conj <val> <oc>) ; add <val> as efficiently as possible to <oc>. Makes no guarentee about position.

(fold-right <callable> <seed> <oc> ...) ; only ordered collections have the concept of "right-to-left"

(remove-first <predicate?> <oc>)
(remove-last <predicate?> <oc>)

(head <oc>)
(tail <oc>)
(init <oc>)
(last <oc>)

(slice <oc> <start-key> <optional-length-or-end-predicate>)

(reverse <oc>)

(skip <predicate?> <oc>)
(skip-right <predicate?> <oc>)

(key <predicate?> <oc>) ; returns key of left-most value satisfying <predicate?>
(key-right <predicate?> <oc>) ; returns key of right-most value satisfying <predicate?>

(drop <oc> <length>)
(drop-right <oc> <length>)
(drop-while <predicate?> <oc>)
(drop-right-while <predicate?> <oc>)

(take <oc> <length>)
(take-right <oc> <length>)
(take-while <predicate?> <oc>)
(take-right-while <predicate?> <oc>)

(sort <binary-predicate?> <oc>)
(sorted? <binary-predicate?> <oc>)

(delete-neighbor-duplicates <oc>)
```


```java
//////////////////////////////////////////////////////////////////////////////
// NOTE: SHOULD HAVE CERTAIN METHODS FOR INSTANCES TO SATISFY, __BUT__ CLASS 
//       SHOULD HAVE STATIC METHODS TO PERFORM PRM OPERATIONS DERIVED BY THOSE
//       IMPLEMENTED METHODS ON INSTANCES
//         => example: "forEach" is a method that can be used to implement the
//                     static `AssociativeCollection.Map(Callable c, AssociativeCollection ... acs)` method
//////////////////////////////////////////////////////////////////////////////



//////////////////////////////////////////////////////////////////////////////
// REQUIRED METHODS TO IMPLEMENT IN JAVA/ESCM OBJECTS SATISFYING AC:

int length();

Trampoline.Bounce forEach(Callable c, Trampoline.Continuation continuation) throws Exception; // -> Void

ValueType ref(KeyType key) throws Exception;

AssociativeCollection append(AssociativeCollection ac) throws Exception;

AssociativeCollection delete(KeyType key) throws Exception;

AssociativeCollection conj(KeyType key, ValueType val) throws Exception;

Datum toList() throws Exception;
Datum toVector() throws Exception;
Datum toString() throws Exception;
Datum toHashmap() throws Exception;
```







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

        => SUPPORT A CHARACTERS: `#\unicode` `#\Unicode` (for \u & \U)


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