<!-- TODO.md -->

## MORE



- BETTER ERRORS BY HAVING THE MESSAGE PRINT, THEN PRINT THE EXCEPTION/ERROR NAME IN PARENS UNDER THE MESSAGE?
  ```
  ESCM ERROR: <message>
              (<java exception name>)
  ```




- CHANGE READER TO GIVE BETTER ERRORS
  * INSTEAD OF DUMPING OUT THE ENTIRE STRING, DO THE `JSON` PARSER THING OF HAVING AN ARROW THAT POINTS TO THE RIGHT POSITION OF THE ERROR
    AFTER PRINTING OUT A SUBSTRING OF THE ERRORFUL CODE
      => CONSIDER EVEN PRINTING OUT THE LINE/COLUMN NUMBER THAT THE ERROR OCCURED ON !!!

      => ALSO IMPROVE THE ERRORS THROWN BY THE PORT-READING METHOD IN `InputPort`




==================================================================================================================
==================================================================================================================
==================================================================================================================
==================================================================================================================
==================================================================================================================
==================================================================================================================


```
loadWithName() // SHOULD ACCEPT A SYMBOL
  => for procedures, binds the InformationSource object of the symbol

```



```
loadWithSource(SourceInformation source)




SHOW SOURCE INFORMATION OF PROCEDURES IN CALLSTACK
  * CURRENT APPROACH OF MODIFIYING NAME-BINDING (`loadWithName`) TO ALSO "CONS" SOURCE INFO ONTO PROCEDURES THAT MAY BE REGISTERED IN THE CALLSTACK ALONG WITH A NAME





SHOW "LAST CALLED" PROCEDURE LOCATION
  - Designed to catch exact location of `+` in: `(+ "hi")`
  - (push +) ; can have `loadWithState` of `Symbol` also `cons` source information onto env result iff getting a procedure
    - then this is pushed to thread-local "last called" stack
      * must differentiate between name-binding (defining) source and call-binding (invoking) source internally
        - if no invoking source, do nothing
        - in continuation, pop it from from the thread-local stack
```





==================================================================================================================
==================================================================================================================
==================================================================================================================
==================================================================================================================
==================================================================================================================
==================================================================================================================





- `#line` macro variable: special "macro-specific" variable.
  When the compiler expands a macro, it parses the expanded value for all `#line` instances.
  Any instances found are replaced with the line number in the `source` object of the macro name.
    => also have `#file` `#column`

    => have compiling these symbols spereately as an atomic instead compile them to become their
       respective values by introspecting on their `source` object instead compiling that specific
       value!

    => MENTION THESE IN `help` AND `README.md` AS A SCM EXTENSION







- have anonymous lambdas print some unique id for slightly easier debugging







- ADD AN `(absolute-path? <string>)` PRIMITIVE
    * EQUIVALENT TO: `(equal? <string> (absolute-path <string>))`

        => ISSUE: MAY NEED TO GET RID OF `..` INSTANCES IN <(absolute-path <string>)> (SEE HOW CAN DO THIS IN JAVA & IF ITS WORTH IT)







- ISSUE:
  * Suppose `t.scm` loads `s.scm`. Then suppose we execute `escm ../t.scm`. This triggers an error that `s.scm` is not found!
    => solution: have a `CURRENT_LOADING_DIRECTORY` constant + global determined by the path preceding the given interpretable file name
                 (or just CWD if ___ONLY___ in the REPL). Then, if ever reading/loading/slurping (etc.) any file with a non-absolute path,
                 make it relative to the `CURRENT_LOADING_DIRECTORY` variable

                  => VERIFY THIS WORKS !!!






- CONSIDER HAVING INPUT PORTS CHANGE THEIR NAME TO BE AN ABSOLUTE PATH RATHER THAN THE RELATIVE PATH !!!
  * ALSO HAVE `file-read` DO THIS TO ITS FILENAME PRIOR TO CREATING THE `source` OBJECT PASSED TO THE READER !!!






- CHECK (& RECORD HERE) IPHONE NOTES ON ESCM !!!













- SUPPORT OTHER ESCAPE JAVA CHARS IN STRINGS AS NEEDED
  * ESCPECIALLY UNICODE (`\u`) SUPPORT !!!
    => SEE `StringParser.java`
       * UPDATE IN `json` AS WELL
  
  * ADD DETAILS TO `README` & `help` REGARDING WHAT ESCAPE CHARACTERS ARE / AREN'T SUPPORTED IN STRINGS !!!










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





  



- ADD JSON SUPPORT PRIMITIVES
  => `json->scm`, `scm->json`, `object->json`, `json-datum?`, `json-string?`
  => USE HASHMAPS AND VECTORS AS APPROPRIATE

- ADD CSV SUPPORT PRIMITIVES









- GENERATE DETAILED DOCUMENTION FOR `README.md` AUTOMATICALLY VIA THE `help` ENTRIES !!!
  * should also generate internal links correctly !!!







- RUNTIME TYPING ___!!! THIS WOULD WARRANT A VERSION UPGRADE TO `8.0` !!!___
  
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







- LOGIC PROGRAMMING FUNCTIONS ???

- NETWORKING PRIMITIVES

- GUI PRIMITIVES
  
  * CONSIDER A -i, --interactive FLAG TO LAUNCH THE REPL IN A GUI WINDOW THAT DETECTS KEYSTROKES, STORES HISTORY VIA ^v, ETC. 

- LOOK BACK AT HEIST-SCHEME FOR ANY OTHER FEATURES TO IMPLEMENT

- CHECK IPHONE `ESCM TODOS` NOTE