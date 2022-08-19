<!-- TODO.md -->

## MORE





`SEE T.java ON DESKTOP & REMEMBER TO FIX BOOLEAN TO NOTE CHECK COMPARISON TO POINTER DIRECTLY, RATHER CHECK RELATIVE TO TYPE (VERIFY NOT DOING THIS ANYWHERE ELSE BEYOND BOOLEAN)`


<!--

- IMPROVE BOOT TIME VIA SERIALIZATION ???

    => ___THIS MAY NOT BE WORTH IT, BUT WORTH KEEPING HERE IN CASE___

    => PERHAPS SOME WAY TO PRE-COMPILE ENTIRE STDLIB INTO A SINGLE GIANT `bytecode` EXPRESSION (`SGBE`):
       => THAT `SGBE` IS THEN COMPILED DOWN TO AN "INSTRUCTION SET" (`ArrayList<Instruction>`) 
          => THAT `ArrayList<Instruction>` IS THEN SERIALIZED AND SAVED IN A `compiled-stdlib` FILE IN `/src`
             => THEN, UPON INTERPRETER BOOT-TIME, `compiled-stdlib` IS READ AND EXECUTED DIRECTLY

             * MUST TEST THE PERF DIFFERENCE OF THIS VS OUR CURRENT APPROACH

               => IF WE END UP KEEPING THIS SOLUTION:

                  * WE MUST MENTION IN `stdlib.scm` THAT A FULL, FRESH `EScheme` REINSTALL IS REQUIRED FOR 
                    THE `stdlib.scm` FILE'S CONTENTS TO EFFECT THE ESCM RUNTIME

                  * CONSIDER EXPOSING THIS ABILITY SOMEWHAT TO TO THE ESCM RUNTIME? 
                    - `(write-java-object <obj>)`
                    - `(read-java-object <string-or-input-port>)`
                    - `(run-and-serialize <file-name>)` ; COULD EVEN EXECUTE THIS IN A SANDBOXED ENVIRONMENT ???
 -->





- PUT A NOTE @ TOP OF `stdlib.scm` EXPLAINING THE `stdlib.ser` SITUATION, AND THAT A FULL REINSTALL 
  __OR__ CALLING `escm --serialize-stdlib` IS REQUIRED FOR CHANGES TO `stdlib.scm` TO TAKE EFFECT ON THE ESCHEME RUNTIME !!!




- INSTALLER: After compiling EScheme, 
  * DELETE `bin/stdlib.ser` IF PRESENT
  * EXECUTE `escm --serialize-stdlib` TO HAVE AN INTERNAL FCN W/IN ESCHEME GET TRIGGERED TO WRITE THE PRE-COMPILED VERSION OF STDLIB TO `bin/stdlib.ser`
  * EXTEND INSTALLER TO HAVE FLAG: `-i`, `--interpreted-stdlib`
    => DISABLES PRE-SERIALIZATION OF `src/stdlib.scm` AND HAS ESCHEME JUST INTERPRET IT AS A REGULAR ESCM FILE !


USE THE NEWLY ADDED `serialize` AND `load-serialized` PRIMITIVE LOGICS INTERNALLY !!!!!!!
==========================================================================================































- MAKE SURE ALL "Stream" INSTANCES ARE PROPERLY CLOSED IN JAVA
  * MAKE SURE MOVE AS MANY "CLOSE" OPERATIONS AS POSSIBLE PRIOR THROWING ERRORS FOR FILES!









- CONSIDER HAVING `eval` & `bytecode-eval` & `load` & `load-from` (ETC. AS NEEDED) SUPPORT SANDBOXING FUNCTIONALITY TO EVAL CODE IN A SEPERATE GLOBAL ENVIRONMENT
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