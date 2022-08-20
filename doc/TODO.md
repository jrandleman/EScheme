<!-- TODO.md -->

## MORE

- `(serialized? <file-path-string>)`

    * IF THIS IS IMPLEMENTED CORRECTLY: 
      1. DON'T NEED THE NEW `-s`, `-ls` COMMAND LINE FLAGS BELOW
         - CAN INSTEAD DETERMINE EVAL STRATEGY AUTOMATICALLY WHEN LOADING/INTERPRETING A GIVEN FILENAME
      2. DON'T NEED `load-serialized` & `load-once-serialized` !!!
         - RM FROM `help` & `primitives`
         - CAN INSTEAD DETERMINE EVAL STRATEGY AUTOMATICALLY VIA `load` & `load-from` & `load-once` & `load-once-from`
           * MENTION IN THEIR HELP ENTRY THAT THEY WILL AUTOMATICALLY WORK W/ SERIALIZED FILES TOO!




<!-- 
- COMMAND LINE SUPPORT FOR LOADING/INTERPRETTING SERIALIZED FILES!
  `-s`, `--serialized <script> <arg1> ...` ; interpret & terminate the serialized `<script>` file
  `-ls`, `--load-serialized <script> <arg1> ...` ; load the serialized `<script>` file into the REPL

    ADD THESE TO THE README, `--help` RESULT, & `command-line` SECTION IN `help`



    ```
    Command-line flags may be used to modify EScheme's behavior:
      1. -v,  --version                         | Print EScheme version information
      2. -h,  --help                            | Print this information
      3. -q,  --quiet                           | Launch the REPL without ASCII art
      5. -l,  --load <script> <arg1> ...        | Load <script> with <arg> ... as *argv* into the REPL
      6. -ls, --load-serial <script> <arg1> ... | Load serialized <script> into the REPL
      4. -s,  --serial <script> <arg1> ...      | Interpret serialized <script>
      7. <script> <arg1> ...                    | Interpret <script>
      8. [no arguments]                         | Launch the REPL

    ```
 -->








- ADD A `serialization` ENTRY TO THE `Topics` HELP SECTION !!!!






- CONSIDER HAVING `load` & `load-from` & `load-serialized` ALSO POPULATE THE `load-once` BUFFER TOO !!!






- ADD IN `(file-remove-extension <file-path-string> <extension-string>)`







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