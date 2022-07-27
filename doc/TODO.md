<!-- TODO.md -->

## MORE







- PROPER PORTS





  * SEPERATE `SystemPrimitives` TO BE `SystemPrimitives` `FilePrimitives` AND ADD `PortPrimitives`




  * UPDATE I/O PRIMITIVES TO WORK WITH SUCH TOO !!!

  * ADD TO `primitives.md` AND `help` THE TYPE INFORMATION IN `Types` ABOUT PORTS & THE ACTION INFO IN `Procedures`




  


  * ADD `WITH-ERROR-TO-FILE` `CALL-WITH-ERROR-FILE` `CURRENT-ERROR-PORT` `(input-port? <obj>)` `(output-port? <obj>)` `(error-port? <obj>)`


```
help> PORTS

OPEN-PORT?              CLOSED-PORT?             CURRENT-INPUT-PORT      CURRENT-OUTPUT-PORT
CALL-WITH-INPUT-FILE    CALL-WITH-OUTPUT-FILE    WITH-INPUT-FROM-FILE    WITH-OUTPUT-TO-FILE
OPEN-INPUT-FILE         OPEN-OUTPUT-FILE         OPEN-OUTPUT-FILE+       OPEN-OUTPUT-FILE!  
REWIND-PORT!            PORT-SEEK!               PORT-SEEK-FRONT!        CLOSE-PORT         

help> FILES

GETCWD                 DIRNAME           MKDIR                  CHDIR              
FILE?                  DIRECTORY?        PATH?                  DIRECTORY-ENTRIES  
DIRECTORY-ENTRIES*     DELETE-PATH!      RENAME-PATH!           COPY-PATH          
FILE-SIZE              FILE-EXTENSION    HAS-FILE-EXTENSION?    SET-FILE-EXTENSION!
SWAP-FILE-EXTENSION

help> IO // THESE SHOULD ACCEPT OPTIONAL PORTS AS THEIR FIRST ARGUMENT

PRETTY-PRINT    WRITE    DISPLAY    NEWLINE
WRITE-CHAR 

READ         READ-STRING    READ-LINE     READ-CHAR 
PEEK-CHAR    CHAR-READY?    SLURP-PORT    SLURP-FILE
READ-PORT    READ-FILE
```



  * SUPPORT A `dosync`-LIKE MACRO FOR FILES TO CLEAN UP VIA `do-wind`: 
    `(let-ports ((<port-name> <port-expr>) ...) <body> ...)`
    =>
    ```scheme
    (let ((<port-name> <port-expr>) ...)
      (dynamic-wind 
        (lambda () #void)
        (lambda () <body> ...)
        (lambda () (close-port! <port-name>) ...)))
    ```










- PRIMITIVE TO GET CURRENT DATE/TIME WITH OFFSETS
  * ADD TO `primitives.md` AND `help`

- VECTORS `(vector )` -- MUST USE ONE OF JAVA'S THREAD-SAFE DATA STRUCTS UNDER THE HOOD
- HASH MAPS `(hashmap )` -- MUST USE ONE OF JAVA'S THREAD-SAFE DATA STRUCTS UNDER THE HOOD

- ADD JSON SUPPORT PRIMITIVES
  => `json->scm`, `scm->json`, `object->json`, `json-datum?`, `json-string?`
  => USE HASHMAPS AND VECTORS AS APPROPRIATE

- ADD CSV SUPPORT PRIMITIVES

- STRING FORMATTING PROCEDURES `displayf`, `writef`, `stringf`, `pprintf` (`pretty-printf`)

- FRACTIONS, `BigInteger`, `BigDecimal`, COMPLEX NUMERICS
  * INCLUDE `#e` `#i` `#b` `#o` `#x` `#NNr` SUPPORT
  * DETERMINE WHETHER REALLY WANT BIGDECIMAL OR NOT !!!
  * UPDATE THE `number` `help` ENTRY !!!

- MORE STREAM PRIMITIVES




- RUNTIME TYPING
  
  * NOTE: THE BELOW SHOULD ALSO BE AVAILABLE FOR INTERFACE DEFINITIONS !!!

  BASICS: `(def :string (f :string s :any a))` use keywords to denote basic type checks
                ^          ^         ^
            return value  string     any-type!

          ***NOTE:*** SUPPORT FALSE-ABLE TYPES (SIMILAR TO NULLABILITY) VIA: `:string?`
          
          ARBITRARY PREDICATE TYPING: `(def (g :type even? arg-name))` `:type <predicate?-fcn>` => check validity of arg based on predicate. ALSO SUPPORT FALSABLE `:type?`

          COULD SUPPORT MULTIPLETYPES VIA `|`: `(def (f :string|number a))`, ALSO SUPPORT WITH TYPE: `:number|type|string|type <predicate1> <predicate2> <arg-name>`
                                                                                                              ^           ^
                                                                                                              # OF PREDICATES PARSED CORRELATES TO THE NUMBER OF TIMES
                                                                                                              `type` IS IN THE TYPE SEQUENCE
          COULD THEN HAVE:
            ```scheme
            (defn fun
              ((:string s) s) ; !!! can dispatch on type !!!
              ((:number n) n))
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

            :vector ; ONCE IMPLEMENTED
            :hashmap ; ONCE IMPLEMENTED

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

            :port ; ONCE IMPLEMENTED
            :input-port ; ONCE IMPLEMENTED
            :output-port ; ONCE IMPLEMENTED
            ```




- NETWORKING PRIMITIVES

- GUI PRIMITIVES
  
  * CONSIDER A -i, --interactive FLAG TO LAUNCH THE REPL IN A GUI WINDOW THAT DETECTS KEYSTROKES, STORES HISTORY VIA ^v, ETC. 

- LOOK BACK AT HEIST-SCHEME FOR ANY OTHER FEATURES TO IMPLEMENT

- CHECK IPHONE `ESCM TODOS` NOTE