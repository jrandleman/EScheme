<!-- TODO.md -->

## MORE



- look up PATH variable then write escm to that? Have "installer" install the code in the path and rename Main to Escm so running "java Escm" launches it ???
     => TEST THIS ON MAC AND RPI4

- PUT ESCM ON PRIVATE REPO ON GITHUB !







- IMPLEMENT PRETTY PRINTING !!!

- CONSIDER A `defined?` INSTRUCTION / MACRO THAT SUPPORTS INTROSPECTION

- CHANGE RESUME TO SAY HAVE BEEN USING JAVA FOR 1 YEAR

- ADD A `-q`  `--quiet` (quiet) CMD-LINE FLAG TO LAUNCH THE REPL W/O THE ASCII ART
- ADD A `-h`  `--help` (help) CMD-LINE FLAG TO SHOW USE OF THE CMD-LINE
- ADD A `--version` (verion information) CMD-LINE FLAG TO SHOW USE OF THE CMD-LINE
- ADD `--load` AS AN ALT TO `-l`
  
  * MENTION ALL OF THE CMD-LINE FORMS IN THE README !!!
  * ADD A SECTION ABOUT THIS IN `help`



- SOLVE THE "running escm via `system` problem"
  `*run-escm-cmd*`

- SUPPORT OFFERING POSSIBLY INTENDED VARIABLE NAMES UPON THE ENV NOT FINDING A NEEDED VARIABLE (LIKE W/ THE `help` MENU)
  * USE `escm.util.GetClosestMatches.run()`

- MENTION IN README THAT BLANK LINES IN THE ESCM CALL STACK DENOTE CALLS TO ANONYMOUS PROCEDURES



- ADD `=>` SUPPORT FOR `cond` AND `case`
  => ADD THIS TO THEIR `help` ENTRIES



- ADD THE `do` MACRO (MENTION IN `help`) !!!
- ADD THE `letrec*` MACRO (ADD TO `help` TOO)



- PROPER PORTS
  * UPDATE I/O PRIMITIVES TO WORK WITH SUCH TOO !!!
  * HAVE DIRECTORY SUPPORT !!!
  * CERTAIN OTHER SYSTEM COMMANDS: `(getcwd)`
  * SEPERATE `SystemPrimitives` TO BE `SystemPrimitives` `FilePrimitives` AND ADD `PortPrimitives`
  * SUPPORT A `dosync`-LIKE MACRO FOR FILES TO CLEAN UP VIA `do-wind`: 
    `(with-ports ((<port-name> <port-expr>) ...) <body> ...)`

- VECTORS `(vector )` -- MUST USE ONE OF JAVA'S THREAD-SAFE DATA STRUCTS UNDER THE HOOD
- HASH MAPS `(hashmap )` -- MUST USE ONE OF JAVA'S THREAD-SAFE DATA STRUCTS UNDER THE HOOD

- ADD JSON SUPPORT PRIMITIVES
  => `json->scm`, `scm->json`, `object->json`, `json-datum?`, `json-string?`
  => USE HASHMAPS AND VECTORS AS APPROPRIATE

- ADD CSV SUPPORT PRIMITIVES

- STRING FORMATTING PROCEDURES `displayf`, `writef`, `stringf`

- FRACTIONS, `BigInteger`, `BigDecimal`, COMPLEX NUMERICS
  * INCLUDE `#e` `#i` `#b` `#o` `#x` `#NNr` SUPPORT
  * DETERMINE WHETHER REALLY WANT BIGDECIMAL OR NOT !!!
  * UPDATE THE `number` `help` ENTRY !!!

- MORE STREAM PRIMITIVES




- RUNTIME TYPING
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

            :metaobject
            :object
            :class
            :interface
            ```




- NETWORKING PRIMITIVES

- GUI PRIMITIVES

- LOOK BACK AT HEIST-SCHEME FOR ANY OTHER FEATURES TO IMPLEMENT

- CHECK IPHONE `ESCM TODOS` NOTE