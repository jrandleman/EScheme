<!-- TODO.md -->

## MORE



- VECTORS `(vector )` -- MUST USE ONE OF JAVA'S THREAD-SAFE DATA STRUCTS UNDER THE HOOD

<!-- 
  => CONSIDER IMPACT OF HAVING BOTH IMMUTABLE `vector` WITH READER `[]` SUPPORT, AND A MUTABLE `mutable-vector` W/O READER SUPPORT  
     * `(vector->mutable-vector <vector>)` `(mutable-vector->vector <vector>)`

       * OR HAVE THE ONLY WAY TO GET A MUTABLE VECTOR BE `(vector->mutable <vector>)` `(mutable->vector <mutable-vector>)`

     => THEN COULD ALSO HAVE `hashmap`, `{}` AND `mutmap`

       * OR HAVE THE ONLY WAY TO GET A MUTABLE MAP BE `(hashmap->mutable <hashmap>)` `(mutable->hashmap <mutable-hashmap>)`
 -->

```java
synchronized(vector) {
  if(vector.size() > 0) return vector.get(0); // wrap in <sync> to avoid race condition of .size() and .get() !!!
}
```

  * CONSIDER `[]` SYNTAX (ACCOUNT FOR IN READER & `quasiquote`)



- ADD PRIMTIIVES & UPDATE `primitives.md` & `help`

- ALSO LOOK INTO `string` PRIMITIVES FOR INSPIRATION IN ADDITION TO THE BELOW !!!

- ALSO LOOK INTO HEIST'S PRIMITIVES FOR INSPIRATION IN ADDITION TO THE BELOW !!!

```scheme
vector   vector-append          vector-length   vector-reverse     
vector-map     vector-filter  vector-for-each        vector-fold     vector-fold-right  
vector-ref             vector-subvector             vector-sort     vector-sorted?     
vector?   vector-empty?

(vector-insert! <vector> <optional-idx> <obj>) ; dflt to push
(vector-remove! <vector> <optional-idx> <obj>) ; dflt to pop
(vector-has? <vector> <obj>)

(vector-grow! <vector> <number-elts-to-grow-by>)
```








- HASH MAPS `(hashmap )` -- MUST USE ONE OF JAVA'S THREAD-SAFE DATA STRUCTS UNDER THE HOOD

- ADD JSON SUPPORT PRIMITIVES
  => `json->scm`, `scm->json`, `object->json`, `json-datum?`, `json-string?`
  => USE HASHMAPS AND VECTORS AS APPROPRIATE

- ADD CSV SUPPORT PRIMITIVES

- STRING FORMATTING PROCEDURES `displayf`, `writef`, `stringf`, `pprintf` (`pretty-printf`), `(stringf )`
  => ACCOUNT FOR THIS IN THE `error` PROCEDURE

- A CHARACTER TYPE ???







- GENERATE DOCUMENTATION AUTOMATICALLY VIA THE `help` ENTRIES !!!







- RUNTIME TYPING ___!!! THIS WOULD WARRANT A VERSION UPGRADE TO `7.0` !!!___
  
  * NOTE: THE BELOW SHOULD ALSO BE AVAILABLE FOR INTERFACE DEFINITIONS !!!

  * CONSIDER: `(define-type <keyword> <unary-predicate-callable>)` `(type? <keyword>)` `(delete-type! <keyword>)` `(type-predicate <keyword>)`

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
            (defn :integer fun
              ((:string s) (length s)) ; !!! can dispatch on type !!!
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










- AFTER RUNTIME TYPING, CONSIDER RUNTIME ACCESS MODIFIERS FOR OBJECTS (NOTE THAT `:public` MUST BE ___FAST___ !!!)









- NETWORKING PRIMITIVES

- GUI PRIMITIVES
  
  * CONSIDER A -i, --interactive FLAG TO LAUNCH THE REPL IN A GUI WINDOW THAT DETECTS KEYSTROKES, STORES HISTORY VIA ^v, ETC. 

- LOOK BACK AT HEIST-SCHEME FOR ANY OTHER FEATURES TO IMPLEMENT

- CHECK IPHONE `ESCM TODOS` NOTE