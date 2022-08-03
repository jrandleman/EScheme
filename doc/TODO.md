<!-- TODO.md -->

## MORE






___HASHMAP PRIMITIVES___
```clj
(hashmap <key> <value> ...)

(hashmap-keys <hashmap>)
(hashmap-values <hashmap>)

(hashmap-key? <hashmap> <obj>)

(hashmap-ref <hashmap> <key>) ; Equivalent to: (<hashmap> <key>)
(hashmap-set! <hashmap> <key> <value>)
(hashmap-delete! <hashmap> <key>)

(hashmap-length <hashmap>)
(hashmap-empty? <hashmap>)

(hashmap-merge <hashmap> ...)
(hashmap-merge! <hashmap> ...)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(hashmap->list <hashmap>)
(list->hashmap <list>)

(hashmap->vector <hashmap>)
(vector->hashmap <vector>)
```






- HASHMAPS `(hashmap )`
  * CONSIDER `{}` SYNTAX (ACCOUNT FOR IN READER & `quasiquote`)
  * add to README as a Scheme extension (under the `keyword` section)

  * add a `help` type entry about hashmaps (INLCUDE INFO ABOUT IT BEING A CALLABLE)

  1. DO TYPE & PRIMITIVES
     -> ___TYPE SHOULD BE AN UNARY CALLABLE ACCEPTING A KEY AS ITS ARGUMENT !!!___
  2. UPDATE `primitives.md` and `help`
  3. UPDATE THE READER & INPUT-PORT !!!!
     * update `quasiquote` as well !!!
     * then update `help` again as needed in terms of the new syntax
  4. UPDATE `README.md`
  5. MAKE SURE TO AVOID DATA RACES BY USING `sync` AS NEEDED
      ```java
      synchronized(vector) {
        if(vector.size() > 0) return vector.get(0); // wrap in <sync> to avoid race condition of .size() and .get() !!!
      }
      ```


`hashmap->alist`
`alist->hashmap`

`hashmap->list`
`list->hashmap`

`hashmap->vector`
`vector->hashmap`




<!-- 

  * NOTE: BUILDERS ___MUST___ BE THREAD-SAFE !!!

  * MENTION THE "BUILDER" CONCEPT UNDER SECTION 5 OF THE README'S "SCHEME DEVIATIONS" SECTION. EXTEND "IMMUTABLE CORE" TYPES TO INCLUDE HASHMAPS AND VECTORS
    - explain `[]` and `{}` reader syntax creates an immutable vector and hashmap. Mutated via `builder` equivalents.
    - mention it as a mutable alternative to the immutable other types.
    - note that `eq?` is used for `builder-key?` & `builder-value?`

```java
synchronized(vector) {
  if(vector.size() > 0) return vector.get(0); // wrap in <sync> to avoid race condition of .size() and .get() !!!
}
```


```
(list-builder <seed-vector>)    ; <key> is an index! [[[ should use a vector builder under the hood, with the only things changes being the ctor & '->value procedures ]]]
(vector-builder <seed-vector>)  ; <key> is an index!
(string-builder <seed-string>)  ; <key> is an index! <val> is a string (<key> will refer to a single-char substring)
(hashmap-builder <seed-string>) ; <key> is an obj!

(builder-append! <builder> <appended-builder> ...)
(builder-insert! <builder> <key> <elt>)
(builder-delete! <builder> <key>) ; returns success status

(builder-key <builder> <elt> <optional-default-value>) ; returns index if <elt> in <builder>, else reutrns <optional-default-value> (or triggers error if none given)
(builder-value <builder> <key>) ; value of the key

(builder-key? <builder> <key>)
(builder-value? <builder> <value>)

(builder-length <builder>)

(builder->value <builder>) ; convert internal builder to a value (actual list/vector/string/hashmap)

(builder? <obj>)
(list-builder? <obj>)
(vector-builder? <obj>)
(string-builder? <obj>)
(hashmap-builder? <obj>)
``` -->










- AFTER VECTORS AND HASHMAPS: UPGRADE VERSION TO ___7.0___







- STRING FORMATTING PROCEDURES `displayf`, `writef`, `stringf`, `pprintf` (`pretty-printf`), `(stringf )`
  => ACCOUNT FOR THIS IN THE `error` PROCEDURE

- ADD JSON SUPPORT PRIMITIVES
  => `json->scm`, `scm->json`, `object->json`, `json-datum?`, `json-string?`
  => USE HASHMAPS AND VECTORS AS APPROPRIATE

- ADD CSV SUPPORT PRIMITIVES









- GENERATE DETAILED DOCUMENTION FOR `README.md` AUTOMATICALLY VIA THE `help` ENTRIES !!!
  * should also generate internal links correctly !!!







- RUNTIME TYPING ___!!! THIS WOULD WARRANT A VERSION UPGRADE TO `8.0` !!!___
  
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

            :builder ; ONCE IMPLEMENTED
            :list-builder ; ONCE IMPLEMENTED
            :vector-builder ; ONCE IMPLEMENTED
            :string-builder ; ONCE IMPLEMENTED
            :hashmap-builder ; ONCE IMPLEMENTED

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