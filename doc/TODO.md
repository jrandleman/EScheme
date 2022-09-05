<!-- TODO.md -->

## MORE


- ADD IN THE `Character` TYPE PRIOR TO IMMUTABLE GENERIC ALGOS (REQUIRED FOR ITERATION IN STRING CONTAINERS)
  => `#\` PREFIX AS PER REGULAR SCHEME (REPRESENTS AN INT UNDER THE HOOD)

[X] FIX READERS TO ACCOUNT FOR BASIC SINGLE CHARACTERS
[X] EXTEND READERS TO ALLOW FOR NAMED CHARACTERS (#\newline, #\tab, ETC.)
    ```
    #\space, #\tab, #\newline, #\page, #\return,
    #\backspace, #\nul, #\esc (27), #\delete (127)
    ```
[X] EXTEND READERS TO ALLOW FOR CODEPOINT CHARACTERS (#\uXXXX, #\UXXXXXXXX)
[X] UPDATE CHARACTER PRINTER TO PRINT OUT NAMED CHARS & CHAR HEX CODES AS NEEDED!
[X] FIX SITUATION WHERE `#\newline` WILL NEVER BE READ W/O NAME B/C OF HOW `isDelimiter` WORKS
[X] UPDATE `help` WITH A `Types > character` ENTRY CONTAINING INFO ON CODEPOINTS, DIFFERENT NAMED CHARS, ETC.
    * mention that "write" & "display" are different for chars!

[ ] ADD IN CHARACTER PRIMITIVE FUNCTIONS
    * ADJUST `help` & `primitives.md` AS NEEDED

[ ] UPLOAD TO GITHUB

[ ] UPDATE SOME OF THE STRING PRIMITIVES TO ACCOUNT FOR "CHAR" TYPE
    [ ] => MUST FIX `string-ref` !!!!
    [ ] => consider changing `string-length` to be based on the number of codepoints within the string, then have a sperate `string-char-length`?
    * ADJUST `help` & `primitives.md` AS NEEDED

[ ] UPLOAD TO GITHUB

[ ] LIKE HASHMAPS & VECTORS, _HAVE STRINGS BE UNARY CALLABLES_ ACCEPTING AN INDEX AS ITS ARG TO GET A CHAR @ THAT POSITION
    * MENTION THIS IN `help`!

[ ] UPLOAD TO GITHUB



# CHAR PROCS FROM HEIST
```
char-upcase   char-downcase


### TEST PRIMITIVES & UPDATE `primitives.md` & `help`


char=?    char<?    char>?    char<=?    char>=?
char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
```


# NEW ESCHEME CHAR PROCS
```
char-pair?  ; to determine if holding a 32bit unicode char surrogate pair rather than a true 16bit java character (32bit codepoints take up 2 char slots in a string)
char-java?  ; whether a 16bit char
char-ascii? ; whether a 8bit char
char-32?    ; alias for <char-pair?>
char-16?    ; alias for <char-java?>
char-8?     ; alias for <char-ascii?>


### TEST PRIMITIVES & UPDATE `primitives.md` & `help`


(char-count <char>) ; number of chars to represent codepoint (1 if <char-16?>, else 2)

(char-digit <char> <optional-radix>)        ; returns integer of <char> in <optional-radix> (defaults to 36) (Character.digit) => returns #f if an invalid <char>
(char-for-digit <integer> <optional-radix>) ; returns char of <integer> in <optional-radix> (defaults to 36) (Character.forDigit) => returns #f if an invalid <integer>

(char-name <char>) ; returns name of <char>, or #f if <char> is an unassigned codepoint (Character.getName)

(char-high <char>)             ; returns top 16 bits of 32 bit codepoint (Character.highSurrogate)
(char-low <char>)              ; returns bottom 16 bits of 32 bit codepoint (Character.lowSurrogate)
(char-codepoint <char> <char>) ; returns a char formed by combining the bottom bits of both chars as the top & bottom 16 bits of a new 32bit codepoint (Character.toCodePoint)

(char-defined? <char>) ; returns whether <char> is a defined unicode point (Character.isDefined)



### TEST PRIMITIVES & UPDATE `primitives.md` & `help`



```






















====================================================================================
====================================================================================
====================================================================================
====================================================================================

```
(union <elt=?> <ac> ...)
(intersection <elt=?> <ac> ...)
(difference <elt=?> <ac> ...)
(symmetric-difference <elt=?> <ac> ...)

(merge <predicate?> <ac> ...)

(delete-duplicates <ac>)
```



- RECONSIDER WHETHER SHOULD REALLY HAVE AN ESCM-OO OBJECT VERSION TO PASS TO GENERIC ALGOS (MAY BE MORE TROUBLE THAN ITS WORTH)
  => MAYBE HAVE IT, BUT WITH 2 INTERFACES THAT A JAVA CLASS CAN SELECT FROM IN ORDER TO IMPLEMENT A "`*Collection`" OBJECT
  => HAVE "ITERATOR" CONCEPT?

     ```java
      public interface AssociativeCollectionIterator {
        boolean finished(); // returns whether at end of the collection

        Datum key() throws Exception;   // throws if <finished() == true>
        Datum value() throws Exception; // throws if <finished() == true>

        AssociativeCollectionIterator next(); // may return <this> if <finished() == true>

        Datum fromList(Datum l); // convert list <l> into an <AssociativeCollection> Datum
        Datum toList();          // convert remaining values until <finished() == true> to a list Datum
      }



      public interface AssociativeCollection {
        //////////////////////////////////////////////////////////////////////
        // Instance Methods
        AssociativeCollectionIterator iterator();

        Datum head();
        AssociativeCollection tail();

        int length();

        Datum ref(Datum key) throws Exception;

        AssociativeCollection append(AssociativeCollection ac) throws Exception;

        AssociativeCollection delete(Datum key) throws Exception;

        AssociativeCollection conj(Datum key, Datum val) throws Exception;

        Datum toList();
        Datum toVector();
        Datum toString();
        Datum toHashmap();


        //////////////////////////////////////////////////////////////////////
        // Extract iterators for the given collections 
        private static AssociativeCollectionIterator[] getIterators(AssociativeCollection[] acs) {
          AssociativeCollectionIterator[] iters = new AssociativeCollectionIterator[acs.length];
          for(int i = 0; i < acs.length; ++i) iters[i] = acs[i].iterator();
          return iters;
        }


        //////////////////////////////////////////////////////////////////////
        // Folding Logic (returns Datum)
        private static Trampoline.Bounce foldIter(Callable c, Datum seed, AssociativeCollectionIterator[] iters, Trampoline.Continuation continuation) throws Exception {
          AssociativeCollectionIterator[] cdrs = new AssociativeCollectionIterator[iters.length];
          ArrayList<Datum> args = new ArrayList<Datum>(1+iters.length);
          args.add(seed);
          for(int i = 0; i < iters.length; ++i) {
            if(iters[i].finished()) return continuation.run(seed);
            args.add(iters[i].value());
            cdrs[i] = iters[i].next();
          }
          return c.callWith(args,(acc) -> () -> foldIter(c,acc,cdrs,continuation));
        }


        public static Trampoline.Bounce fold(Callable c, Datum seed, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception {
          if(acs.length == 0) return continuation.run(seed);
          AssociativeCollectionIterator[] iters = getIterators(acs);
          return foldIter(c,seed,iters,continuation);
        }


        //////////////////////////////////////////////////////////////////////
        // Mapping Logic (returns AssociativeCollection)
        private static Trampoline.Bounce mapIter(Callable c, AssociativeCollectionIterator[] iters, Trampoline.Continuation continuation) throws Exception {
          AssociativeCollectionIterator[] cdrs = new AssociativeCollectionIterator[iters.length];
          ArrayList<Datum> args = new ArrayList<Datum>(iters.length);
          for(int i = 0; i < iters.length; ++i) {
            if(iters[i].finished()) return continuation.run(escm.type.Nil.VALUE);
            args.add(iters[i].value());
            cdrs[i] = iters[i].next();
          }
          return c.callWith(args,(mappedValue) -> () -> mapIter(c,cdrs,(mappedList) -> () -> continuation.run(new escm.type.Pair(mappedValue,mappedList))));
        }


        public static Trampoline.Bounce map(Callable c, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception {
          if(acs.length == 0) throw new Exception("'(map <callable> <ac> ...) expects exactly at least 1 <ac>");
          AssociativeCollectionIterator[] iters = getIterators(acs);
          return mapIter(c,iters,(mappedList) -> () -> continuation.run(iters[0].fromList(mappedList)));
        }


        //////////////////////////////////////////////////////////////////////
        // For-each Logic (returns Void)
        private static Trampoline.Bounce forEachIter(Callable c, AssociativeCollectionIterator[] iters, Trampoline.Continuation continuation) throws Exception {
          AssociativeCollectionIterator[] cdrs = new AssociativeCollectionIterator[iters.length];
          ArrayList<Datum> args = new ArrayList<Datum>(iters.length);
          for(int i = 0; i < iters.length; ++i) {
            if(iters[i].finished()) return continuation.run(Void.VALUE);
            args.add(iters[i].value());
            cdrs[i] = iters[i].next();
          }
          return c.callWith(args,(ignore) -> () -> forEachIter(c,cdrs,continuation));
        }


        public static Trampoline.Bounce forEach(Callable c, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception {
          if(acs.length == 0) throw new Exception("'(for-each <callable> <ac> ...) expects exactly at least 1 <ac>");
          AssociativeCollectionIterator[] iters = getIterators(acs);
          return forEachIter(c,iters,continuation);
        }


        //////////////////////////////////////////////////////////////////////
        // Filtering Logic (inverse of <remove>) (returns AssociativeCollection)
        private static Trampoline.Bounce filterIter(Callable predicate, AssociativeCollectionIterator iter, Trampoline.Continuation continuation) throws Exception {
          if(iter.finished()) return continuation.run(escm.type.Nil.VALUE);
          AssociativeCollectionIterator cdr = iter.next();
          Datum value = iter.value();
          ArrayList<Datum> args = new ArrayList<Datum>(1);
          args.add(value);
          return predicate.callWith(args,(shouldKeep) -> () -> {
            if(shouldKeep.isTruthy() == false) return filterIter(predicate,cdr,continuation);
            return filterIter(predicate,cdr,(filteredList) -> () -> continuation.run(new escm.type.Pair(value,filteredList)));
          });
        }


        public static Trampoline.Bounce filter(Callable predicate, AssociativeCollection ac, Trampoline.Continuation continuation) throws Exception {
          AssociativeCollectionIterator iter = ac.iterator();
          return filterIter(predicate,iter,(filteredList) -> () -> continuation.run(iter.fromList(filteredList)));
        }


        //////////////////////////////////////////////////////////////////////
        // Counting Logic (returns Exact)
        private static Trampoline.Bounce countIter(int count, Callable c, AssociativeCollectionIterator iter, Trampoline.Continuation continuation) throws Exception {
          if(iter.finished()) return continuation.run(new escm.type.number.Exact(count));
          AssociativeCollectionIterator cdr = iter.next();
          ArrayList<Datum> args = new ArrayList<Datum>(1);
          args.add(iter.value());
          return c.callWith(args,(shouldCount) -> () -> {
            if(shouldCount.isTruthy() == false) return countIter(count,c,cdr,continuation);
            return countIter(count+1,c,cdr,continuation);
          });
        }


        public static Trampoline.Bounce count(Callable predicate, AssociativeCollection ac, Trampoline.Continuation continuation) throws Exception {
          return countIter(0,c,ac.iterator(),continuation);
        }


        //////////////////////////////////////////////////////////////////////
        // Removing Logic (inverse of <filter>) (returns AssociativeCollection)
        private static Trampoline.Bounce removeIter(Callable predicate, AssociativeCollectionIterator iter, Trampoline.Continuation continuation) throws Exception {
          if(iter.finished()) return continuation.run(escm.type.Nil.VALUE);
          AssociativeCollectionIterator cdr = iter.next();
          Datum value = iter.value();
          ArrayList<Datum> args = new ArrayList<Datum>(1);
          args.add(value);
          return predicate.callWith(args,(shouldRemove) -> () -> {
            if(shouldRemove.isTruthy() == true) return removeIter(predicate,cdr,continuation);
            return removeIter(predicate,cdr,(filteredList) -> () -> continuation.run(new escm.type.Pair(value,filteredList)));
          });
        }


        public static Trampoline.Bounce remove(Callable predicate, AssociativeCollection ac, Trampoline.Continuation continuation) throws Exception {
          AssociativeCollectionIterator iter = ac.iterator();
          return removeIter(predicate,iter,(filteredList) -> () -> continuation.run(iter.fromList(filteredList)));
        }


        //////////////////////////////////////////////////////////////////////
        // Key Logic (returns Datum [OR #f IF DNE])
        private static Trampoline.Bounce keyIter(Callable predicate, AssociativeCollectionIterator iter, Trampoline.Continuation continuation) throws Exception {
          if(iter.finished()) return continuation.run(escm.type.bool.Boolean.FALSE);
          ArrayList<Datum> args = new ArrayList<Datum>(1);
          args.add(iter.value());
          return predicate.callWith(args,(foundKey) -> () -> {
            if(foundKey.isTruthy() == true) return continuation.run(iter.key());
            return keyIter(predicate,iter.next(),continuation);
          });
        }


        public static Trampoline.Bounce key(Callable predicate, AssociativeCollection ac, Trampoline.Continuation continuation) throws Exception {
          return keyIter(predicate,ac.iterator(),continuation);
        }


        // //////////////////////////////////////////////////////////////////////
        // // Merging Logic (returns AssociativeCollection)
        // private static Trampoline.Bounce mergeIter(Callable predicate, AssociativeCollectionIterator iter1, AssociativeCollectionIterator iter2, Trampoline.Continuation continuation) throws Exception {
        //   if(iter1.finished()) return continuation.run(iter2.toList());
        //   if(iter2.finished()) return continuation.run(iter1.toList());
        //   AssociativeCollectionIterator cdr1 = iter1.next();
        //   AssociativeCollectionIterator cdr2 = iter2.next();
        //   Datum value1 = iter1.value();
        //   Datum value2 = iter2.value();
        //   ArrayList<Datum> args = new ArrayList<Datum>(2);
        //   args.add(value1);
        //   args.add(value2);
        //   return predicate.callWith(args,(keepValue1) -> () -> {
        //     if(keepValue1.isTruthy() == true) 
        //       return mergeIter(predicate,cdr1,iter2,(mergedList) -> () -> continuation.run(new escm.type.Pair(value1,mergedList)));
        //     return mergeIter(predicate,iter1,cdr2,(mergedList) -> () -> continuation.run(new escm.type.Pair(value2,mergedList)));
        //   });
        // }


        // public static Trampoline.Bounce merge(Callable predicate, AssociativeCollection ac1, AssociativeCollection ac2, Trampoline.Continuation continuation) throws Exception {
        //   AssociativeCollectionIterator iter1 = ac1.iterator();
        //   AssociativeCollectionIterator iter2 = ac2.iterator();
        //   return mergeIter(predicate,iter1,iter2,(mergedList) -> () -> continuation.run(iter1.fromList(mergedList)));
        // }
      ```







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
; Predicates
(AssociativeCollection? <object>) ; for objects (aliased by <AC?>)
(OrderedCollection? <object>) ; aliased by <OC?>

(associative-collection? <obj>) ; for objs (aliased by <ac?>)
(ordered-collection? <obj>) ; aliased by <oc?>


;;;;;;;;;;;;;;;;
; ACs
(head <oc>)
(tail <oc>)

(empty? <ac>)
(length <ac>)
(length+ <list>)

(fold <callable> <seed> <ac> ...)

(map <callable> <ac> ...)
(for-each <callable> <ac> ...)
(filter <predicate?> <ac>)

(count <predicate?> <ac>)
(remove <predicate?> <ac>)

(val <ac> <key>) ; "ref"

(key <predicate?> <ac>) ; returns key of left-most value satisfying <predicate?>

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


;;;;;;;;;;;;;;;;
; OCs
(conj <val> <oc>) ; add <val> as efficiently as possible to <oc>. Makes no guarentee about position.

(init <oc>)
(last <oc>)

(slice <oc> <start-key> <optional-length-or-end-predicate>)

(reverse <oc>)

(remove-first <predicate?> <oc>)
(remove-last <predicate?> <oc>)

(skip <predicate?> <oc>)
(skip-right <predicate?> <oc>)

(fold-right <callable> <seed> <oc> ...) ; only ordered collections have the concept of "right-to-left"

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

(merge <predicate?> <ac> <ac>)

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
        ==============================================================


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