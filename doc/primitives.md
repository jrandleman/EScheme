<!-- primitives.md (tasnim) -->

# Primitive Procedures
### Functions implemented in Java accessible by EScheme source code!


------------------------
## Language Help:
```scheme
(help <optional-query-symbol-or-obj>)
```


------------------------
## Variables:
```scheme
#path ; reader literal that expands to a string (not a true variable)

*argv*

*file-separator*
*path-separator*

*os-name*
*os-version*
*os-architecture*

*escm-path*
*escm-execution-command*

*dosync-lock* ; parameter
*dosync-module-lock* ; global variable

*generator-complete*

*min-radix*
*max-radix*

*min-priority*
*max-priority*

*import*

*load-once-files*
```


------------------------
## Numbers:
```scheme
(= <num> <num> ...)
(< <real> <real> ...)
(> <real> <real> ...)
(<= <real> <real> ...)
(>= <real> <real> ...)

(+ <num> <num> ...)
(- <num> <num> ...) (- <num>)
(/ <num> <num> ...) (/ <num>)
(* <num> <num> ...)

(expt <num> <num> ...)
(exp <num>)
(log <num> <optional-base>)
(sqrt <num>)
(abs <real>)

(expt-mod <base-real> <power-real> <mod-real>)

(numerator <real>)
(denominator <real>)

(quotient <real1> <real2>)
(remainder <real1> <real2>)
(modulo <real1> <real2>)
(divrem <real1> <real2>)

(modf <real1> <real2>)

(integral <inexact>)
(fractional <inexact>)

(gcd <integer1> <integer2>)
(lcm <integer1> <integer2>)

(round <real>)
(floor <real>)
(ceiling <real>)
(truncate <real>)

(min <real> ...)
(max <real> ...)

(exact->inexact <number>)
(inexact->exact <number>)

(number? <obj>)
(complex? <obj>)
(real? <obj>)
(inexact? <obj>)
(exact? <obj>)
(integer? <obj>)
(finite? <obj>)
(infinite? <obj>)
(nan? <obj>)

(odd? <real>)
(even? <real>)

(positive? <real>)
(negative? <real>)
(zero? <real>)

(sin <num>)
(cos <num>)
(tan <num>)
(asin <num>)
(acos <num>)
(atan <num>) (atan <real> <real>)
(sinh <num>)
(cosh <num>)
(tanh <num>)
(asinh <num>)
(acosh <num>)
(atanh <num>)

(npr <n-integer> <r-integer>)
(ncr <n-integer> <r-integer>)

(random) ; random number between 0.0 and 1.0

(make-rectangular <real-real> <imag-real>)
(make-polar <magnitude-real> <angle-real>)

(real-part <number>)
(imag-part <number>)

(magnitude <number>)
(angle <number>)
(conjugate <number>)
```


------------------------
## IO:
```scheme
(pretty-print <optional-output-port> <obj>) ; aliased by <pprint>
(write <optional-output-port> <obj>)
(display <optional-output-port> <obj>)
(newline <optional-output-port>)

(pretty-printf <optional-output-port> <format-string> <arg> ...) ; aliased by <pprintf>
(writef <optional-output-port> <format-string> <arg> ...)
(displayf <optional-output-port> <format-string> <arg> ...)

(read <optional-input-port>)
(read-string <str>) ; returns a pair: (cons <read-datum> <str-without-read-datum>)
(read-line <optional-input-port>)
(read-char <optional-input-port>)
(read-chars <integer> <optional-input-port>)

(eof? <obj>)
```


------------------------
## System:
```scheme
(exit <optional-integer-exit-code>)

(load <optional-directory-str> <filename-str>) ; this (& all other <load> variants) work on both regular & <serialize>d EScheme files.
(load-once <optional-directory-str> <filename-str>) ; prevents cyclic loading

(module? <obj>)
(module-path <module>) ; <module>'s absolute file path location
(module-bindings <module>) ; list of <module>'s defined variable symbols (beware that all modules have all the stdlib!)

(system <optional-millisecond-timeout> <command-str> <optional-env-var-str-list> <optional-directory-str>)

(getenv <optional-var-name-str>)
```


------------------------
## Files:
```scheme
(file-read <file-path-str>) ; read file contents as a data structure
(file-read-string <file-path-str>) ; read file contents as a string

(file-write <file-path-str> <obj>)
(file-display <file-path-str> <obj>)
(file-pretty-print <file-path-str> <obj>)

(file-write+ <file-path-str> <obj>)
(file-display+ <file-path-str> <obj>)
(file-pretty-print+ <file-path-str> <obj>)

(path? <path-str>)
(directory? <path-str>)
(file? <path-str>)

(file-delete! <file-path-str>)
(directory-delete! <directory-path-str>)
(directory-recursive-delete! <directory-path-str>)
(path-delete! <path-str>)
(path-recursive-delete! <path-str>)

(directory-entries <directory-path-str>) ; with dot-files
(directory-entries* <directory-path-str>) ; no dot-files

(current-directory)

(path-parent <path-str>)
(path-file <path-str>)

(make-directory <path-str>) ; triggers error if directories in <path-str> don't exist
(make-directory! <path-str>) ; creates directories as needed to make this directory exist

(absolute-path <path-str>)
(absolute-path? <path-str>)

(file-extension <path-str>)
(file-has-extension? <path-str> <extension-str>)
(swap-file-extension <path-str> <new-extension-str>)
(remove-file-extension <path-str>)

(file-size <file-path-str>)

(move-file <old-path-str> <new-path-str>) ; triggers error if directories in <new-path-str> don't exist
(move-file! <old-path-str> <new-path-str>) ; creates directories & overwrites files as needed to make this path exist

(copy-file <old-path-str> <new-path-str>) ; triggers error if <new-path-str> already exists
(copy-file! <old-path-str> <new-path-str>) ; creates directories & overwrites files as needed to make this path exist
```


------------------------
## Ports:
```scheme
(open-input-file <filename-string>)
(open-output-file <optional-filename-string>) ; nullary call creates a "temporary file"!
(open-output-file+ <filename-string>)

(close-port! <port>)

(port-path <port>)
(port-position <input-port>) ; returns a pair: (<line-number> . <column-number>)

(port? <obj>)
(input-port? <obj>)
(output-port? <obj>)
(temp-port? <obj>) ; check if port handles a "temporary file"!

(open-port? <port>)
(closed-port? <port>)

(current-input-port)
(current-output-port)

(call-with-input-file <filename-string> <unary-callable>)
(call-with-output-file <filename-string> <unary-callable>)
(call-with-output-file+ <filename-string> <unary-callable>)

(with-input-from-file <filename-string> <thunk-callable>)
(with-output-to-file <filename-string> <thunk-callable>)
(with-output-to-file+ <filename-string> <thunk-callable>)

(peek-port <input-port>)
```


------------------------
## Pairs:
```scheme
(cons <obj> <obj>)

(car <pair>) ... (cddddr <pair>)

(pair? <obj>)
(atom? <obj>) ; equivalent to (not (pair? <obj>))
```


------------------------
## Lists:
```scheme
(list <obj> ...) (list)
(list* <obj> <obj> ...) ; create a dotted list

(unfold <break-condition> <map-callable> <successor-callable> <seed>)
(unfold-right <break-condition> <map-callable> <successor-callable> <seed>)

(memq <obj> <list>)
(member <obj> <list>)

(assq <key-obj> <alist>)
(assoc <key-obj> <alist>)

(list? <obj>)
(list*? <obj>)
(alist? <obj>) ; associative list predicate
(null? <obj>)
```


------------------------
## Vectors:
```scheme
(vector <obj> ...)
(make-vector <length> <fill-value>)
(vector-set! <vector> <index> <obj>)

(vector-fill! <vector> <fill-value>)
(vector-grow! <vector> <length> <fill-value>)
(vector-insert! <vector> <index> <obj>) ; insert <obj> at <index> & shift back items as needed
(vector-delete! <vector> <index>)       ; returns the deleted object

(vector-push! <vector> <obj>)
(vector-push-front! <vector> <obj>)
(vector-pop! <vector>) ; returns the popped item
(vector-pop-front! <vector>) ; returns the popped item

(vector-append! <vector> <appended-vector> ...)

(vector-unfold <break-condition> <map-callable> <successor-callable> <seed>)
(vector-unfold-right <break-condition> <map-callable> <successor-callable> <seed>)

(vector-memq <vector> <obj>)   ; returns index of position, or #f if DNE. Uses <eq?> for comparisons.
(vector-member <vector> <obj>) ; returns index of position, or #f if DNE. Uses <equal?> for comparisons.

(vector? <obj>)
```


------------------------
## Hashmaps:
```scheme
(hashmap <key> <value> ...)

(hashmap-keys <hashmap>)
(hashmap-values <hashmap>)

(hashmap-key? <hashmap> <obj>)
(hashmap-val? <hashmap> <obj>)

(hashmap-set! <hashmap> <key> <value>) ; returns whether replaced another entry
(hashmap-delete! <hashmap> <key>)      ; returns whether deleted an entry

(hashmap-merge <hashmap> ...)
(hashmap-merge! <hashmap> <merged-hashmap> ...)

(hashmap? <obj>)

(hashcode <obj> ...)
```


------------------------
## Strings:
```scheme
(string? <obj>)

(string <str-or-char> ...)

(string-java-length <str>)

(string-upcase <str>)
(string-downcase <str>)

(string-escape <str>)
(string-java-escape <str>)
(string-unescape <str>)

(string-replace <str> <regex-str> <replacement-str>)
(string-trim <str>)

(string-contains <str1> <str2>) ; returns index of appearance or #f if not present
(string-contains-right <str1> <str2>) ; returns index of appearance or #f if not present

(string-join <string-list> <optional-str>) ; str defaults to ""
(string-split <str> <optional-regex-str>)

(string-unfold <break-condition> <map-callable> <successor-callable> <seed>)
(string-unfold-right <break-condition> <map-callable> <successor-callable> <seed>)

(string=? <str> <str> ...)
(string<? <str> <str> ...)
(string>? <str> <str> ...)
(string<=? <str> <str> ...)
(string>=? <str> <str> ...)

(string-ci=? <str> <str> ...)
(string-ci<? <str> <str> ...)
(string-ci>? <str> <str> ...)
(string-ci<=? <str> <str> ...)
(string-ci>=? <str> <str> ...)

(stringf <format-string> <arg> ...)
```


------------------------
## Associative Collections:
* Includes: Strings, Lists, Vectors, Hashmaps
* Denoted by `<ac>`
```scheme
(associative-collection? <obj>)
(ac? <obj>)

(head <ac>)
(tail <ac>)

(empty? <ac>)
(length <ac>)
(length+ <ac>) ; returns #f for dotted lists instead of throwing an error

(fold <callable> <seed> <ac> ...)

(map <callable> <ac> ...)
(for-each <callable> <ac> ...)
(filter <predicate?> <ac>)

(count <predicate?> <ac>)
(remove <predicate?> <ac>)

(val <ac> <key>) ; "ref"

(key <predicate?> <ac>) ; returns key of left-most value satisfying <predicate?>

(append <ac> ...)

(delete <ac> <key>) ; return copy of <ac> w/o <key>

(conj <key> <val> <ac>) ; return version of <ac> with <val> associated to <key>

(any? <predicate?> <ac> ...)
(every? <predicate?> <ac> ...)

(take <ac> <length>)
(drop <ac> <length>)

(ac->list <ac>)
(ac->string <ac>)
(ac->vector <ac>)
(ac->hashmap <ac>)

(union <elt=?> <ac> ...)
(intersection <elt=?> <ac> ...)
(difference <elt=?> <ac> ...)
(symmetric-difference <elt=?> <ac> ...)
```


------------------------
## Ordered Collections:
* Includes: Strings, Lists, Vectors
* Denoted by `<oc>`
```scheme
(ordered-collection? <obj>)
(oc? <obj>)

(conj <val> <oc>) ; add <val> as efficiently as possible to <oc>. Makes no guarentee about position.

(init <oc>)
(last <oc>)

(slice <oc> <start-index> <optional-length-or-end-predicate>)

(reverse <oc>)

(remove-first <predicate?> <oc>)
(remove-last <predicate?> <oc>)

(skip <predicate?> <oc>)       ; get the 1st item in <oc> that fails <predicate?>
(skip-right <predicate?> <oc>) ; get the last item in <oc> that fails <predicate?>

(fold-right <callable> <seed> <oc> ...) ; only ordered collections have the concept of "right-to-left"

(key-right <predicate?> <oc>) ; returns key of right-most value satisfying <predicate?>

(drop-right <oc> <length>)
(drop-while <predicate?> <oc>)
(drop-right-while <predicate?> <oc>)

(take-right <oc> <length>)
(take-while <predicate?> <oc>)
(take-right-while <predicate?> <oc>)

(sort <binary-predicate?> <oc>)
(sorted? <binary-predicate?> <oc>)

(merge <predicate?> <oc> <oc>)

(delete-neighbor-duplicates <elt=?> <oc>)
```


------------------------
## Characters:
```scheme
(char? <obj>)

(char-alphabetic? <char>)
(char-numeric? <char>)
(char-whitespace? <char>)
(char-upper-case? <char>)
(char-lower-case? <char>)
(char-alphanumeric? <char>)
(char-control? <char>)
(char-print? <char>)
(char-graph? <char>)
(char-punctuation? <char>)
(char-xdigit? <char>)

(char-upcase <char>)
(char-downcase <char>)

(char=? <char> ...)
(char<? <char> ...)
(char>? <char> ...)
(char<=? <char> ...)
(char>=? <char> ...)

(char-ci=? <char> ...)
(char-ci<? <char> ...)
(char-ci>? <char> ...)
(char-ci<=? <char> ...)
(char-ci>=? <char> ...)

(char-pair? <char>)
(java-char? <char>)
(ascii-char? <char>)

(char-count <char>)

(char-digit <char> <optional-radix>)
(char-for-digit <integer> <optional-radix>)

(char-name <char>)
(char-defined? <char>)

(char-high? <char>)
(char-low? <char>)
(char-high <char>)
(char-low <char>)
(char-codepoint <high-char> <low-char>)
```


------------------------
## Symbols:
```scheme
(symbol-append <sym> ...)
(symbol? <obj>)
```


------------------------
## Keywords:
```scheme
(keyword-append <keyword> ...)
(keyword? <obj>)
```


------------------------
## Booleans:
```scheme
(boolean? <obj>)
(not <obj>)
```


------------------------
## Void:
```scheme
(void? <obj>)
```


------------------------
## Equality:
```scheme
(eq? <obj> <obj> ...)
(equal? <obj> <obj> ...)
```


------------------------
## Type Coercions:
```scheme
(number->string <num> <optional-radix>)
(string->number <str> <optional-radix>)

(keyword->symbol <keyword>)
(symbol->keyword <sym>)

(string->symbol <str>)
(symbol->string <sym>)

(string->keyword <str>)
(keyword->string <keyword>)

(vector->list <vector>)
(list->vector <list>)

(hashmap->list <hashmap>)
(list->hashmap <list>)

(hashmap->vector <hashmap>)
(vector->hashmap <vector>)

(char->integer <char>)
(integer->char <integer>)

(list->string <list>)
(string->list <string>)

(vector->string <vector>)
(string->vector <string>)

(write-to-string <obj>)
(display-to-string <obj>)
(pretty-print-to-string <obj>)
```


------------------------
## Utilities:
```scheme
(typeof <obj>) ; returns a symbol of obj's typename

(error <reason> <arg> ...)
(errorf <format-string> <arg> ...)

(copy <obj>)

(force <delayed-expression>)

(call-with-current-continuation <unary-callable>) ; aliased by <call/cc>
(dynamic-wind <thunk1> <thunk2> <thunk3>)

(values <obj> ...)
(call-with-values <thunk-producer> <callable-consumer>)

(with-exception-handler <unary-callable-handler> <callable-thunk>)
(raise <obj>)

(time <callable> <arg> ...) ; (<time-taken-in-milliseconds> . <result>)

(serialize <escm-file-path> <serialized-file-path>)
(serialized? <file-path>)
```


------------------------
## Meta:
```scheme
(apply <callable> <obj-argument-list>)

(compile <quoted-escm-expression>)

(eval-bytecode <quoted-escm-bytecode-list>)

(eval <quoted-escm-expression>)

(gensym <optional-name-symbol>) ; optional arg improves readability when printing generated symbols

(syntax? <obj>) ; (syntax? if)
(expand-syntax <quoted-macro-expression>) ; (expand-syntax '(if 1 2 3))
```


------------------------
## Functional:
```scheme
(compose <callable> ...)
(bind <callable> <obj> ...) ; bind <obj> ... as arguments to <callable>
(id <obj>)
(procedure? <obj>)
(callable? <obj>) ; (or (procedure? <obj>) (functor? <obj>) (class? <obj>))
```


------------------------
## Streams:
```scheme
(stream-pair? <obj>)
(stream? <obj>)

(scar <stream-pair>) ... (scddddr <stream-pair>)
(stream-ref <stream> <index>)

(stream->list <stream> <list-length>)

(stream-map <callable> <stream> ...)
(stream-filter <predicate?> <stream>)
(stream-iterate <update-callable> <seed>)
(stream-constant <obj> ...)
(stream-append <stream> ...)
(stream-interleave <stream1> <stream2>)
(stream->generator <stream>)
```


------------------------
## Generators:
```scheme
(complete-all-generators! <generator-procedure> ...)
(complete-n-generators! <total-generators-to-complete> <generator-procedure> ...)
```


------------------------
## Concurrency:
### Thread:
```scheme
(thread <optional-string-name> <callable-thunk>)
(thread? <obj>)

(thread-name <thread>)
(thread-id <thread>)
(thread-runnable <thread>)
(thread-status <thread>)

(thread-yield)

(thread-set-daemon! <thread> <boolean-status>)
(thread-daemon? <thread>)

(thread-priority <thread>)
(thread-set-priority! <thread> <int-priority>)

(thread-start! <thread> ...)
(thread-join! <thread> <optional-max-millis-to-wait>)

(thread-interrupted? <thread>)
(thread-interrupt! <thread>)

(interrupted?!)
(sleep <millis-to-sleep>)
(current-thread)

(parallel <callable-thunk> ...)

(thread-define' <optional-thread> <symbolic-variable-name> <value>)
(thread-set!' <optional-thread> <symbolic-variable-name> <value>)
(thread-get' <optional-thread> <symbolic-variable-name>)
(thread-defined?' <optional-thread> <symbolic-variable-name>)

(thread-dynamic-environment <optional-thread>)
```
### Mutex:
```
(mutex <optional-string-name>)
(mutex? <obj>)

(mutex-name <mutex>)

(mutex-specific <mutex>)
(mutex-specific-set! <mutex> <obj>)

(mutex-lock! <mutex> <optional-millis-timeout>) ; returns if acquired lock prior provided timeout (defaults to Infinity)
(mutex-unlock! <mutex>)
(mutex-locked? <mutex>) ; mutex is held by any thread

(mutex-queue-length <mutex>) ; total threads waiting to acquire the mutex
(mutex-queued? <mutex>) ; thread is waiting to acquire the mutex

(mutex-hold-count <mutex>) ; total holds on the mutex by the current thread
(mutex-held? <mutex>) ; mutex is held by the current thread
```


------------------------
## Object Orientation:
```scheme
(meta-object? <obj>) ; (or (class? <obj>) (interface? <obj>) (object? <obj>))
(object? <obj>)
(class? <obj>)
(interface? <obj>)

(functor? <obj>) ; (and (object? <obj>) (oo-has? <obj> '->procedure))

(oo-is? <object> <class-or-interface>) ; equivalent to Java's <instanceof>

; The following only operate on STATIC props if <meta-object> is a class or interface
;   => <property-symbol-name> may be passed as "'prop1 'prop2" OR "'prop1.prop2"
(oo-has? <meta-object> <property-symbol-name> ...)
(oo-get <meta-object> <property-symbol-name> ...)          ; triggers an error upon failure
(oo-set! <meta-object> <property-symbol-name> ... <value>) ; triggers an error upon failure
(oo-define <meta-object> <property-symbol-name> ... <value>)

(oo-super <class-or-object>)  ; returns the super obj
(oo-interfaces <meta-object>) ; returns list of interface objs
(oo-properties <meta-object>) ; returns list of name symbols (and-for classes and interfaces-whether they're static or not)
```


------------------------
## Date-Time:
```scheme
(current-time) ; (<hour> <minute> <second> <millisecond>)
(current-date) ; (<year> <month> <day>)

(epoch-time) ; the current number of milliseconds from the epoch

(time-zone) ; time zone name as a string (#f if unknown)
(day)       ; name of the current week-day as a string (#f if unknown)
(month)     ; name of the current month as a string (#f if unknown)
(year)      ; number of the current year as a string (#f if unknown)
```