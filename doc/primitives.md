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
#path ; reader literal (expands to a string, not a true variable)

*argv*

*file-separator*
*path-separator*

*os-name*
*os-version*
*os-architecture*

*escm-path*
*escm-execution-command*

*dosync-lock*

*generator-complete*

*min-radix*
*max-radix*
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

(quotient <real1> <real2>)
(remainder <real1> <real2>)
(modulo <real1> <real2>)
(divrem <real1> <real2>)

(modf <real1> <real2>)

(gcd <integer1> <integer2>)
(lcm <integer1> <integer2>)

(round <real>)
(floor <real>)
(ceiling <real>)
(truncate <real>)

(min <real> ...)
(max <real> ...)

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
(read-string <str>) ; returns a pair: (cons <read-datum> <str-without-serialized-read-datum>)
(read-line <optional-input-port>)

(eof? <obj>)
```


------------------------
## System:
```scheme
(exit <optional-integer-exit-code>)

(load <filename-str>)

(load-from <directory-str> <filename-str>) ; preferred for portability, esp. where <directory-str> is <#path>

(system <command-str> <optional-env-var-str-list> <optional-directory-str>)
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
(open-output-file <filename-string>)
(open-output-file+ <filename-string>)

(close-port! <port>)

(port-path <port>)
(port-position <input-port>) ; returns a pair: (<line-number> . <column-number>)

(port? <obj>)
(input-port? <obj>)
(output-port? <obj>)

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
(append <list> ... <obj>)

(length <list>)

(reverse <list>)

(map <callable> <list> ...)
(filter <predicate?> <list>)
(for-each <callable> <list> ...)

(fold <callable> <seed-obj> <list> ...)
(fold-right <callable> <seed-obj> <list> ...)

(last <list>)
(init <list>)
(ref <list> <index-num>)
(sublist <list> <index-num> <optional-length-num>) ; length defaults to end of list

(memq <obj> <list>)
(member <obj> <list>)

(assq <key-obj> <alist>)
(assoc <key-obj> <alist>)

(sort <binary-predicate?> <list>)
(sorted? <binary-predicate?> <list>)

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

(vector-length <vector>)

(vector-ref <vector> <index>) ; equivalent to (<vector> <index>)
(vector-set! <vector> <index> <obj>)

(vector-fill! <vector> <fill-value>)
(vector-grow! <vector> <length> <fill-value>)
(vector-insert! <vector> <index> <obj>) ; insert <obj> at <index> & shift back items as needed
(vector-delete! <vector> <index>)       ; returns the deleted object

(vector-push! <vector> <obj>)
(vector-push-front! <vector> <obj>)
(vector-pop! <vector>) ; returns the popped item
(vector-pop-front! <vector>) ; returns the popped item

(vector-append <vector> ...)
(vector-append! <vector> <appended-vector> ...)

(vector-reverse <vector>)
(subvector <vector> <index> <optional-length>) ; defaults to the end fo the vector

(vector-memq <vector> <obj>)   ; returns index of position, or #f if DNE. Uses <eq?> for comparisons.
(vector-member <vector> <obj>) ; returns index of position, or #f if DNE. Uses <equal?> for comparisons.

(vector-sort <binary-predicate?> <vector>)
(vector-sorted? <binary-predicate?> <vector>)

(vector? <obj>)
(vector-empty? <vector>)
```


------------------------
## Hashmaps:
```scheme
(hashmap <key> <value> ...)

(hashmap-keys <hashmap>)
(hashmap-values <hashmap>)

(hashmap-key? <hashmap> <obj>)

(hashmap-ref <hashmap> <key>)          ; equivalent to: (<hashmap> <key>)
(hashmap-set! <hashmap> <key> <value>) ; returns whether replaced another entry
(hashmap-delete! <hashmap> <key>)      ; returns success status

(hashmap-length <hashmap>)
(hashmap-empty? <hashmap>)

(hashmap-merge <hashmap> ...)
(hashmap-merge! <hashmap> <merged-hashmap> ...)

(hashmap? <obj>)

(hashcode <obj> ...)
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

(write-to-string <obj>)
(display-to-string <obj>)
(pretty-print-to-string <obj>)
```


------------------------
## Strings:
```scheme
(string-length <str>)
(string-empty? <str>)

(string-reverse <str>)

(string-append <str> ...)

(string-ref <str> <index-num>) ; returns a substring of length 1 (since no characters)
(substring <str> <index-num> <optional-length-num>)  ; length defaults to end of string

(string-upcase <str>)
(string-downcase <str>)

(string-escape <str>)
(string-unescape <str>)

(string-replace <str> <regex-str> <replacement-str>)
(string-trim <str>)

(string-contains <str1> <str2>) ; returns index of appearance or #f if not present
(string-contains-right <str1> <str2>) ; returns index of appearance or #f if not present

(string-join <string-list> <optional-str>) ; str defaults to ""
(string-split <str> <regex-str>)

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

(string? <obj>)

(stringf <format-string> <arg> ...)
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
```


------------------------
## Meta:
```scheme
(apply <callable> <obj-argument-list>)

(compile <quoted-escm-expression>)

(eval-bytecode <quoted-escm-bytecode-list>)

(eval <quoted-escm-expression>)

(gensym)

(syntax? <macro-name-symbol>)
(expand-syntax <quoted-macro-expression>)
(delete-syntax! <macro-name-symbol>)
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