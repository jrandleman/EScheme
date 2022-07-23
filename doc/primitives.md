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
*argv*

*file-separator*
*path-separator*

*os-name*
*os-version*
*os-arch*

*escm-path*
```


------------------------
## Numbers:
```scheme
(= <num> <num> ...)
(< <num> <num> ...)
(> <num> <num> ...)
(<= <num> <num> ...)
(>= <num> <num> ...)

(+ <num> <num> ...)
(- <num> <num> ...) (- <num>)
(/ <num> <num> ...) (/ <num>)
(* <num> <num> ...)

(expt <num> <num> ...)
(exp <num>)
(log <num>)
(sqrt <num>)
(abs <num>)

(quotient <num1> <num2>)
(remainder <num1> <num2>)

(round <num>)
(floor <num>)
(ceiling <num>)
(truncate <num>)

(min <num> ...)
(max <num> ...)

(number? <obj>)
(integer? <num>)
(finite? <num>)
(infinite? <num>)
(nan? <num>)

(odd? <num>)
(even? <num>)

(positive? <num>)
(negative? <num>)
(zero? <num>)

(sin <num>)
(cos <num>)
(tan <num>)
(asin <num>)
(acos <num>)
(atan <num>) (atan <num> <num>)
(sinh <num>)
(cosh <num>)
(tanh <num>)
(asinh <num>)
(acosh <num>)
(atanh <num>)

(random) ; random number between 0.0 and 1.0
```


------------------------
## IO:
```scheme
(write <obj>)
(display <obj>)
(newline)
(read)
(read-string <str>) ; returns a pair: (cons <read-datum> <str-without-serialized-read-datum>)
```


------------------------
## System:
```scheme
(exit <optional-integer-exit-code>)

(file-read <filename-str>) ; read file contents as a data structure
(file-read-string <filename-str>) ; read file contents as a string

(file-write <filename-str> <obj>)
(file-display <filename-str> <obj>)

(file-delete! <filename-str>)

(file? <str>)

(load <filename-str>)

(system <cmd-str/str-list> <optional-env-var-str-list> <optional-dir-str>)
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

(sort <predicate?> <list>)
(sorted? <predicate?> <list>)

(list? <obj>)
(list*? <obj>)
(circular-list? <obj>)
(alist? <obj>) ; associative list predicate
(null? <obj>)
```


------------------------
## Equality:
```scheme
(eq? <obj> <obj> ...)
(equal? <obj> <obj> ...)
```


------------------------
## Type Predicates:
```scheme
(typeof <obj>) ; returns a symbol of obj's typename
(void? <obj>)
(boolean? <obj>)
```


------------------------
## Type Coercions:
```scheme
(number->string <num>)
(string->number <str>)

(keyword->symbol <keyword>)
(symbol->keyword <sym>)

(string->symbol <str>)
(symbol->string <sym>)

(string->keyword <str>)
(keyword->string <keyword>)

(write-to-string <obj>)
(display-to-string <obj>)
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
## Utilities:
```scheme
(not <obj>)
(error <reason> <arg> ...)
(copy <obj>)
(force <delayed-expression>)

(call-with-current-continuation <unary-callable>) ; aliased by <call/cc>
(dynamic-wind <thunk1> <thunk2> <thunk3>)

(values <obj> ...)
(call-with-values <thunk-producer> <callable-consumer>)

(with-exception-handler <unary-callable-handler> <callable-thunk>)
(raise <obj>)
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
(scons <obj> <obj>)
(stream-pair? <obj>)
(stream? <obj>)
(scar <stream-pair>) ... (scddddr <stream-pair>)
(stream->list <stream> <list-length>)
(stream-map <callable> <stream>)
(stream-filter <predicate?> <stream>)
(stream-ref <stream> <index>)
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

(thread-start! <thread>)
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
