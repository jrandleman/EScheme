# Table of Contents

* [Intrinsic-Types](#Intrinsic-Types)
* [Procedures](#Procedures)
  * [Associative-Collections](#Associative-Collections)
  * [Booleans](#Booleans)
  * [Characters](#Characters)
  * [Concurrency](#Concurrency)
    * [Mutexes](#Mutexes)
    * [Promises](#Promises)
    * [Threads](#Threads)
  * [Date-Time](#Date-Time)
  * [Equality](#Equality)
  * [Files](#Files)
  * [Functional](#Functional)
  * [Hashmaps](#Hashmaps)
  * [IO](#IO)
  * [Keywords](#Keywords)
  * [Lists](#Lists)
  * [Meta](#Meta)
  * [Numbers](#Numbers)
  * [OOP](#OOP)
  * [Ordered-Collections](#Ordered-Collections)
  * [Pairs](#Pairs)
  * [Ports](#Ports)
  * [Strings](#Strings)
  * [Symbols](#Symbols)
  * [System](#System)
  * [Type-Coercions](#Type-Coercions)
  * [Utilities](#Utilities)
  * [Vectors](#Vectors)
  * [Void](#Void)
* [Syntax](#Syntax-1)
  * [Core](#Core)
  * [Generators](#Generators)
  * [Modules](#Modules)
  * [Objects](#Objects)
  * [Streams](#Streams)
  * [Synchronization](#Synchronization)
* [Topics](#Topics)
* [Variables](#Variables)


-------------------------------------------------------------------------------
# Intrinsic-Types


-------------------------------------------------------------------------------
## `associative-collection`

### Description:
```
Family of collection types that associate keys to values.
Used to provide a generic interface across algorithmic primitives!

Hashmaps are the most flexible example of such (supporting arbitrary key &
value types), while strings are the most restrictive (only supporting index
keys and character values).

Their coercion hierarchy is as follows: String < List < Vector < Hashmap
```

-------------------------------------------------------------------------------
## `boolean`

### Description:
```
The boolean type. #t is true and #f is false. Any non-false value is truthy.
Typically returned by predicate actions that end in '?'.
```

-------------------------------------------------------------------------------
## `callable`

### Description:
```
Any EScheme value that can be applied to arguments: (<callable> <argument> ...)

This includes procedures, functors (objects with a '->procedure' method defined),
classes (to invoke their constructor), as well as O(1)-access containers
(strings, vectors, and hashmaps).
```

-------------------------------------------------------------------------------
## `character`

### Description:
```
Reader literals have the '#\' prefix: #\h #\e #\l #\l #\o #\!

Represented by a 32bit codepoint internally:
  * In keeping with Java, any codepoint <= 16bits correlates to a true, single
    character in strings.
  * Any codepoint above 16bits (as in Java) becomes 2 separate characters in 
    strings. However, most EScheme string operations (like <length>) operate 
    relative to the number of codepoints in a string, NOT to the number of
    'java characters'.
    - The only exceptions to this rule are EScheme's regex primtives, which 
      operate relative to 16bit characters since they wrap Java's regex ops.

Supports Named Characters:
  #\space,     #\tab, #\newline, #\page, #\return,
  #\backspace, #\nul, #\esc,     #\delete

Supports Unicode Codepoints (note 'u' vs. 'U'):
  #\uXXXX (create a 16bit hex-code character)
  #\UXXXXXXXX (create a 32bit hex-code character)

Displaying characters prints them in a human-readable way, whereas writing them
prints characters with their prefix such that they can be re-read by the reader
as EScheme data.
```

-------------------------------------------------------------------------------
## `class`

### Description:
```
The value created by the <class> macro. Access static fields and methods via
"." syntax:

  class.static-field
  (class.static-method <arg1> <arg2> ...)

See the <class> macro from <Objects> in <Syntax> for more class details.
See <object-system> in <Topics> for more general OO details.
See <object>, <interface>, and <meta-object> in <Intrinsic-Types> for more 
specific object oriented typing details.
```

-------------------------------------------------------------------------------
## `eof`

### Description:
```
The "end-of-file" value terminating files.
Denoted via #eof and detected via (eof? <obj>).
Ends file/REPL evaluation early if read in as a single expression.
```

-------------------------------------------------------------------------------
## `hashmap`

### Description:
```
Also see the <hashmap> procedure.
A hashmap containing key-value associations of "<key> <value> ...".
Create hashmap literals via the {<key> <value> ...} syntax.
Hashmaps are applicable to a key to get their entry: (<hashmap> <key>)
```

-------------------------------------------------------------------------------
## `interface`

### Description:
```
The value created by the <interface> macro. Access static fields and methods
via "." syntax:

  interface.static-field
  (interface.static-method <arg1> <arg2> ...)

See the <interface> macro from <Objects> in <Syntax> for more interface details.
See <object-system> in <Topics> for more general OO details.
See <object>, <class>, and <meta-object> in <Intrinsic-Types> for more specific 
object oriented typing details.
```

-------------------------------------------------------------------------------
## `keyword`

### Description:
```
Similar to symbols, but they always evaluate to themselves.
Denoted as a symbol prefixed with ':'.

Can be used to denote runtime types for procedures!
  => See <type-system> in <Topics> for more details on EScheme's types!
```

-------------------------------------------------------------------------------
## `list`

### Description:
```
Also see the <list> procedure.
A linked list containing "<obj> ...". The empty list is (quote ()).
Lists are right-nested pairs ending in nil: (quote ())
Create list literals via the (<item> ...) syntax.
```

-------------------------------------------------------------------------------
## `meta-object`

### Description:
```
Super type of objects, classes, and interfaces, all of which support
'dot-notation' to access a property via ".". 

Note that, despite supporting dot-notation, modules are not meta-objects.

See <object-system> in <Topics> for more general OO details.
See <object>, <class>, and <interface> in <Intrinsic-Types> for more
specific object oriented typing details.
```

-------------------------------------------------------------------------------
## `module`

### Description:
```
The value created by an <import> expression.

Modules present an alternative to Scheme's usual inter-file semantics.

The <load> function has always served as a means by which to execute the code
of another Scheme file in the calling file's global environment. This simplicity
is a double edged sword though: while making <load> easy to implement, it leaves
much to be desired with respect to enabling encapsulation of code across
coordinated Scheme files.

As such, in addition to <load>, EScheme offers a minimalistic module system
that strives to enable file-specific encapsulation of EScheme code. Files
processed via the <import> macro are defined as <module> objects within the
enclosing enivironment. See the <import> 'help' entry for more details on how
EScheme evaluates modules.

Each module has its own isolated global environment, and has automatic access
to EScheme's standard library. Note that this means that operations that depend
on global variables (e.g. <load-once>) are hence only able to operate on
a module-relative basis.

  * Note that <dosync> notably works across modules, since its internal lock
    was created via <define-parameter>. Use <dosync-module> for module-relative
    locking behavior.

Both variables and macros can be accessed from a module by using EScheme's
dot-notation: for example, to access variable 'PersonClass' from module 'Mod',
we write 'Mod.PersonClass'.

Further note that imported modules are cached! Importing the same module
multiple times does not cause multiple evaluations of that module. Use the
<reload> macro if you'd like to forcefully re-evaluate the module in question.

Additionally, the <from> macro may be used to load specific variables within
a given module, without defining that module itself as a local variable.

Modules may be introspected upon by 2 primitives:

  1. <module-path>: yield the absolute file path to the module (this is what
     distinguishes one module from another under the hood).
  2. <module-bindings>: yield a list of the symbols defined in a module (beware:
     every module redefines the entire EScheme standard library!).

Note that the 'meta-thread's environment (see <thread-define>) is module
independant!

Use the '*import*' variable to determine if the current file was <import>ed.
Can combine with <unless> to mimic Python's "if __name__=='__main__':" pattern:

  (unless *import*
    <execute-main-escheme-code-here> ...)

Lastly, note that the concept of 'parameter' variables exist in order to
have global state shared across modules. See the <define-parameter> and
<parameter?> 'help' entries for more details.
```

-------------------------------------------------------------------------------
## `mutex`

### Description:
```
The value created by the <mutex> function, using a true JVM reentrant-lock
under the hood. See the <mutex> function for creation details, and <concurrency>
in <Topics> for more details on leveraging parallelism in EScheme.
```

-------------------------------------------------------------------------------
## `nil`

### Description:
```
The "null" type terminating lists. Denoted via (quote ()) and #nil
```

-------------------------------------------------------------------------------
## `number`

### Description:
```

EScheme provides a rich numeric tower, in keeping with its second namesake.

This entails fully supporting big integers, fractions, doubles, complex numbers, 
exactness conversions, radix-dependant parsing, and more!

An overview of EScheme's numeric literal syntax is provided below. See <help>
for more information on EScheme's numeric primitive functions.

4 Number Types:
 0. Exact/Ratnum (rational number)
    *) Arbitrary precision numerator & denominator (automatically reduced to simplest form!)
    *) Special Case: denominator of 1 creates a BigInt

       -1/2 ; stays as a fraction!
       3    ; ratnum w/ denom of 1 = bigint
       4/2  ; gets simplified to bigint 2

 1. Inexact/Flonum (floating-point number)
    * Java <double> under the hood

       1.0
       3.5e10 ; scientific notation
       -4E12  ; also scientific notation

 2. Special Constants:
    *) Positive Infinity: Infinity
    *) Negative Infinity: -Infinity
    *) Not-a-Number: NaN

 3. Complex Numbers:
    *) Both the real and imaginary components will match in exactness
    *) Supports Infinity or -Infinity components (NaN is unique & never complex!)
    *) Special Case: imaginary value of 0 becomes a real (non-complex) number!

       3/4+1/2i
       3/4+0.5i ; becomes 0.75+0.5i to match exactness
       -i       ; valid complex number!
       -44+0i   ; becomes -44

2 Prefix Types:
 0. Radix:
    *) Binary: #b, Octal: #o, Hexadecimal: #x
    *) Nary: #Nr (for N in [*min-radix*, *max-radix*])
       - Note that (typically) *min-radix* is 2 and *max-radix* is 36

       #b-101      ; -5
       #b10/11     ; 2/3
       #o77        ; 63
       #xC0DE      ; 49374
       #xc0de      ; 49374
       #29rEScheme ; 8910753077
       #2r-101/10  ; -5/2

 1. Exactness:
    *) Inexact: #i, Exact: #e

       #i3      ; 3.0
       #i1/2    ; 0.5
       #e3.5    ; 7/2
       #e1.0    ; 1
       #i#2r101 ; Inexact & Binary! => 5.0
```

-------------------------------------------------------------------------------
## `object`

### Description:
```
The value created by invoking a class constructor. Access fields and methods
via "." syntax:

  object.field
  (object.method <arg1> <arg2> ...)

Objects that have a "->procedure" method can be applied directly to arguments
as a functor:

  ; implicitely applies <arg1> <arg2> ... to <object.->procedure>
  (object <arg1> <arg2> ...)

See <object-system> in <Topics> for more general OO details.
See <class>, <interface>, and <meta-object> in <Intrinsic-Types> for more 
specific object oriented typing details.
```

-------------------------------------------------------------------------------
## `ordered-collection`

### Description:
```
Family of collection types that associate ordered indices to values.
Used to provide a generic interface across algorithmic primitives!
Their coercion hierarchy is as follows: String < List < Vector
```

-------------------------------------------------------------------------------
## `pair`

### Description:
```
An immutable pair of objects. <car> and <cdr> respectively get the 1st and 2nd
items. Right-nest them and end with #nil as the last <cdr> to create a list.
Create pair literals via the (<car> . <cdr>) syntax.

For example, the following are equivalent:
  (list 1 2 3)
  (cons 1 (cons 2 (cons 3 #nil)))
```

-------------------------------------------------------------------------------
## `port`

### Description:
```
Input and output ports are handles for read and write files (respectively).
Note that output functions that end in "+" append to the port, whereas the
alternative clears it out first.
```

-------------------------------------------------------------------------------
## `stream`

### Description:
```
A lazy alternative to lists, where each item is only evaluated once accessed.
Use <scons> to create a <stream-pair>, as <cons> does for regular pairs.
Use <scar>/<scdr> to access to the 1st/2nd items in a <stream-pair>.

Example:
  (define (sieve int-stream)
    (scons
      (scar int-stream)
      (sieve
        (stream-filter
          (lambda (n) (positive? (remainder n (scar int-stream))))
          (scdr int-stream)))))

  (define (ints-from n)
    (scons n (ints-from (+ n 1))))

  (define primes (sieve (ints-from 2))) ; infinite stream of prime numbers!

  (display (stream->list primes 13))
  (newline)
```

-------------------------------------------------------------------------------
## `string`

### Description:
```
Also see the <string> procedure.

Represents a Java <string> under the hood (hence immutable).
Literals are denoted via double-quotes.

Strings support the following control characters:
  1) "\t": tab,             represented as a char by #\tab
  2) "\n": newline,         represented as a char by #\newline
  3) "\f": form feed,       represented as a char by #\page
  4) "\r": carriage return, represented as a char by #\return
  5) "\b": backspace,       represented as a char by #\backspace

Octal literals may be used by prefixing up to 6 octal digits with "\", ranging from
\0-\177777 (0-65535 in decimal). This range ensures that each value fits neatly
within a single 16bit Java char internally.
  => Note this extends Java's octals, which only support \0-\377 (0-255 in decimal).

Java 16bit unicode literals may be used by prefixing up to 4 hex digits with "\u".
  => Adjacent unicode literals may be used to create "surrogate pairs" that render
     as a single unicode image for unicode values that require 32bit encoding.

EScheme also extends Java unicode literals with syntax for 32bit unicode values.
Prefixing up to 8 hex digits with "\U" compiles to 2 seperate "\u" instances.
  => For example, both "\U1f608" and "\ud83d\ude08" create the same string, but the
     former is easier to write out after referencing the "U+" code from the internet.

Strings are also applicable to an index to get a character: (<string> <index>)
```

-------------------------------------------------------------------------------
## `symbol`

### Description:
```
Extensively used by metaprograms, symbols are variables that evaluate to
another value.
```

-------------------------------------------------------------------------------
## `syntax`

### Description:
```
The value that macros evaluate to when passed as a procedure argument.
The only value that yields true with <syntax?>, and can be applied to
a list of quoted macros arguments via <apply> like any other procedure.
```

-------------------------------------------------------------------------------
## `thread`

### Description:
```
The value created by the <thread> function, using a true JVM thread under 
the hood. See the <thread> function for creation details, and <concurrency>
in <Topics> for more details on leveraging parallelism in EScheme.
```

-------------------------------------------------------------------------------
## `type-alias`

### Description:
```
The value created by the <type-alias> function, or <define-type> macro.

Used to reference a pre-existing keyword type, <type-alias>s allow us
to hide complicated union/parameterized types behind simple names. 

See <type-system> in <Topics> for more details on EScheme's types.
See the the <type-alias> function and <define-type> syntax descriptions
for more details on how to create and use type aliases.
```

-------------------------------------------------------------------------------
## `vector`

### Description:
```
Also see the <vector> procedure.
A vector containing "<obj> ...".
Create vector literals via the [<item> ...] syntax.
Vectors are applicable to an index to get an entry: (<vector> <index>)
```

-------------------------------------------------------------------------------
## `void`

### Description:
```
The "nothing" type, typically returned by non-pure actions that end in '!'.
Denoted via #void
```

-------------------------------------------------------------------------------
# Procedures


-------------------------------------------------------------------------------
## Associative-Collections


-------------------------------------------------------------------------------
### `ac->hashmap`

#### Signatures:
```scheme
(ac->hashmap <associative-collection>)
```

#### Description:
```
Convert <ac> to a hashmap.
```

-------------------------------------------------------------------------------
### `ac->list`

#### Signatures:
```scheme
(ac->list <associative-collection>)
```

#### Description:
```
Convert <ac> to a list (note that its keys must be increasing integers starting
from 0).
```

-------------------------------------------------------------------------------
### `ac->string`

#### Signatures:
```scheme
(ac->string <associative-collection>)
```

#### Description:
```
Convert <ac> to a string (note that its keys must be increasing integers
starting from 0, and its values must be characters).
```

-------------------------------------------------------------------------------
### `ac->vector`

#### Signatures:
```scheme
(ac->vector <associative-collection>)
```

#### Description:
```
Convert <ac> to a vector (note that its keys must be increasing integers
starting from 0).
```

-------------------------------------------------------------------------------
### `associative-collection?`

#### Signatures:
```scheme
(associative-collection? <obj>)
```

#### Description:
```
Returns whether <obj> is an associative collection:
    String | List | Vector | Hashmap
```

-------------------------------------------------------------------------------
### `any?`

#### Signatures:
```scheme
(any? <predicate?-callable> <associative-collection> ...)
```

#### Description:
```
Returns whether any of the <ac>s satisfy <predicate?-callable>.
```

-------------------------------------------------------------------------------
### `append`

#### Signatures:
```scheme
(append)
(append <obj>)
(append <symbol> ...)
(append <keyword> ...)
(append <list> ... <obj>)
(append <associative-collection> ...)
```

#### Description:
```
Aliased by <+>. Given nothing, returns NIL. Given '<obj>', returns <obj>.

Given '<symbol> ...':
  Returns a new symbol of '<symbol> ...' concatenated with one another.

Given '<keyword> ...':
  Returns a new keyword of '<keyword> ...' concatenated with one another.

Given '<ac> ...':
  Creates a new associative collection by appending '<ac> ...' together.

    => Note that the '<ac> ...' values will have their types unified according
       to the following hierarchy: String < List < Vector < Hashmap
```

-------------------------------------------------------------------------------
### `conj`

#### Signatures:
```scheme
(conj <value> <associative-collection>)
(conj <key> <value> <associative-collection>)
```

#### Description:
```
Given 3 args:
  Returns a copy of <ac> with <val> associated to <key>.
Given 2 args:
  Returns a copy of <oc> with <val> added as efficiently as possible.
  <val>'s position will depend on <ac>'s specific collection type.
Note:
  <ac> denotes an associative-collection
  <oc> denotes an ordered-collection
```

-------------------------------------------------------------------------------
### `count`

#### Signatures:
```scheme
(count <predicate?-callable> <associative-collection>)
```

#### Description:
```
Count the number of times <predicate?-callable> is satisfied in <ac>.
```

-------------------------------------------------------------------------------
### `delete`

#### Signatures:
```scheme
(delete <associative-collection> <key>)
```

#### Description:
```
Return a copy of <ac> without <key>'s association.
```

-------------------------------------------------------------------------------
### `difference`

#### Signatures:
```scheme
(difference <elt=?-callable> <associative-collection> ...)
```

#### Description:
```
Returns the set difference of values (compared by <elt=?>) in "<ac> ...".

  => Note that the "<ac> ..." values will have their types unified according
     to the following hierarchy: String < List < Vector < Hashmap
```

-------------------------------------------------------------------------------
### `drop`

#### Signatures:
```scheme
(drop <associative-collection> <length>)
```

#### Description:
```
Returns <ac> with <length> items dropped from its left side.
```

-------------------------------------------------------------------------------
### `empty?`

#### Signatures:
```scheme
(empty? <associative-collection>)
```

#### Description:
```
Returns whether <ac> is empty.
```

-------------------------------------------------------------------------------
### `every?`

#### Signatures:
```scheme
(every? <predicate?-callable> <associative-collection> ...)
```

#### Description:
```
Returns whether every one of the <ac>s satisfy <predicate?-callable>.
```

-------------------------------------------------------------------------------
### `filter`

#### Signatures:
```scheme
(filter <keep?-callable> <associative-collection>)
```

#### Description:
```
Creates a new associative collection by filtering items in <ac> that don't
satisfy the <keep?-callable> callable.
```

-------------------------------------------------------------------------------
### `fold`

#### Signatures:
```scheme
(fold <callable> <seed> <associative-collection> ...)
```

#### Description:
```
Accumulate the values in "<ac> ..." from left to right by applying
<callable> to "<previous-result>" and <item> with <seed-obj> acting
as the initial "<previous-result>".

  => Note that the "<ac> ..." values will have their types unified according
     to the following hierarchy: String < List < Vector < Hashmap
```

-------------------------------------------------------------------------------
### `for-each`

#### Signatures:
```scheme
(for-each <callable> <associative-collection> ...)
```

#### Description:
```
Applies <callable> to each item in "<ac> ...".

  => Note that the "<ac> ..." values will have their types unified according
     to the following hierarchy: String < List < Vector < Hashmap
```

-------------------------------------------------------------------------------
### `head`

#### Signatures:
```scheme
(head <associative-collection>)
```

#### Description:
```
Get the first item in <ac>.
```

-------------------------------------------------------------------------------
### `intersection`

#### Signatures:
```scheme
(intersection <elt=?-callable> <associative-collection> ...)
```

#### Description:
```
Returns the intersection of values (compared by <elt=?>) between "<ac> ...".

  => Note that the "<ac> ..." values will have their types unified according
     to the following hierarchy: String < List < Vector < Hashmap
```

-------------------------------------------------------------------------------
### `key`

#### Signatures:
```scheme
(key <predicate?-callable> <associative-collection>)
```

#### Description:
```
Get the first key in <ac> who's associated value satisfies <predicate?-callable>.
```

-------------------------------------------------------------------------------
### `length`

#### Signatures:
```scheme
(length <associative-collection>)
```

#### Description:
```
Returns <ac>'s length. Aliased by <len>.
```

-------------------------------------------------------------------------------
### `length+`

#### Signatures:
```scheme
(length+ <associative-collection>)
```

#### Description:
```
Returns <ac>'s length. If <ac> is a dotted-list, returns #f.
This is instead of triggering an error, as <length> would.
```

-------------------------------------------------------------------------------
### `map`

#### Signatures:
```scheme
(map <callable> <associative-collection> ...)
```

#### Description:
```
Creates a new associative collection by applying <callable> to each item in
"<ac> ...".

  => Note that the "<ac> ..." values will have their types unified according
     to the following hierarchy: String < List < Vector < Hashmap
```

-------------------------------------------------------------------------------
### `remove`

#### Signatures:
```scheme
(remove <predicate?-callable> <associative-collection>)
```

#### Description:
```
Creates a new associative collection by removing items in <ac> that
satisfy the <predicate?-callable> callable.
```

-------------------------------------------------------------------------------
### `symmetric-difference`

#### Signatures:
```scheme
(symmetric-difference <elt=?-callable> <associative-collection> ...)
```

#### Description:
```
Returns the set symmetric-difference of values (compared by <elt=?>) in
"<ac> ...". SymDiff(a,b) = Union(Diff(a,b),Diff(b,a))

  => Note that the "<ac> ..." values will have their types unified according
     to the following hierarchy: String < List < Vector < Hashmap
```

-------------------------------------------------------------------------------
### `tail`

#### Signatures:
```scheme
(tail <associative-collection>)
```

#### Description:
```
Get everything after the first item in <ac>.
```

-------------------------------------------------------------------------------
### `take`

#### Signatures:
```scheme
(take <associative-collection> <length>)
```

#### Description:
```
Returns <length> items taken from the left of <ac>.
```

-------------------------------------------------------------------------------
### `union`

#### Signatures:
```scheme
(union <elt=?-callable> <associative-collection> ...)
```

#### Description:
```
Returns the set union of values (compared by <elt=?>) in "<ac> ...".

  => Note that the "<ac> ..." values will have their types unified according
     to the following hierarchy: String < List < Vector < Hashmap
```

-------------------------------------------------------------------------------
### `val`

#### Signatures:
```scheme
(val <associative-collection> <key>)
```

#### Description:
```
Get the value in <ac> associated to <key>.
```

-------------------------------------------------------------------------------
## Booleans


-------------------------------------------------------------------------------
### `boolean?`

#### Signatures:
```scheme
(boolean? <obj>)
```

#### Description:
```
Returns whether <obj> is a boolean value (#t or #f).
```

-------------------------------------------------------------------------------
### `not`

#### Signatures:
```scheme
(not <obj>)
```

#### Description:
```
Returns #t if <obj> is #f, otherwise #t.
```

-------------------------------------------------------------------------------
## Characters


-------------------------------------------------------------------------------
### `ascii-char?`

#### Signatures:
```scheme
(ascii-char? <char>)
```

#### Description:
```
Returns whether <char> is an 8bit ascii char, correlating to exactly 1 char
within strings under the hood. Note that all EScheme string operations are
codepoint-relative though! Hence a string with a single surrogate java
char pair in it will have a <length> of 1.

See <char-pair?> to determine if a <char> is a 32bit unicode value, and
see <java-char?> to determine if a <char> is a 16bit unicode value.
```

-------------------------------------------------------------------------------
### `char-alphabetic?`

#### Signatures:
```scheme
(char-alphabetic? <char>)
```

#### Description:
```
Returns whether <char> is alphabetic.
```

-------------------------------------------------------------------------------
### `char-alphanumeric?`

#### Signatures:
```scheme
(char-alphanumeric? <char>)
```

#### Description:
```
Returns whether <char> is alphabetic or numeric.
```

-------------------------------------------------------------------------------
### `char-ci<=?`

#### Signatures:
```scheme
(char-ci<=? <char> ...)
```

#### Description:
```
Returns whether "<char> ..." are less than or equal to one another
(case-insensitive).
```

-------------------------------------------------------------------------------
### `char-ci<?`

#### Signatures:
```scheme
(char-ci<? <char> ...)
```

#### Description:
```
Returns whether "<char> ..." are less than one another (case-insensitive).
```

-------------------------------------------------------------------------------
### `char-ci=?`

#### Signatures:
```scheme
(char-ci=? <char> ...)
```

#### Description:
```
Returns whether "<char> ..." are equal to one another (case-insensitive).
```

-------------------------------------------------------------------------------
### `char-ci>=?`

#### Signatures:
```scheme
(char-ci>=? <char> ...)
```

#### Description:
```
Returns whether "<char> ..." are greater than or equal to one another
(case-insensitive).
```

-------------------------------------------------------------------------------
### `char-ci>?`

#### Signatures:
```scheme
(char-ci>? <char> ...)
```

#### Description:
```
Returns whether "<char> ..." are greater than one another (case-insensitive).
```

-------------------------------------------------------------------------------
### `char-codepoint`

#### Signatures:
```scheme
(char-codepoint <high-char> <low-char>)
```

#### Description:
```
Returns a new character formed by combining <high-char> & <low-char> into
a single 32bit surrogate char pair.
Returns #f if (not (and (char-high? <high-char>) (char-low? <low-char>)))
```

-------------------------------------------------------------------------------
### `char-control?`

#### Signatures:
```scheme
(char-control? <char>)
```

#### Description:
```
Returns whether <char> is a control character.
```

-------------------------------------------------------------------------------
### `char-count`

#### Signatures:
```scheme
(char-count <char>)
```

#### Description:
```
Returns the number of java chars <char> stringifies to.
Returns 2 if (char-pair? <char>), else returns 1.
```

-------------------------------------------------------------------------------
### `char-defined?`

#### Signatures:
```scheme
(char-defined? <char>)
```

#### Description:
```
Returns whether <char> is an unassigned unicode value.
```

-------------------------------------------------------------------------------
### `char-digit`

#### Signatures:
```scheme
(char-digit <char>)
(char-digit <char> <radix>)
```

#### Description:
```
Returns the digit represented by <char> in <radix> (defaults to 36).
Returns #f if <radix> is an invalid non-negative int or <char> isn't a digit.
```

-------------------------------------------------------------------------------
### `char-downcase`

#### Signatures:
```scheme
(char-downcase <char>)
```

#### Description:
```
Returns the upper-case version of <char>.
```

-------------------------------------------------------------------------------
### `char-for-digit`

#### Signatures:
```scheme
(char-for-digit <non-negative-integer>)
(char-for-digit <non-negative-integer> <radix>)
```

#### Description:
```
Returns the char representing digit <integer> in <radix> (defaults to 36).
Returns #f if <radix> is an invalid non-negative int or <integer> isn't a digit.
```

-------------------------------------------------------------------------------
### `char-graph?`

#### Signatures:
```scheme
(char-graph? <char>)
```

#### Description:
```
Returns whether <char> is a graph character, equivalent to:
  (or (char-alphanumeric? <char>) (char-punctuation? <char>))
```

-------------------------------------------------------------------------------
### `char-high`

#### Signatures:
```scheme
(char-high <char>)
```

#### Description:
```
Returns the high portion of <char> if it's a 32 surrogate pair.
If (java-char? <char>), returns #f.
```

-------------------------------------------------------------------------------
### `char-high?`

#### Signatures:
```scheme
(char-high? <char>)
```

#### Description:
```
Returns whether <char> is the high portion of a 32bit surrogate char pair.
```

-------------------------------------------------------------------------------
### `char-low`

#### Signatures:
```scheme
(char-low <char>)
```

#### Description:
```
Returns the low portion of <char> if it's a 32 surrogate pair.
If (java-char? <char>), returns #f.
```

-------------------------------------------------------------------------------
### `char-low?`

#### Signatures:
```scheme
(char-low? <char>)
```

#### Description:
```
Returns whether <char> is the low portion of a 32bit surrogate char pair.
```

-------------------------------------------------------------------------------
### `char-lower-case?`

#### Signatures:
```scheme
(char-lower-case? <char>)
```

#### Description:
```
Returns whether <char> is lowercase.
```

-------------------------------------------------------------------------------
### `char-name`

#### Signatures:
```scheme
(char-name <char>)
```

#### Description:
```
Returns <char>'s name as a string, or #f if its an unassigned unicode value.
```

-------------------------------------------------------------------------------
### `char-numeric?`

#### Signatures:
```scheme
(char-numeric? <char>)
```

#### Description:
```
Returns whether <char> is numeric.
```

-------------------------------------------------------------------------------
### `char-pair?`

#### Signatures:
```scheme
(char-pair? <char>)
```

#### Description:
```
Returns whether <char> is a 32bit surrogate java char pair. Used to represent
32bit unicode values, such codepoints become 2 java chars once stringified.
Note that all EScheme string operations are codepoint-relative though! Hence a
string with a single surrogate java char pair in it will have a <length> of 1.

See <java-char?> to determine if a <char> is a 16bit unicode value, and
see <ascii-char?> to determine if a <char> is an ASCII value.
```

-------------------------------------------------------------------------------
### `char-print?`

#### Signatures:
```scheme
(char-print? <char>)
```

#### Description:
```
Returns whether <char> is printable, equivalent to:
  (or (char-graph? <char>) (eq? #\space <char>))
```

-------------------------------------------------------------------------------
### `char-punctuation?`

#### Signatures:
```scheme
(char-punctuation? <char>)
```

#### Description:
```
Returns whether <char> is a punctuation character.
```

-------------------------------------------------------------------------------
### `char-upcase`

#### Signatures:
```scheme
(char-upcase <char>)
```

#### Description:
```
Returns the upper-case version of <char>.
```

-------------------------------------------------------------------------------
### `char-upper-case?`

#### Signatures:
```scheme
(char-upper-case? <char>)
```

#### Description:
```
Returns whether <char> is uppercase.
```

-------------------------------------------------------------------------------
### `char-whitespace?`

#### Signatures:
```scheme
(char-whitespace? <char>)
```

#### Description:
```
Returns whether <char> is whitespace.
```

-------------------------------------------------------------------------------
### `char-xdigit?`

#### Signatures:
```scheme
(char-xdigit? <char>)
```

#### Description:
```
Returns whether <char> is a hexadecimal digit.
```

-------------------------------------------------------------------------------
### `char<=?`

#### Signatures:
```scheme
(char<=? <char> ...)
```

#### Description:
```
Returns whether "<char> ..." are less than or equal to one another.
```

-------------------------------------------------------------------------------
### `char<?`

#### Signatures:
```scheme
(char<? <char> ...)
```

#### Description:
```
Returns whether "<char> ..." are less than one another.
```

-------------------------------------------------------------------------------
### `char=?`

#### Signatures:
```scheme
(char=? <char> ...)
```

#### Description:
```
Returns whether "<char> ..." are equal to one another.
```

-------------------------------------------------------------------------------
### `char>=?`

#### Signatures:
```scheme
(char>=? <char> ...)
```

#### Description:
```
Returns whether "<char> ..." are greater than or equal to one another.
```

-------------------------------------------------------------------------------
### `char>?`

#### Signatures:
```scheme
(char>? <char> ...)
```

#### Description:
```
Returns whether "<char> ..." are greater than one another.
```

-------------------------------------------------------------------------------
### `char?`

#### Signatures:
```scheme
(char? <obj>)
```

#### Description:
```
Returns whether <obj> is a character value.
```

-------------------------------------------------------------------------------
### `java-char?`

#### Signatures:
```scheme
(java-char? <char>)
```

#### Description:
```
Returns whether <char> is a 16bit java char, correlating to exactly 1 char
within strings under the hood. Note that all EScheme string operations are
codepoint-relative though! Hence a string with a single surrogate java
char pair in it will have a <length> of 1.

See <char-pair?> to determine if a <char> is a 32bit unicode value, and
see <ascii-char?> to determine if a <char> is an ASCII value.
```

-------------------------------------------------------------------------------
## Concurrency


-------------------------------------------------------------------------------
### Mutexes


-------------------------------------------------------------------------------
#### `mutex`

##### Signatures:
```scheme
(mutex)
(mutex <name-string>)
```

##### Description:
```
Create a new reentrant-lock mutex. Returns <name-str> (defaults to a random string)
if passed to <mutex-name>.
```

-------------------------------------------------------------------------------
#### `mutex-held?`

##### Signatures:
```scheme
(mutex-held? <mutex>)
```

##### Description:
```
Returns whether <mutex> is held by the current thread.
```

-------------------------------------------------------------------------------
#### `mutex-hold-count`

##### Signatures:
```scheme
(mutex-hold-count <mutex>)
```

##### Description:
```
Get the total number of holds on <mutex> by the current thread.
```

-------------------------------------------------------------------------------
#### `mutex-lock!`

##### Signatures:
```scheme
(mutex-lock! <mutex>)
(mutex-lock! <mutex> <milliseconds-timeout>)
```

##### Description:
```
Returns whether managed to acquired the lock prior to <millisecond-timeout>
(defaults to Infinity).
```

-------------------------------------------------------------------------------
#### `mutex-locked?`

##### Signatures:
```scheme
(mutex-locked? <mutex>)
```

##### Description:
```
Returns whether <mutex> is locked by any thread.
```

-------------------------------------------------------------------------------
#### `mutex-name`

##### Signatures:
```scheme
(mutex-name <mutex>)
```

##### Description:
```
Returns <mutex>'s name.
```

-------------------------------------------------------------------------------
#### `mutex-queue-length`

##### Signatures:
```scheme
(mutex-queue-length <mutex>)
```

##### Description:
```
Returns the total number of threads waiting to acquire <mutex>.
```

-------------------------------------------------------------------------------
#### `mutex-queued?`

##### Signatures:
```scheme
(mutex-queued? <mutex>)
```

##### Description:
```
Returns whether any thread is waiting for <mutex>.
```

-------------------------------------------------------------------------------
#### `mutex-set-specific!`

##### Signatures:
```scheme
(mutex-set-specific! <mutex> <obj>)
```

##### Description:
```
Set the variable value "specifically associated" with <mutex> to <obj>.
```

-------------------------------------------------------------------------------
#### `mutex-specific`

##### Signatures:
```scheme
(mutex-specific <mutex>)
```

##### Description:
```
Get the variable value "specifically associated" with <mutex>.
```

-------------------------------------------------------------------------------
#### `mutex-unlock!`

##### Signatures:
```scheme
(mutex-unlock! <mutex>)
```

##### Description:
```
Unlocks <mutex>. Returns #f if the current thread wasn't locking <mutex>.
```

-------------------------------------------------------------------------------
#### `mutex?`

##### Signatures:
```scheme
(mutex? <obj>)
```

##### Description:
```
Returns whether <obj> is a mutex.
```

-------------------------------------------------------------------------------
### Promises


-------------------------------------------------------------------------------
#### `await`

##### Signatures:
```scheme
(await <promise> <resolve-lambda> <reject-lambda>)
(await <timeout-ms> <promise> <resolve-lambda> <reject-lambda>)
```

##### Description:
```
Pause the current thread's execution until <promise> either resolves or 
rejects. If it rejects, the value is passed to <reject-lambda>. Otherwise,
the value is passed to <resolve-lambda>.

If <timeout-ms> is provided, <await> will wait for the promise's thread to
complete at most <timeout> milliseconds, before <reject>ing with the 'timeout
symbol.

Note that it's safe to pass non-promises to <await>: they're treated as 
values of immediately resolved promises.

<await> conceptually mirrors JavaScript's <then>/<catch> promise methods.

See <promise>'s <help> entry for an example use!
```

-------------------------------------------------------------------------------
#### `await-all`

##### Signatures:
```scheme
(await-all <promises-ordered-collection>)
(await-all <promise> ...)
```

##### Description:
```
Get a new promise that either resolves to a list of the <promise>s resolved
results, or rejects with the first error encountered. Akin to JavaScript's
<Promise.all()>.

<await-all> can be called in two ways:
  1. (await-all <promises-ordered-collection>)
  2. (await-all <promise> ...)

Note that it's safe to pass non-promises to <await-all>: they're treated as 
values of immediately resolved promises.

See <promise>'s <help> entry for an example use!
```

-------------------------------------------------------------------------------
#### `promise`

##### Signatures:
```scheme
(promise <promise-lambda>)
```

##### Description:
```
EScheme's implementation of JavaScript's <Promise> concurrency paradigm.

Returns a <promise-handle> value to <await> the promise with later on.

<promise-lambda> should accept two unary callables as its arguments: the 
<resolve> and <reject> procedures.
  * Call <resolve> with the success value of the finished promise.
  * Call <reject> with an error value for the promise.
  * Note that if neither's called, <promise-lambda>'s final value resolves.

Example use:

  (define (test resolve?)
    (promise
      (lambda (resolve reject)
        (if resolve? 
            (resolve 'success!)
            (reject 'failed!)))))

  (define (success result) result)
  (define (failure error) error)

  ; Prints 'success!
  (println (await (test #t) success failure))

  ; Prints 'failed!
  (println (await (test #f) success failure))

  ; Prints '(success! success! success!)
  (define all-promises (await-all (test #t) (test #t) (test #t)))
  (println (await all-promises success failure))

  ; Prints 'failed!
  (define all-promises (await-all (test #t) (test #f) (test #t)))
  (println (await all-promises success failure))
```

-------------------------------------------------------------------------------
#### `promise?`

##### Signatures:
```scheme
(promise? <obj>)
```

##### Description:
```
Determine if <obj> came from <promise>.
```

-------------------------------------------------------------------------------
### Threads


-------------------------------------------------------------------------------
#### `current-thread`

##### Signatures:
```scheme
(current-thread)
```

##### Description:
```
Returns the current thread.
```

-------------------------------------------------------------------------------
#### `interrupted?!`

##### Signatures:
```scheme
(interrupted?!)
```

##### Description:
```
Returns whether the current thread has been interrupted.
Also clears its "interrupted?" flag afterwards.
```

-------------------------------------------------------------------------------
#### `parallel`

##### Signatures:
```scheme
(parallel <callable-thunk> ...)
```

##### Description:
```
Run the given "<callable-thunk> ..." items in parallel.
```

-------------------------------------------------------------------------------
#### `sleep`

##### Signatures:
```scheme
(sleep <millisecond-integer>)
```

##### Description:
```
Has the current thread sleep for <ms-to-sleep> milliseconds.
Returns whether was interrupted.
```

-------------------------------------------------------------------------------
#### `thread`

##### Signatures:
```scheme
(thread <thunk-callable>)
(thread <name-string> <thunk-callable>)
```

##### Description:
```
Create a new thread that invokes <callable-thunk> upon being passed to
<thread-start!>. Returns <name-string> (defaults to a random string) if
passed to <thread-name>.
```

-------------------------------------------------------------------------------
#### `thread-daemon?`

##### Signatures:
```scheme
(thread-daemon? <thread>)
```

##### Description:
```
Returns whether <thread> is a JVM daemon thread.
```

-------------------------------------------------------------------------------
#### `thread-define'`

##### Signatures:
```scheme
(thread-define' <symbol> <value>)
(thread-define' <thread> <symbol> <value>)
```

##### Description:
```
Bind <symbolic-variable-name> to <value> in <thread>'s (defaults to the
"meta-thread") dynamic environment (effectively a thread-local global
environment).

Use the <thread-define> macro to pass <symbolic-variable-name> as a literal.

Note that the "meta-thread" is a pseudo-thread accessable by all threads:
  Thread dynamic environments "inherit" value bindings from the
  "meta-thread" by caching a copy of them upon reference.
```

-------------------------------------------------------------------------------
#### `thread-defined?'`

##### Signatures:
```scheme
(thread-defined?' <symbol>)
(thread-defined?' <thread> <symbol>)
```

##### Description:
```
Return whether <symbolic-variable-name> is defined in <thread>'s (defaults to the
"meta-thread") dynamic environment (effectively a thread-local global
environment).

Use the <thread-get> macro to pass <symbolic-variable-name> as a literal.

Note that the "meta-thread" is a pseudo-thread accessable by all threads:
  Thread dynamic environments "inherit" value bindings from the
  "meta-thread" by caching a copy of them upon reference.
```

-------------------------------------------------------------------------------
#### `thread-dynamic-environment`

##### Signatures:
```scheme
(thread-dynamic-environment)
(thread-dynamic-environment <thread>)
```

##### Description:
```
Return an associative list of the variables (and their values!) defined
in <thread>'s (defaults to the "meta-thread") dynamic environment.

Note that the "meta-thread" is a pseudo-thread accessable by all threads:
  Thread dynamic environments "inherit" value bindings from the
  "meta-thread" by caching a copy of them upon reference.
```

-------------------------------------------------------------------------------
#### `thread-get'`

##### Signatures:
```scheme
(thread-get' <symbol>)
(thread-get' <thread> <symbol>)
```

##### Description:
```
Get <symbolic-variable-name>'s value in <thread>'s (defaults to the
"meta-thread") dynamic environment (effectively a thread-local global
environment).

Use the <thread-get> macro to pass <symbolic-variable-name> as a literal.

Note that the "meta-thread" is a pseudo-thread accessable by all threads:
  Thread dynamic environments "inherit" value bindings from the
  "meta-thread" by caching a copy of them upon reference.
```

-------------------------------------------------------------------------------
#### `thread-id`

##### Signatures:
```scheme
(thread-id <thread>)
```

##### Description:
```
Returns <thread>'s unique id.
```

-------------------------------------------------------------------------------
#### `thread-interrupt!`

##### Signatures:
```scheme
(thread-interrupt! <thread>)
```

##### Description:
```
Tries to interrupt <thread>, and returns whether succeeded.
```

-------------------------------------------------------------------------------
#### `thread-interrupted?`

##### Signatures:
```scheme
(thread-interrupted? <thread>)
```

##### Description:
```
Returns whether <thread> was interrupted.
```

-------------------------------------------------------------------------------
#### `thread-join!`

##### Signatures:
```scheme
(thread-join! <thread>)
(thread-join! <thread> <milliseconds-timeout>)
```

##### Description:
```
Waits for <thread> to join. If given <max-ms-to-wait>, waits that
many milliseconds prior returning control to the calling thread.
Returns whether <thread> was interrupted.
```

-------------------------------------------------------------------------------
#### `thread-name`

##### Signatures:
```scheme
(thread-name <thread>)
```

##### Description:
```
Returns <thread>'s name.
```

-------------------------------------------------------------------------------
#### `thread-priority`

##### Signatures:
```scheme
(thread-priority <thread>)
```

##### Description:
```
Returns <thread>'s priority, between <*min-priority*> and <*max-priority*>.
```

-------------------------------------------------------------------------------
#### `thread-runnable`

##### Signatures:
```scheme
(thread-runnable <thread>)
```

##### Description:
```
Returns <thread>'s callable thunk runnable.
```

-------------------------------------------------------------------------------
#### `thread-set!'`

##### Signatures:
```scheme
(thread-set!' <symbol> <value>)
(thread-set!' <thread> <symbol> <value>)
```

##### Description:
```
Set <symbolic-variable-name> to <value> in <thread>'s (defaults to the
"meta-thread") dynamic environment (effectively a thread-local global
environment).

Use the <thread-set!> macro to pass <symbolic-variable-name> as a literal.

Note that the "meta-thread" is a pseudo-thread accessable by all threads:
  Thread dynamic environments "inherit" value bindings from the
  "meta-thread" by caching a copy of them upon reference.
```

-------------------------------------------------------------------------------
#### `thread-set-daemon!`

##### Signatures:
```scheme
(thread-set-daemon! <thread> <status-boolean>)
```

##### Description:
```
Tries to set <thread> to be a JVM daemon thread or not, based on
<boolean-status>. Returns whether succeeded.
```

-------------------------------------------------------------------------------
#### `thread-set-priority!`

##### Signatures:
```scheme
(thread-set-priority! <thread> <priority-integer>)
```

##### Description:
```
Tries to set <thread>'s priority to be <int-priority>, which must be between
<*min-priority*> and <*max-priority*>. Returns whether succeeded.
```

-------------------------------------------------------------------------------
#### `thread-start!`

##### Signatures:
```scheme
(thread-start! <thread> ...)
```

##### Description:
```
Starts "<thread> ..." and invokes their callable thunk runnables.
Returns a list of threads that failed to start (e.g. already started).
```

-------------------------------------------------------------------------------
#### `thread-status`

##### Signatures:
```scheme
(thread-status <thread>)
```

##### Description:
```
Returns <thread>'s status as a symbolic name:
  'ready | 'running | 'blocked | 'waiting | 'timed-waiting | 'finished
```

-------------------------------------------------------------------------------
#### `thread-yield`

##### Signatures:
```scheme
(thread-yield)
```

##### Description:
```
Hints that the runtime may temporarily pause this thread if needed.
```

-------------------------------------------------------------------------------
#### `thread?`

##### Signatures:
```scheme
(thread? <obj>)
```

##### Description:
```
Returns whether <obj> is a thread.
```

-------------------------------------------------------------------------------
## Date-Time


-------------------------------------------------------------------------------
### `current-date`

#### Signatures:
```scheme
(current-date)
```

#### Description:
```
Returns a list of the current date components:
  (<year> <month> <day>)
```

-------------------------------------------------------------------------------
### `current-time`

#### Signatures:
```scheme
(current-time)
```

#### Description:
```
Returns a list of the current time components:
  (<hour> <minute> <second> <millisecond>)
```

-------------------------------------------------------------------------------
### `day`

#### Signatures:
```scheme
(day)
```

#### Description:
```
Returns the name of the current week-day as a string (#f if unknown).
```

-------------------------------------------------------------------------------
### `epoch-time`

#### Signatures:
```scheme
(epoch-time)
```

#### Description:
```
Returns the number of milliseconds since the epoch (1 January 1970).
```

-------------------------------------------------------------------------------
### `month`

#### Signatures:
```scheme
(month)
```

#### Description:
```
Returns the name of the current month as a string (#f if unknown).
```

-------------------------------------------------------------------------------
### `time-zone`

#### Signatures:
```scheme
(time-zone)
```

#### Description:
```
Returns the name of the current time-zone as a string (#f if unknown).
```

-------------------------------------------------------------------------------
### `year`

#### Signatures:
```scheme
(year)
```

#### Description:
```
Returns the name of the current year as a string (#f if unknown).
```

-------------------------------------------------------------------------------
## Equality


-------------------------------------------------------------------------------
### `eq?`

#### Signatures:
```scheme
(eq? <obj> ...)
```

#### Description:
```
Determines shallow equality for objects that maintain state.
```

-------------------------------------------------------------------------------
### `equal?`

#### Signatures:
```scheme
(equal? <obj> ...)
```

#### Description:
```
Determines deep equality for objects.
Note that this will infinitely loop for cyclical structures!
```

-------------------------------------------------------------------------------
## Files


-------------------------------------------------------------------------------
### `absolute-path`

#### Signatures:
```scheme
(absolute-path <path-string>)
```

#### Description:
```
Returns the absolute path of <path-string>.
```

-------------------------------------------------------------------------------
### `absolute-path?`

#### Signatures:
```scheme
(absolute-path? <path-string>)
```

#### Description:
```
Returns whether <path-string> is an absolute path. Note that this is different
from <path?>: checks path structure, not whether it exists on the current
system. Effectively: (equal? <path-string> (absolute-path <path-string>))
```

-------------------------------------------------------------------------------
### `copy-file`

#### Signatures:
```scheme
(copy-file <old-path-string> <new-path-string>)
```

#### Description:
```
Copy <old-path-string> to <new-path-string>.
Triggers an error if <new-path-string> already exists, or if
any of the intermediate directories in <new-path-string> don't exist.
```

-------------------------------------------------------------------------------
### `copy-file!`

#### Signatures:
```scheme
(copy-file! <old-path-string> <new-path-string>)
```

#### Description:
```
Copy <old-path-string> to <new-path-string>.
Replaces <new-path-string> if it already exists, and creates
any intermediate directories in <new-path-string> as needed.
Returns whether succeeded.
```

-------------------------------------------------------------------------------
### `current-directory`

#### Signatures:
```scheme
(current-directory)
```

#### Description:
```
Get the current working directory's absolute path as a string.

Note that <current-directory> refers to the directory that launched
the current process. Use #path if you instead want to refer to the
directory of the current file.
```

-------------------------------------------------------------------------------
### `directory-delete!`

#### Signatures:
```scheme
(directory-delete! <directory-path-string>)
```

#### Description:
```
Deletes <directory-path-string> if its empty. Returns the deletion's success status.
```

-------------------------------------------------------------------------------
### `directory-entries`

#### Signatures:
```scheme
(directory-entries <directory-path-string>)
```

#### Description:
```
Returns a list of the directory entry paths in <directory-path-string>.
Returns "#f" if <directory-path-string> doesn't denote a directory.
```

-------------------------------------------------------------------------------
### `directory-entries*`

#### Signatures:
```scheme
(directory-entries* <directory-path-string>)
```

#### Description:
```
Returns a list of the directory entry paths in <directory-path-string>, WITHOUT dot-files.
Note that "dot-file" here refers to file names starting with ".".
Returns "#f" if <directory-path-string> doesn't denote a directory.
```

-------------------------------------------------------------------------------
### `directory-recursive-delete!`

#### Signatures:
```scheme
(directory-recursive-delete! <directory-path-string>)
```

#### Description:
```
Deletes <directory-path-string> and its contents. Returns the deletion's success status.
```

-------------------------------------------------------------------------------
### `directory?`

#### Signatures:
```scheme
(directory? <directory-path-string>)
```

#### Description:
```
Returns whether <directory-path-string> is a directory path.
```

-------------------------------------------------------------------------------
### `file-delete!`

#### Signatures:
```scheme
(file-delete! <file-path-string>)
```

#### Description:
```
Deletes <file-path-string>. Returns the deletion's success status.
```

-------------------------------------------------------------------------------
### `file-display`

#### Signatures:
```scheme
(file-display <filename-string> <obj>)
```

#### Description:
```
Write <obj> to <filename-string> using human-readable syntax.
Note that this will infinitely loop for cyclical structures!
```

-------------------------------------------------------------------------------
### `file-display+`

#### Signatures:
```scheme
(file-display+ <filename-string> <obj>)
```

#### Description:
```
Append <obj> to <filename-string> using human-readable syntax.
Note that this will infinitely loop for cyclical structures!
```

-------------------------------------------------------------------------------
### `file-extension`

#### Signatures:
```scheme
(file-extension <path-string>)
```

#### Description:
```
Get the file extension of <path-string>. Returns #f if non-existant.
```

-------------------------------------------------------------------------------
### `file-has-extension?`

#### Signatures:
```scheme
(file-has-extension? <path-string> <extension-string>)
```

#### Description:
```
Returns whether <path-string> has <extension-string> as its file extension.
```

-------------------------------------------------------------------------------
### `file-pretty-print`

#### Signatures:
```scheme
(file-pretty-print <filename-string> <obj>)
```

#### Description:
```
Write <obj> to <filename-string> using indented machine-readable syntax.
Note that this will infinitely loop for cyclical structures!
```

-------------------------------------------------------------------------------
### `file-pretty-print+`

#### Signatures:
```scheme
(file-pretty-print+ <filename-string> <obj>)
```

#### Description:
```
Append <obj> to <filename-string> using indented machine-readable syntax.
Note that this will infinitely loop for cyclical structures!
```

-------------------------------------------------------------------------------
### `file-read`

#### Signatures:
```scheme
(file-read <filename-string>)
```

#### Description:
```
Get the contents of <filename-string> as an EScheme data structure.
```

-------------------------------------------------------------------------------
### `file-read-string`

#### Signatures:
```scheme
(file-read-string <filename-string>)
```

#### Description:
```
Get the contents of <filename-string> as a string.
```

-------------------------------------------------------------------------------
### `file-size`

#### Signatures:
```scheme
(file-size <file-path-string>)
```

#### Description:
```
Return the size of <file-path-string> in bytes.
Platform-dependant result if <file-path-string> is a directory.
```

-------------------------------------------------------------------------------
### `file-write`

#### Signatures:
```scheme
(file-write <filename-string> <obj>)
```

#### Description:
```
Write <obj> to <filename-string> using machine-readable syntax.
Note that this will infinitely loop for cyclical structures!
```

-------------------------------------------------------------------------------
### `file-write+`

#### Signatures:
```scheme
(file-write+ <filename-string> <obj>)
```

#### Description:
```
Append <obj> to <filename-string> using machine-readable syntax.
Note that this will infinitely loop for cyclical structures!
```

-------------------------------------------------------------------------------
### `file?`

#### Signatures:
```scheme
(file? <file-path-string>)
```

#### Description:
```
Returns whether <file-path-string> is a file path.
```

-------------------------------------------------------------------------------
### `make-directory`

#### Signatures:
```scheme
(make-directory <path-string>)
```

#### Description:
```
Create <path-string> as a directory.
Fails if any directory midway in <path-string> doesn't exist.
Returns whether succeeded.
```

-------------------------------------------------------------------------------
### `make-directory!`

#### Signatures:
```scheme
(make-directory! <path-string>)
```

#### Description:
```
Create <path-string> as a directory.
Creates intermediate directories as needed while creating <path-string>.
Returns whether succeeded.
```

-------------------------------------------------------------------------------
### `move-file`

#### Signatures:
```scheme
(move-file <old-path-string> <new-path-string>)
```

#### Description:
```
Move <old-path-string> to be at <new-path-string> instead.
Triggers an error if <new-path-string> already exists, or if
any of the intermediate directories in <new-path-string> don't exist.
```

-------------------------------------------------------------------------------
### `move-file!`

#### Signatures:
```scheme
(move-file! <old-path-string> <new-path-string>)
```

#### Description:
```
Move <old-path-string> to be at <new-path-string> instead.
Replaces <new-path-string> if it already exists, and creates
any intermediate directories in <new-path-string> as needed.
Returns whether succeeded.
```

-------------------------------------------------------------------------------
### `path`

#### Signatures:
```scheme
(path <string> ...)
```

#### Description:
```
Create a path by combining "<string> ..." with <*file-separator*> between each
component. Yields an absolute path. Passing no arguments is equivalent to
(current-directory).
```

-------------------------------------------------------------------------------
### `path-delete!`

#### Signatures:
```scheme
(path-delete! <path-string>)
```

#### Description:
```
Deletes <path-string> by dispatching to <file-delete!> or <directory-delete!>.
Returns the deletion's success status.
```

-------------------------------------------------------------------------------
### `path-file`

#### Signatures:
```scheme
(path-file <path-string>)
```

#### Description:
```
Get <path-string>'s file name. Returns #f if <path-string> is empty.
```

-------------------------------------------------------------------------------
### `path-parent`

#### Signatures:
```scheme
(path-parent <path-string>)
(path-parent <path-string> <positive-integer>)
```

#### Description:
```
Get <path-string>'s parent path. Returns #f if <path-string> doesn't have a
parent. <positive-integer> denotes how many parents in the chain to go up
(1 by default).
```

-------------------------------------------------------------------------------
### `path-recursive-delete!`

#### Signatures:
```scheme
(path-recursive-delete! <path-string>)
```

#### Description:
```
Deletes <path-string> by dispatching to <file-delete!> or <directory-recursive-delete!>.
Returns the deletion's success status.
```

-------------------------------------------------------------------------------
### `path?`

#### Signatures:
```scheme
(path? <path-string>)
```

#### Description:
```
Returns whether <path-string> is a path (directory or file) that exists on the
system.
```

-------------------------------------------------------------------------------
### `remove-file-extension`

#### Signatures:
```scheme
(remove-file-extension <path-string>)
```

#### Description:
```
Returns <path-string> without its extension, including the ".".
Returns <path-string> if it doesn't have an extension or is an invalid path.
```

-------------------------------------------------------------------------------
### `swap-file-extension`

#### Signatures:
```scheme
(swap-file-extension <path-string> <new-extension-string>)
```

#### Description:
```
Returns <path-string> with <new-extension-string> as its file extension.
```

-------------------------------------------------------------------------------
## Functional


-------------------------------------------------------------------------------
### `bind`

#### Signatures:
```scheme
(bind <callable> <arg> ...)
```

#### Description:
```
Create a new procedure by binding "<arg> ..." as arguments to <callable>.
Aliased by <+> if only given non-<associative-collection> callables.
```

-------------------------------------------------------------------------------
### `callable?`

#### Signatures:
```scheme
(callable? <obj>)
```

#### Description:
```
Returns whether <obj> is applicable. Equivalent to:
  (or (procedure? <obj>)
      (functor? <obj>)
      (class? <obj>)
      (string? <obj>)
      (vector? <obj>)
      (hashmap? <obj>))
```

-------------------------------------------------------------------------------
### `compose`

#### Signatures:
```scheme
(compose <callable> ...)
```

#### Description:
```
Create a new, variadic procedure that is the composition of "<callable> ...".
Aliased by <*> if only given callables.
```

-------------------------------------------------------------------------------
### `id`

#### Signatures:
```scheme
(id <obj>)
```

#### Description:
```
Returns <obj>. Useful for certain higher-level procedures.
```

-------------------------------------------------------------------------------
### `procedure?`

#### Signatures:
```scheme
(procedure? <obj>)
```

#### Description:
```
Returns whether <obj> is a procedure.
Prefer <callable?> for more generic code.
```

-------------------------------------------------------------------------------
## Hashmaps


-------------------------------------------------------------------------------
### `hashcode`

#### Signatures:
```scheme
(hashcode <obj> ...)
```

#### Description:
```
Returns the hashcode of "<obj> ..." combined with one another.
Unused by any of the other hashmap primitives, but hey, it
could be useful to expose to users.
```

-------------------------------------------------------------------------------
### `hashmap`

#### Signatures:
```scheme
(hashmap <key> <value> ...)
```

#### Description:
```
Construct a hashmap containing key-value associations of "<key> <value> ...".
Create hashmap literals via the {<key> <value> ...} syntax.
Hashmaps are applicable to a key to get their entry: (<hashmap> <key>)
```

-------------------------------------------------------------------------------
### `hashmap-delete!`

#### Signatures:
```scheme
(hashmap-delete! <hashmap> <key>)
```

#### Description:
```
Delete <key>'s association in <hashmap>. Returns whether succeeded.
```

-------------------------------------------------------------------------------
### `hashmap-key?`

#### Signatures:
```scheme
(hashmap-key? <hashmap> <obj>)
```

#### Description:
```
Returns whether <obj> is a key in <hashmap>.
```

-------------------------------------------------------------------------------
### `hashmap-keys`

#### Signatures:
```scheme
(hashmap-keys <hashmap>)
```

#### Description:
```
Get the keys of <hashmap> as a list.
```

-------------------------------------------------------------------------------
### `hashmap-merge`

#### Signatures:
```scheme
(hashmap-merge <hashmap> ...)
```

#### Description:
```
Returns a new <hashmap> combining all of the associations in "<hashmap> ...".
```

-------------------------------------------------------------------------------
### `hashmap-merge!`

#### Signatures:
```scheme
(hashmap-merge! <hashmap> ...)
```

#### Description:
```
Combine all of the associations in "<merged-hashmap> ..." into <hashmap>.
```

-------------------------------------------------------------------------------
### `hashmap-set!`

#### Signatures:
```scheme
(hashmap-set! <hashmap> <key> <obj>)
```

#### Description:
```
Associate <value> to <key> in <hashmap>. Returns whether replaced an existing
value. Note this can also be done by directly applying the hashmap to the key
and new value (like a function).
```

-------------------------------------------------------------------------------
### `hashmap-val?`

#### Signatures:
```scheme
(hashmap-val? <hashmap> <obj>)
```

#### Description:
```
Returns whether <obj> is a value in <hashmap>.
```

-------------------------------------------------------------------------------
### `hashmap-values`

#### Signatures:
```scheme
(hashmap-values <hashmap>)
```

#### Description:
```
Get the values of <hashmap> as a list.
```

-------------------------------------------------------------------------------
### `hashmap?`

#### Signatures:
```scheme
(hashmap? <obj>)
```

#### Description:
```
Returns whether <obj> is a hashmap.
```

-------------------------------------------------------------------------------
## IO


-------------------------------------------------------------------------------
### `display`

#### Signatures:
```scheme
(display <obj>)
(display <output-port> <obj>)
```

#### Description:
```
Print <obj> to <output-port> in human-readable form.
<output-port> defaults to (current-output-port).
Note that this will infinitely loop for cyclical structures!
Aliased by <print>.
```

-------------------------------------------------------------------------------
### `displayf`

#### Signatures:
```scheme
(displayf <format-string> <arg> ...)
(displayf <output-port> <format-string> <arg> ...)
```

#### Description:
```
Print formatted <format-string> with "<arg> ..." to <output-port> in
human-readable form. <output-port> defaults to (current-output-port).
Note that this will infinitely loop for cyclical structures!
Aliased by <printf>.
>> <format-string> is like Java's printf with unique formatting patterns:
   ----------------------------------------------------------------------
   %a = display anything
   %wa = write anything
   %pa = pretty-print anything
   ----------------------------------------------------------------------
   %... = display unpacked list/vector/hashmap
   %w... = write unpacked list/vector/hashmap
   %p... = pretty-print unpacked list/vector/hashmap
   ----------------------------------------------------------------------
   %n = number
   %+n = number (show sign if positive too)
   %,n = number with commas
   %En = %en = number (coerced to exact)
   %In = %in = number (coerced to inexact)
   %#rn = %#Rn = number (in radix <#> from 2 to 36)
   %#n = number (left-padded with 0s to a width of <#> characters)
   %.#n = number (with <#> digits of precision)
   -> IE "%+e2rn": make exact in binary with sign
   -> NOTE: 1) 0-padding & precision MUST be of 2 digits or less!
            2) Can't have radix with I-coercion or precision!
   ----------------------------------------------------------------------
   %$ = display real finite as a dollar value
   %,$ = display real finite as a dollar value seperated by commas
   ----------------------------------------------------------------------
   %s = display string
   %#s = display string & pad left with # spaces
   %-#s = display string & pad right with # spaces
   %ws = write string
   -> NOTE: padding MUST be of 3 digits or less (ie from -999 to 999)
   ----------------------------------------------------------------------
   %b  = bool
   %wb = write "true" or "false" instead of "#t" or "#f"
   ----------------------------------------------------------------------
   %%  = "%" (escapes a "%")
   ----------------------------------------------------------------------
```

-------------------------------------------------------------------------------
### `eof?`

#### Signatures:
```scheme
(eof? <obj>)
```

#### Description:
```
Returns whether <obj> is the #eof object.
```

-------------------------------------------------------------------------------
### `newline`

#### Signatures:
```scheme
(newline)
(newline <output-port>)
```

#### Description:
```
Print a newline to <output-port>. Equivalent to: (display "\n").
<output-port> defaults to (current-output-port).
```

-------------------------------------------------------------------------------
### `pretty-print`

#### Signatures:
```scheme
(pretty-print <obj>)
(pretty-print <output-port> <obj>)
```

#### Description:
```
Pretty-print <obj> to <output-port> in indented, machine-readable form.
<output-port> defaults to (current-output-port).
Aliased by "pprint".
Note that this will infinitely loop for cyclical structures!
```

-------------------------------------------------------------------------------
### `pretty-printf`

#### Signatures:
```scheme
(pretty-printf <format-string> <arg> ...)
(pretty-printf <output-port> <format-string> <arg> ...)
```

#### Description:
```
Pretty-print formatted <format-string> with "<arg> ..." to <output-port>
in indented, machine-readable form. <output-port> defaults to
(current-output-port).
Aliased by "pprintf".
Note that this will infinitely loop for cyclical structures!
>> <format-string> is like Java's printf with unique formatting patterns:
   ----------------------------------------------------------------------
   %a = display anything
   %wa = write anything
   %pa = pretty-print anything
   ----------------------------------------------------------------------
   %... = display unpacked list/vector/hashmap
   %w... = write unpacked list/vector/hashmap
   %p... = pretty-print unpacked list/vector/hashmap
   ----------------------------------------------------------------------
   %n = number
   %+n = number (show sign if positive too)
   %,n = number with commas
   %En = %en = number (coerced to exact)
   %In = %in = number (coerced to inexact)
   %#rn = %#Rn = number (in radix <#> from 2 to 36)
   %#n = number (left-padded with 0s to a width of <#> characters)
   %.#n = number (with <#> digits of precision)
   -> IE "%+e2rn": make exact in binary with sign
   -> NOTE: 1) 0-padding & precision MUST be of 2 digits or less!
            2) Can't have radix with I-coercion or precision!
   ----------------------------------------------------------------------
   %$ = display real finite as a dollar value
   %,$ = display real finite as a dollar value seperated by commas
   ----------------------------------------------------------------------
   %s = display string
   %#s = display string & pad left with # spaces
   %-#s = display string & pad right with # spaces
   %ws = write string
   -> NOTE: padding MUST be of 3 digits or less (ie from -999 to 999)
   ----------------------------------------------------------------------
   %b  = bool
   %wb = write "true" or "false" instead of "#t" or "#f"
   ----------------------------------------------------------------------
   %%  = "%" (escapes a "%")
   ----------------------------------------------------------------------
```

-------------------------------------------------------------------------------
### `println`

#### Signatures:
```scheme
(println <obj>)
(println <output-port> <obj>)
```

#### Description:
```
Print <obj> to <output-port> in human-readable form, followed by a newline.
<output-port> defaults to (current-output-port).
Note that this will infinitely loop for cyclical structures!
```

-------------------------------------------------------------------------------
### `read`

#### Signatures:
```scheme
(read)
(read <input-port>)
```

#### Description:
```
Read an EScheme datum from <input-port>.
<input-port> defaults to (current-input-port).
Returns #eof once reached <input-port>'s end.
```

-------------------------------------------------------------------------------
### `read-char`

#### Signatures:
```scheme
(read-char)
(read-char <input-port>)
```

#### Description:
```
Read a char from <input-port>.
<input-port> defaults to (current-input-port).
Returns #eof once reached <input-port>'s end.
```

-------------------------------------------------------------------------------
### `read-chars`

#### Signatures:
```scheme
(read-chars <integer>)
(read-chars <input-port> <integer>)
```

#### Description:
```
Read <integer> chars as a string from <input-port>.
<input-port> defaults to (current-input-port).
Returns #eof once reached <input-port>'s end.
```

-------------------------------------------------------------------------------
### `read-line`

#### Signatures:
```scheme
(read-line)
(read-line <input-port>)
```

#### Description:
```
Read a line of text as a string from <input-port>.
<input-port> defaults to (current-input-port).
Returns #eof once reached <input-port>'s end.
```

-------------------------------------------------------------------------------
### `read-string`

#### Signatures:
```scheme
(read-string <string>)
```

#### Description:
```
Read an EScheme datum from the string. Returns a list:
  (<read-datum> <string-w/o-read-datum>)
```

-------------------------------------------------------------------------------
### `write`

#### Signatures:
```scheme
(write <obj>)
(write <output-port> <obj>)
```

#### Description:
```
Print <obj> to <output-port> in machine-readable form.
<output-port> defaults to (current-output-port).
Note that this will infinitely loop for cyclical structures!
```

-------------------------------------------------------------------------------
### `writef`

#### Signatures:
```scheme
(writef <format-string> <arg> ...)
(writef <output-port> <format-string> <arg> ...)
```

#### Description:
```
Print formatted <format-string> with "<arg> ..." to <output-port> in
machine-readable form. <output-port> defaults to (current-output-port).
Note that this will infinitely loop for cyclical structures!
>> <format-string> is like Java's printf with unique formatting patterns:
   ----------------------------------------------------------------------
   %a = display anything
   %wa = write anything
   %pa = pretty-print anything
   ----------------------------------------------------------------------
   %... = display unpacked list/vector/hashmap
   %w... = write unpacked list/vector/hashmap
   %p... = pretty-print unpacked list/vector/hashmap
   ----------------------------------------------------------------------
   %n = number
   %+n = number (show sign if positive too)
   %,n = number with commas
   %En = %en = number (coerced to exact)
   %In = %in = number (coerced to inexact)
   %#rn = %#Rn = number (in radix <#> from 2 to 36)
   %#n = number (left-padded with 0s to a width of <#> characters)
   %.#n = number (with <#> digits of precision)
   -> IE "%+e2rn": make exact in binary with sign
   -> NOTE: 1) 0-padding & precision MUST be of 2 digits or less!
            2) Can't have radix with I-coercion or precision!
   ----------------------------------------------------------------------
   %$ = display real finite as a dollar value
   %,$ = display real finite as a dollar value seperated by commas
   ----------------------------------------------------------------------
   %s = display string
   %#s = display string & pad left with # spaces
   %-#s = display string & pad right with # spaces
   %ws = write string
   -> NOTE: padding MUST be of 3 digits or less (ie from -999 to 999)
   ----------------------------------------------------------------------
   %b  = bool
   %wb = write "true" or "false" instead of "#t" or "#f"
   ----------------------------------------------------------------------
   %%  = "%" (escapes a "%")
   ----------------------------------------------------------------------
```

-------------------------------------------------------------------------------
## Keywords


-------------------------------------------------------------------------------
### `keyword?`

#### Signatures:
```scheme
(keyword? <obj>)
```

#### Description:
```
Returns whether <obj> is a keyword.
```

-------------------------------------------------------------------------------
## Lists


-------------------------------------------------------------------------------
### `alist?`

#### Signatures:
```scheme
(alist? <obj>)
```

#### Description:
```
Returns whether <obj> is an associative list (list of pairs).
```

-------------------------------------------------------------------------------
### `assoc`

#### Signatures:
```scheme
(assoc <key> <associative-list>)
```

#### Description:
```
Returns the pair in associative-lsit <alist> starting with <key> based
on <equal?> item equality. Returns #f if <obj> isn't a key in in <alist>.
```

-------------------------------------------------------------------------------
### `assq`

#### Signatures:
```scheme
(assq <key> <associative-list>)
```

#### Description:
```
Returns the pair in associative-lsit <alist> starting with <key> based
on <eq?> item equality. Returns #f if <obj> isn't a key in in <alist>.
```

-------------------------------------------------------------------------------
### `list`

#### Signatures:
```scheme
(list <obj> ...)
```

#### Description:
```
Return a list containing "<obj> ...". Given no args, returns '().
Lists are right-nested pairs ending in nil: '()
Create list literals via the (<literal> ...) syntax.
Can quote list literals to create values: '(<literal> ...)

Aliased by <ls>.
```

-------------------------------------------------------------------------------
### `list*`

#### Signatures:
```scheme
(list* <obj> <obj> ...)
```

#### Description:
```
Return a dotted.list containing "<obj> <obj> ...".
```

-------------------------------------------------------------------------------
### `list*?`

#### Signatures:
```scheme
(list*? <obj>)
```

#### Description:
```
Returns whether <obj> is a dotted-list.
```

-------------------------------------------------------------------------------
### `list?`

#### Signatures:
```scheme
(list? <obj>)
```

#### Description:
```
Returns whether <obj> is a proper list.
```

-------------------------------------------------------------------------------
### `member`

#### Signatures:
```scheme
(member <obj> <list>)
```

#### Description:
```
Returns the sublist in <list> starting with <obj> based on <equal?> item
equality. Returns #f if <obj> isn't in <list>.
```

-------------------------------------------------------------------------------
### `memq`

#### Signatures:
```scheme
(memq <obj> <list>)
```

#### Description:
```
Returns the sublist in <list> starting with <obj> based on <eq?> item
equality. Returns #f if <obj> isn't in <list>.
```

-------------------------------------------------------------------------------
### `null?`

#### Signatures:
```scheme
(null? <obj>)
```

#### Description:
```
Returns whether <obj> is nill (a.k.a. '() and #nil).
```

-------------------------------------------------------------------------------
### `unfold`

#### Signatures:
```scheme
(unfold <break?-condition> <mapper-callable> <update-callable> <seed>)
```

#### Description:
```
Unfolds a list from left to right, starting with <seed>. <break?-condition>
determines when unfolding stops, <mapper-callable> maps the <seed> to a value
in the unfolded list, and <update-callable> increments <seed> for the
next round of unfolding.
```

-------------------------------------------------------------------------------
### `unfold-right`

#### Signatures:
```scheme
(unfold-right <break?-condition> <mapper-callable> <update-callable> <seed>)
```

#### Description:
```
Unfolds a list from right to left, starting with <seed>. <break?-condition>
determines when unfolding stops, <mapper-callable> maps the <seed> to a value
in the unfolded list, and <update-callable> increments <seed> for the
next round of unfolding.
```

-------------------------------------------------------------------------------
## Meta


-------------------------------------------------------------------------------
### `apply`

#### Signatures:
```scheme
(apply <callable> <argument-ordered-collection>)
```

#### Description:
```
Apply <callable> to the arguments stored in <argument-ordered-collection>.
```

-------------------------------------------------------------------------------
### `callable-name`

#### Signatures:
```scheme
(callable-name <callable>)
```

#### Description:
```
Get the <callable>'s name as EScheme data, or #f if unavailable.
```

-------------------------------------------------------------------------------
### `callable-signature`

#### Signatures:
```scheme
(callable-signature <callable>)
```

#### Description:
```
Get the <callable> call signature as EScheme data, or #f if unavailable.

For unary parameter lists: returns the parameter clause as a list of symbols.
For binary+ parameter lists: returns a list of parameter clauses.

Keyword types are also included for typed signatures!
  => See <type-system> in <Topics> for more details on EScheme's types!

Note that the symbols '... & '. denote variadic parameters.
An argument list may be used to denote a default argument value.

For example:
  ; signature: (f a b c)
  (define (f a b c) a)

  ; signature: (f :int a :char b :str c)
  (define (f :int a :char b :str c) a)

  ; signature: (:int (f :int a :char b :str c))
  (define :int (f :int a :char b :str c) a)

  ; signature: ((f a b) (f c))
  (defn f ((a b) a) ((c) c))

  ; signature: (:int (f :int a :char b) :str (f :str c))
  (defn f (:int (:int a :char b) a) (:str (:str c) c))
```

-------------------------------------------------------------------------------
### `compile`

#### Signatures:
```scheme
(compile <escm-code-as-data>)
```

#### Description:
```
Convert <escm-code-as-data> into a list of its equivalent bytecode instructions.
```

-------------------------------------------------------------------------------
### `define-help`

#### Signatures:
```scheme
(define-help <path:name-string> <docstring>)
```

#### Description:
```
Register the <docstring> topic document in help's file tree, at path <path> in
file <name>. Note that <path> denotes a set of folder names seperated by <:>.

Aliased by <defhelp>. Use <help> to see defined topic documents.
```

-------------------------------------------------------------------------------
### `docstring`

#### Signatures:
```scheme
(docstring <obj>)
```

#### Description:
```
Get the <obj>'s docstring if available, else #f.

Docstrings are used to give details on objects passed to the <help> function,
and **MUST** be writted as string literals to register as valid syntax (think
procedures, classes, interfaces, etc.).

See <help>'s help entry for details on the "@help" docstring syntax: used to 
put the doc's details in a specific folder during the interactive help menu.

Note that EScheme supports spanning string literals over several lines to 
automatically insert newline characters, so
"
hello
there!
"
is a valid string that compiles to "\nhello\nthere!\n"!

Docstrings undergo minor fomatting to improve printing consistency:
  1. All tab characters are converted to 2 spaces
  2. Strings are right-trimmed (whitespace removed from the back)
  3. Leading newline characters are trimmed from the left-hand side
  4. The left-hand side padding of the string's lines are cropped:
     
     The minimum spacing prior the 1st character on a line is eliminated
     from each line. This trims our docstring for more compact printing,
     while still maintaining the original levels of relative indentation.

     This allows us to write functions with docstrings like:

     (define (factorial n)
       "
       The factorial function:
         => accepts a single integer argument
       "
       (if (< n 2)
           1
           (* n (factorial (- n 1)))))

    where despite "The factorial function:" being indented with two spaces 
    (if we start counting spaces from "(define (factorial n)" in the code),
    the docstring will still print in <help> as if written:

      "The factorial function:\n  => accepts a single integer argument"

    thereby cropping the string while maintaining "=>"'s relative indentation.
```

-------------------------------------------------------------------------------
### `eval`

#### Signatures:
```scheme
(eval <escm-code-as-data>)
```

#### Description:
```
Evaluate <escm-code-as-data> to produce a result. Equivalent to:
  (eval-bytecode (compile <escm-code-as-data>))
```

-------------------------------------------------------------------------------
### `eval-bytecode`

#### Signatures:
```scheme
(eval-bytecode <escm-bytecode-as-data>)
```

#### Description:
```
Evaluate <escm-bytecode-as-data> to produce a result.
```

-------------------------------------------------------------------------------
### `expand-syntax`

#### Signatures:
```scheme
(expand-syntax <quoted-macro-expression>)
```

#### Description:
```
Returns the expansion of <quoted-macro-expression> by executing its macro.
```

-------------------------------------------------------------------------------
### `gensym`

#### Signatures:
```scheme
(gensym)
(gensym <name-symbol>)
```

#### Description:
```
Returns a fresh, unique symbol. Guarentees that the symbol is unique across
threads as well. Used extensively by macros. Pass a <name-symbol> to improve
readability when printing generated symbols.
```

-------------------------------------------------------------------------------
### `help`

#### Signatures:
```scheme
(help)
(help <path-string>)
(help <obj>)
```

#### Description:
```
Obj Argument:
  Get information about <obj>.

String Argument:
  Get result of typing in <path-string> to the interactive help menu.

No Arguments:
  Launch the interactive help menu. The help menu consists of folders, which
  in turn hold descriptions of various variables in EScheme's environment.

    * Note that the <help> menu always operates relative to the program's
      original <stdin> and <stdout> streams, rather than the values of
      <current-input-port> and <current-output-port>.

  Type folder names in the input prompt to enter them, and use ":" as a 
  separator to enter multiple folders (e.g. "folder1:folder2:folder3").
  Type . for the current directory, .. for the parent, ... for the 
  grandparent, etc.

  Type :quit to quit, :~ to return to the home directory, or :help for more
  information. Type :eval <var> to print as if had passed <var> to <help>.

  Procedures, classes, and interfaces that want to explain how they work in 
  the <help> menu should use <docstring> syntax. Within the <docstring>, the
  "@help" syntax can be used to place the variable within a certain 
  subdirectory of the help menu. For example:

    (define (fact n)
      "
      @help:Procedures:Numbers
      The factorial function. Accepts an int arg.
      "
      (if (< n 2)
          1
          (* n (fact (- n 1)))))

  would put fact's <help> entry in the Numbers directory, which itself is in
  the Procedures directory. Docstring entries without the "@help" syntax
  are placed in the "Misc" directory.

See <define-help> to register topic documents in help's file tree.
See <help-directory> to get help's files as an EScheme data structure.
```

-------------------------------------------------------------------------------
### `help-directory`

#### Signatures:
```scheme
(help-directory)
```

#### Description:
```
Get the entire <help> menu directory as an EScheme data structure. A folder
is a list that starts with its name as a keyword, followed by its child files.
A topic is a pair with a keyword name <car> and a docstring <cdr>.

Use <help> to explore this data structure interactively via the command line.
```

-------------------------------------------------------------------------------
### `help-markdown`

#### Signatures:
```scheme
(help-markdown)
```

#### Description:
```
Get the entire <help> menu directory as a markdown string. Folder contents are
organized by topics then subfolders, with each section being alphabetically
listed.

Use <help> to explore this markdown interactively via the command line.
```

-------------------------------------------------------------------------------
### `syntax?`

#### Signatures:
```scheme
(syntax? <obj>)
```

#### Description:
```
Returns whether <obj> is a syntax object. Syntax objects are created when
macros are evaluated as a procedure argument.
```

-------------------------------------------------------------------------------
### `type-alias`

#### Signatures:
```scheme
(type-alias <type-keyword>)
```

#### Description:
```
Creates a type alias value for <type-keyword>. When bound via <define> to a
symbolic alias, that alias may be used as a keyword to reference <type-keyword>
in procedural type dispatch.
  => See <type-system> in <Topics> for more details on EScheme's types!

See <type-alias?> to determine if a value is a type alias, as well as
<type-alias-source> to get the keyword type that an alias references.

See <define-type> for a convenience macro wrapping <define> and <type-alias>.

For example:
  ; Have :my-type alias :int|char
  (define my-type (type-alias :int|char))

  ; Use :my-type as a keyword type
  (define :my-type (f :bool choice)
    (if choice 1 #))
```

-------------------------------------------------------------------------------
### `type-alias-source`

#### Signatures:
```scheme
(type-alias-source <type-alias>)
```

#### Description:
```
Returns the keyword type that <type-alias> references. See <type-alias>
for more details.
```

-------------------------------------------------------------------------------
### `type-alias?`

#### Signatures:
```scheme
(type-alias? <obj>)
```

#### Description:
```
Returns whether <obj> is a type-alias. See <type-alias> for more details.
```

-------------------------------------------------------------------------------
### `type-is?`

#### Signatures:
```scheme
(type-is? <obj> <type-keyword>)
```

#### Description:
```
Returns whether <obj> is a <type-keyword>.
  => See <type-system> in <Topics> for more details on EScheme's types!
```

-------------------------------------------------------------------------------
## Numbers


-------------------------------------------------------------------------------
### `*`

#### Signatures:
```scheme
(* <callable> ...)
(* <number> ...)
```

#### Description:
```
Returns the product of "<number> <number> ...".
Note that EScheme defines (* <n> 0) to be 0 for all numeric <n>.

Aliases <compose> if only given callables.
```

-------------------------------------------------------------------------------
### `expt`

#### Signatures:
```scheme
(expt <number> <number> ...)
```

#### Description:
```
Returns the exponentiation of "<number> <number> ...".
Remember that exponentiation is right-associative!
Aliased by <**>.

Note that EScheme defines the following to be true for all numeric <n>:
  (expt <n> 0) ; 1
  (expt 0 <positive-n>) ; 0
  (expt 0 <negative-n>) ; Infinity
  (expt 1 <infinite-n>) ; (expt -1 <infinite-n>) ; 1
```

-------------------------------------------------------------------------------
### `+`

#### Signatures:
```scheme
(+)
(+ <obj>)
(+ <symbol> ...)
(+ <keyword> ...)
(+ <associative-collection> ...)
(+ <callable> <arg> ...)
(+ <number> ...)
```

#### Description:
```
Returns the sum of "<number> <number> ...".

Aliases <append> if given a <symbol>, <keyword>, <associative-collection>,
or 0-1 args. Aliases <bind> if give a non-<associative-collection> callable.
```

-------------------------------------------------------------------------------
### `-`

#### Signatures:
```scheme
(- <number>)
(- <number> <number> ...)
```

#### Description:
```
Returns the difference of "<number> <number> ...".
If given one <number>, returns its negative value.
```

-------------------------------------------------------------------------------
### `/`

#### Signatures:
```scheme
(/ <number>)
(/ <number> <number> ...)
```

#### Description:
```
Returns the division of "<number> <number> ...".
If given one <number>, returns its inverse value.
```

-------------------------------------------------------------------------------
### `<`

#### Signatures:
```scheme
(< <real> <real> ...)
(< <string> <string> ...)
(< <char> <char> ...)
```

#### Description:
```
Returns whether "<real> <real> ..." are < one another.
Aliases <string<?> and <char<?> too!
```

-------------------------------------------------------------------------------
### `<=`

#### Signatures:
```scheme
(<= <real> <real> ...)
(<= <string> <string> ...)
(<= <char> <char> ...)
```

#### Description:
```
Returns whether "<real> <real> ..." are <= one another.
Aliases <string<=?> and <char<=?> too!
```

-------------------------------------------------------------------------------
### `=`

#### Signatures:
```scheme
(= <number> <number> ...)
(= <obj> <obj> ...)
```

#### Description:
```
Returns whether "<number> <number> ..." are equal to one another
Aliases <eq?> if given non-numerics.
```

-------------------------------------------------------------------------------
### `>`

#### Signatures:
```scheme
(> <real> <real> ...)
(> <string> <string> ...)
(> <char> <char> ...)
```

#### Description:
```
Returns whether "<real> <real> ..." are > one another.
Aliases <string>?> and <char>?> too!
```

-------------------------------------------------------------------------------
### `>=`

#### Signatures:
```scheme
(>= <real> <real> ...)
(>= <string> <string> ...)
(>= <char> <char> ...)
```

#### Description:
```
Returns whether "<real> <real> ..." are >= one another.
Aliases <string>=?> and <char>=?> too!
```

-------------------------------------------------------------------------------
### `abs`

#### Signatures:
```scheme
(abs <real>)
```

#### Description:
```
Returns the absolute value of <real>.
```

-------------------------------------------------------------------------------
### `acos`

#### Signatures:
```scheme
(acos <number>)
```

#### Description:
```
Returns the acos of <number>.
Note that acos yields a complex number if <number> exceeds its domain.
```

-------------------------------------------------------------------------------
### `acosh`

#### Signatures:
```scheme
(acosh <number>)
```

#### Description:
```
Returns the acosh of <number>.
Note that acosh yields a complex number if <number> exceeds its domain.
```

-------------------------------------------------------------------------------
### `angle`

#### Signatures:
```scheme
(angle <number>)
```

#### Description:
```
Returns the angle of <number>.
```

-------------------------------------------------------------------------------
### `asin`

#### Signatures:
```scheme
(asin <number>)
```

#### Description:
```
Returns the asin of <number>.
Note that asin yields a complex number if <number> exceeds its domain.
```

-------------------------------------------------------------------------------
### `asinh`

#### Signatures:
```scheme
(asinh <number>)
```

#### Description:
```
Returns the asinh of <number>.
```

-------------------------------------------------------------------------------
### `atan`

#### Signatures:
```scheme
(atan <number>)
(atan <real> <real>)
```

#### Description:
```
Returns the atan of <number>. If given 2 arguments, returns the atan in
radians of (/ <real> <real>) based on the signs of both values
to determine the correct quadrant.
```

-------------------------------------------------------------------------------
### `atanh`

#### Signatures:
```scheme
(atanh <number>)
```

#### Description:
```
Returns the atanh of <number>.
Note that atanh yields a complex number if <number> exceeds its domain.
```

-------------------------------------------------------------------------------
### `ceiling`

#### Signatures:
```scheme
(ceiling <real>)
```

#### Description:
```
Returns the ceiling value of <real>.
```

-------------------------------------------------------------------------------
### `complex?`

#### Signatures:
```scheme
(complex? <obj>)
```

#### Description:
```
Returns whether <obj> is a complex number. Equivalent to <number?>.
```

-------------------------------------------------------------------------------
### `conjugate`

#### Signatures:
```scheme
(conjugate <number>)
```

#### Description:
```
Returns the conjugate of <number>.
```

-------------------------------------------------------------------------------
### `cos`

#### Signatures:
```scheme
(cos <number>)
```

#### Description:
```
Returns the cos of <number>.
```

-------------------------------------------------------------------------------
### `cosh`

#### Signatures:
```scheme
(cosh <number>)
```

#### Description:
```
Returns the cosh of <number>.
```

-------------------------------------------------------------------------------
### `denominator`

#### Signatures:
```scheme
(denominator <real>)
```

#### Description:
```
Returns the denominator of <real> as an exact number.
See <numerator>.
```

-------------------------------------------------------------------------------
### `divrem`

#### Signatures:
```scheme
(divrem <dividend-real> <divisor-real>)
```

#### Description:
```
Returns a pair: the quotient & remainder of (/ <dividend-real> <divisor-real>).
```

-------------------------------------------------------------------------------
### `even?`

#### Signatures:
```scheme
(even? <real>)
```

#### Description:
```
Returns whether <real> is even.
```

-------------------------------------------------------------------------------
### `exact->inexact`

#### Signatures:
```scheme
(exact->inexact <number>)
```

#### Description:
```
Coerces <number> to be inexact.
```

-------------------------------------------------------------------------------
### `exact?`

#### Signatures:
```scheme
(exact? <obj>)
```

#### Description:
```
Returns whether <obj> is an exact number.
```

-------------------------------------------------------------------------------
### `exp`

#### Signatures:
```scheme
(exp <number>)
```

#### Description:
```
Returns Euler's number raised to the power of <number>.
```

-------------------------------------------------------------------------------
### `expt-mod`

#### Signatures:
```scheme
(expt-mod <base-real> <power-real> <mod-real>)
```

#### Description:
```
Efficiently performs (modulo (expt <base-real> <power-real>) <mod-real>).
```

-------------------------------------------------------------------------------
### `finite?`

#### Signatures:
```scheme
(finite? <obj>)
```

#### Description:
```
Returns whether <obj> is a finite value.
```

-------------------------------------------------------------------------------
### `floor`

#### Signatures:
```scheme
(floor <real>)
```

#### Description:
```
Returns the floored value of <real>.
```

-------------------------------------------------------------------------------
### `fractional`

#### Signatures:
```scheme
(fractional <real>)
```

#### Description:
```
Get the fractional exact value of <real> as an inexact.
For example:
  (fractional 5.0) ; 0
  (fractional 5.2) ; 2
  (fractional NaN) ; ERROR
  (fractional Infinity) ; ERROR
```

-------------------------------------------------------------------------------
### `gcd`

#### Signatures:
```scheme
(gcd <integer> <integer>)
```

#### Description:
```
Returns the greatest common denominator of <integer> & <integer>.
```

-------------------------------------------------------------------------------
### `imag-part`

#### Signatures:
```scheme
(imag-part <number>)
```

#### Description:
```
Returns the imaginary component of <number>.
```

-------------------------------------------------------------------------------
### `inexact->exact`

#### Signatures:
```scheme
(inexact->exact <number>)
```

#### Description:
```
Coerces <number> to be exact.
```

-------------------------------------------------------------------------------
### `inexact?`

#### Signatures:
```scheme
(inexact? <obj>)
```

#### Description:
```
Returns whether <obj> is an inexact number.
```

-------------------------------------------------------------------------------
### `infinite?`

#### Signatures:
```scheme
(infinite? <obj>)
```

#### Description:
```
Returns whether <obj> is <Infinite>.
```

-------------------------------------------------------------------------------
### `integer?`

#### Signatures:
```scheme
(integer? <obj>)
```

#### Description:
```
Returns whether <obj> is an integer.
```

-------------------------------------------------------------------------------
### `integral`

#### Signatures:
```scheme
(integral <real>)
```

#### Description:
```
Get the integral exact value of <real> as an inexact.
For Example:
  (integral 0.5) ; 0
  (integral 5.2) ; 5
  (integral NaN) ; ERROR
  (integral Infinity) ; ERROR
```

-------------------------------------------------------------------------------
### `lcm`

#### Signatures:
```scheme
(lcm <integer> <integer>)
```

#### Description:
```
Returns the least common multiple of <integer> & <integer>.
```

-------------------------------------------------------------------------------
### `log`

#### Signatures:
```scheme
(log <number>)
(log <number> <log-base-number>)
```

#### Description:
```
Returns the log base-<base> of <number>. <base> defaults to (exp 1).
```

-------------------------------------------------------------------------------
### `magnitude`

#### Signatures:
```scheme
(magnitude <number>)
```

#### Description:
```
Returns the magnitude of <number>.
```

-------------------------------------------------------------------------------
### `make-polar`

#### Signatures:
```scheme
(make-polar <magnitude-real> <angle-real>)
```

#### Description:
```
Returns a complex number with <magnitude> & <angle> as its magnitude & angle.
```

-------------------------------------------------------------------------------
### `make-rectangular`

#### Signatures:
```scheme
(make-rectangular <real-real> <imag-real>)
```

#### Description:
```
Returns a complex number with <real> & <imag> as real & imaginary components.
```

-------------------------------------------------------------------------------
### `max`

#### Signatures:
```scheme
(max <real> <real> ...)
```

#### Description:
```
Returns the max value of "<real> ...".
```

-------------------------------------------------------------------------------
### `min`

#### Signatures:
```scheme
(min <real> <real> ...)
```

#### Description:
```
Returns the min value of "<real> ...".
```

-------------------------------------------------------------------------------
### `modf`

#### Signatures:
```scheme
(modf <real>)
```

#### Description:
```
Returns a pair: the integral & factional components of <real> as an inexact.
```

-------------------------------------------------------------------------------
### `modulo`

#### Signatures:
```scheme
(modulo <dividend-real> <divisor-real>)
```

#### Description:
```
Returns <dividend-real> modulo <divisor-real>.
```

-------------------------------------------------------------------------------
### `nan?`

#### Signatures:
```scheme
(nan? <obj>)
```

#### Description:
```
Returns whether <obj> is <NaN>.
```

-------------------------------------------------------------------------------
### `ncr`

#### Signatures:
```scheme
(ncr <n-integer> <r-integer>)
```

#### Description:
```
Returns the number of ways in which <r> different things
can be selected out of <n> different things.
Both integers must be non-negative.
```

-------------------------------------------------------------------------------
### `negative?`

#### Signatures:
```scheme
(negative? <real>)
```

#### Description:
```
Returns whether <real> is negative.
```

-------------------------------------------------------------------------------
### `npr`

#### Signatures:
```scheme
(npr <n-integer> <r-integer>)
```

#### Description:
```
Returns the number of ways in which <r> different things
can be selected and arranged out of <n> different things.
Both integers must be non-negative.
```

-------------------------------------------------------------------------------
### `number?`

#### Signatures:
```scheme
(number? <obj>)
```

#### Description:
```
Returns whether <obj> is a number.
```

-------------------------------------------------------------------------------
### `numerator`

#### Signatures:
```scheme
(numerator <real>)
```

#### Description:
```
Returns the numerator of <real> as an exact number.
See <denominator>.
```

-------------------------------------------------------------------------------
### `odd?`

#### Signatures:
```scheme
(odd? <real>)
```

#### Description:
```
Returns whether <real> is odd.
```

-------------------------------------------------------------------------------
### `positive?`

#### Signatures:
```scheme
(positive? <real>)
```

#### Description:
```
Returns whether <real> is positive.
```

-------------------------------------------------------------------------------
### `quotient`

#### Signatures:
```scheme
(quotient <dividend-real> <divisor-real>)
```

#### Description:
```
Returns the quotient of (/ <dividend-real> <divisor-real>).
See <remainder>.
```

-------------------------------------------------------------------------------
### `random`

#### Signatures:
```scheme
(random)
```

#### Description:
```
Returns a random number between 0.0 and 1.0.
Guarenteed to be unique across threads.
```

-------------------------------------------------------------------------------
### `real-part`

#### Signatures:
```scheme
(real-part <number>)
```

#### Description:
```
Returns the real component of <number>.
```

-------------------------------------------------------------------------------
### `real?`

#### Signatures:
```scheme
(real? <obj>)
```

#### Description:
```
Returns whether <obj> is a real (non-complex) number.
```

-------------------------------------------------------------------------------
### `remainder`

#### Signatures:
```scheme
(remainder <dividend-real> <divisor-real>)
```

#### Description:
```
Returns the remainder of (/ <dividend-real> <divisor-real>).
See <quotient>.
```

-------------------------------------------------------------------------------
### `round`

#### Signatures:
```scheme
(round <real>)
```

#### Description:
```
Returns the rounded value of <real>.
```

-------------------------------------------------------------------------------
### `sin`

#### Signatures:
```scheme
(sin <number>)
```

#### Description:
```
Returns the sin of <number>.
```

-------------------------------------------------------------------------------
### `sinh`

#### Signatures:
```scheme
(sinh <number>)
```

#### Description:
```
Returns the sinh of <number>.
```

-------------------------------------------------------------------------------
### `sqrt`

#### Signatures:
```scheme
(sqrt <number>)
```

#### Description:
```
Returns the square-root of <number>.
```

-------------------------------------------------------------------------------
### `tan`

#### Signatures:
```scheme
(tan <number>)
```

#### Description:
```
Returns the tan of <number>.
```

-------------------------------------------------------------------------------
### `tanh`

#### Signatures:
```scheme
(tanh <number>)
```

#### Description:
```
Returns the tanh of <number>.
```

-------------------------------------------------------------------------------
### `truncate`

#### Signatures:
```scheme
(truncate <real>)
```

#### Description:
```
Returns the truncated value of <real>.
```

-------------------------------------------------------------------------------
### `zero?`

#### Signatures:
```scheme
(zero? <real>)
```

#### Description:
```
Returns whether <real> is zero.
```

-------------------------------------------------------------------------------
## OOP


-------------------------------------------------------------------------------
### `class?`

#### Signatures:
```scheme
(class? <obj>)
```

#### Description:
```
Returns whether <obj> is a <class> (as created by <class> or <define-class>).
Classes can be applied to invoke their constructor.
```

-------------------------------------------------------------------------------
### `functor?`

#### Signatures:
```scheme
(functor? <obj>)
```

#### Description:
```
Returns whether <obj> is an applicable object. Equivalent to:
  (and (object? <obj>) (oo-has? <obj> '->procedure))
```

-------------------------------------------------------------------------------
### `interface?`

#### Signatures:
```scheme
(interface? <obj>)
```

#### Description:
```
Returns whether <obj> is an <interface> (as created by <interface> or <define-interface>).
```

-------------------------------------------------------------------------------
### `meta-object?`

#### Signatures:
```scheme
(meta-object? <obj>)
```

#### Description:
```
Returns whether <obj> is a meta-object. Equivalent to:
  (or (class? <obj>) (interface? <obj>) (object? <obj>))
```

-------------------------------------------------------------------------------
### `object?`

#### Signatures:
```scheme
(object? <obj>)
```

#### Description:
```
Returns whether <obj> is an <object> (as created by a class constructor).
Objects can become functors by defining a <->procedure> method.
```

-------------------------------------------------------------------------------
### `oo-define`

#### Signatures:
```scheme
(oo-define <meta-object> <property-symbol-name> ... <value>)
```

#### Description:
```
Defines the "<property-symbol-name> ..." property of <meta-object> to be <value>.
  => NOTE: Only operates on static values if <meta-object> is a class or interface.
```

-------------------------------------------------------------------------------
### `oo-get`

#### Signatures:
```scheme
(oo-get <meta-object> <property-symbol-name> ...)
```

#### Description:
```
Returns the "<property-symbol-name> ..." property of <meta-object>.
Triggers an error upon failure.
  => NOTE: Only operates on static values if <meta-object> is a class or interface.
```

-------------------------------------------------------------------------------
### `oo-has?`

#### Signatures:
```scheme
(oo-has? <meta-object> <property-symbol-name> ...)
```

#### Description:
```
Returns whether <meta-object> contains "<property-symbol-name> ..." as a property chain.
  => NOTE: Only operates on static values if <meta-object> is a class or interface.
```

-------------------------------------------------------------------------------
### `oo-interfaces`

#### Signatures:
```scheme
(oo-interfaces <meta-object>)
```

#### Description:
```
Returns the list of interfaces implemented by <meta-object>.
```

-------------------------------------------------------------------------------
### `oo-is?`

#### Signatures:
```scheme
(oo-is? <object> <class>)
(oo-is? <object> <interface>)
```

#### Description:
```
Returns whether <object> is an instance of the <class> or <interface>.
```

-------------------------------------------------------------------------------
### `oo-properties`

#### Signatures:
```scheme
(oo-properties <meta-object>)
```

#### Description:
```
Returns a list of <meta-object>'s property name symbols.
For classes and interfaces, it also denotes whether the property is static or not.
```

-------------------------------------------------------------------------------
### `oo-set!`

#### Signatures:
```scheme
(oo-set! <meta-object> <property-symbol-name> ... <value>)
```

#### Description:
```
Sets the "<property-symbol-name> ..." property of <meta-object> to <value>.
Triggers an error upon failure.
  => NOTE: Only operates on static values if <meta-object> is a class or interface.
```

-------------------------------------------------------------------------------
### `oo-super`

#### Signatures:
```scheme
(oo-super <object>)
(oo-super <class>)
```

#### Description:
```
Returns the super meta-object of <object> or <class>.
```

-------------------------------------------------------------------------------
## Ordered-Collections


-------------------------------------------------------------------------------
### `delete-neighbor-duplicates`

#### Signatures:
```scheme
(delete-neighbor-duplicates <elt=?-callable> <ordered-collection>)
```

#### Description:
```
Returns <oc> with any adjacent items matching with <elt=?> reduced to a single
item.
```

-------------------------------------------------------------------------------
### `drop-right`

#### Signatures:
```scheme
(drop-right <ordered-collection> <length>)
```

#### Description:
```
Returns <oc> without <length> items from its right side.
```

-------------------------------------------------------------------------------
### `drop-right-while`

#### Signatures:
```scheme
(drop-right-while <continue?-callable> <ordered-collection>)
```

#### Description:
```
Returns <oc> with items dropped from its right side while <continue?-callable>
was satisfied.
```

-------------------------------------------------------------------------------
### `drop-while`

#### Signatures:
```scheme
(drop-while <continue?-callable> <ordered-collection>)
```

#### Description:
```
Returns <oc> with items dropped from its left side while <continue?-callable>
was satisfied.
```

-------------------------------------------------------------------------------
### `fold-right`

#### Signatures:
```scheme
(fold-right <callable> <seed> <ordered-collection> ...)
```

#### Description:
```
Accumulate the values in "<oc> ..." from right to left by applying
<callable> to "<previous-result>" and <item> with <seed-obj> acting
as the initial "<previous-result>".

  => Note that the "<oc> ..." values will have their types unified according
     to the following hierarchy: String < List < Vector
```

-------------------------------------------------------------------------------
### `init`

#### Signatures:
```scheme
(init <ordered-collection>)
```

#### Description:
```
Returns everything except the last item in <oc>.
```

-------------------------------------------------------------------------------
### `key-right`

#### Signatures:
```scheme
(key-right <elt=?-callable> <ordered-collection>)
```

#### Description:
```
Get the last index in <ac> who's associated value satisfies <elt=?-callable?>.
```

-------------------------------------------------------------------------------
### `last`

#### Signatures:
```scheme
(last <ordered-collection>)
```

#### Description:
```
Returns the last item in <oc>.
```

-------------------------------------------------------------------------------
### `merge`

#### Signatures:
```scheme
(merge <binary-predicate?-callable> <ordered-collection> <ordered-collection>)
```

#### Description:
```
Returns the <oc>'s merged with one another according to <binary-predicate?>'s
comparison.
```

-------------------------------------------------------------------------------
### `ordered-collection?`

#### Signatures:
```scheme
(ordered-collection? <obj>)
```

#### Description:
```
Returns whether <obj> is an ordered collection: String | List | Vector
```

-------------------------------------------------------------------------------
### `remove-first`

#### Signatures:
```scheme
(remove-first <elt=?-callable> <ordered-collection>)
```

#### Description:
```
Returns <oc> without the first value satisfying <elt=?-callable>.
```

-------------------------------------------------------------------------------
### `remove-last`

#### Signatures:
```scheme
(remove-last <elt=?-callable> <ordered-collection>)
```

#### Description:
```
Returns <oc> without the last value satisfying <elt=?-callable>.
```

-------------------------------------------------------------------------------
### `reverse`

#### Signatures:
```scheme
(reverse <ordered-collection>)
```

#### Description:
```
Returns <oc> in reverse.
```

-------------------------------------------------------------------------------
### `skip`

#### Signatures:
```scheme
(skip <elt=?-callable> <ordered-collection>)
```

#### Description:
```
Returns the first item that doesn't satisfy <elt=?-callable>.
```

-------------------------------------------------------------------------------
### `skip-right`

#### Signatures:
```scheme
(skip-right <elt=?-callable> <ordered-collection>)
```

#### Description:
```
Returns the last item that doesn't satisfy <elt=?-callable>.
```

-------------------------------------------------------------------------------
### `slice`

#### Signatures:
```scheme
(slice <ordered-collection> <index>)
(slice <ordered-collection> <index> <length>)
(slice <ordered-collection> <index> <continue?-callable>)
```

#### Description:
```
Slices a subset of the items in <oc> starting from <index>.
If no other args are given, returns the rest of the items from <index>.
If <length> is given, returns at most <length> items.
Given <continue?-callable>, slices while values satisfy <continue?-callable>.
```

-------------------------------------------------------------------------------
### `sort`

#### Signatures:
```scheme
(sort <binary-predicate?-callable> <ordered-collection>)
```

#### Description:
```
Returns <oc> sorted according to <binary-predicate?>'s comparison.
```

-------------------------------------------------------------------------------
### `sorted?`

#### Signatures:
```scheme
(sorted? <binary-predicate?-callable> <ordered-collection>)
```

#### Description:
```
Returns whether <oc> was sorted according to <binary-predicate?>'s comparison.
```

-------------------------------------------------------------------------------
### `take-right`

#### Signatures:
```scheme
(take-right <ordered-collection> <length>)
```

#### Description:
```
Returns <oc> with <length> items from its right side.
```

-------------------------------------------------------------------------------
### `take-right-while`

#### Signatures:
```scheme
(take-right-while <continue?-callable> <ordered-collection>)
```

#### Description:
```
Returns items taken from <oc>'s right side while <continue?-callable> was
satisfied.
```

-------------------------------------------------------------------------------
### `take-while`

#### Signatures:
```scheme
(take-while <continue?-callable> <ordered-collection>)
```

#### Description:
```
Returns items taken from <oc>'s left side while <continue?-callable> was
satisfied.
```

-------------------------------------------------------------------------------
## Pairs


-------------------------------------------------------------------------------
### `atom?`

#### Signatures:
```scheme
(atom? <obj>)
```

#### Description:
```
Returns whether <obj> is not a pair.
```

-------------------------------------------------------------------------------
### `caaaar`

#### Signatures:
```scheme
(caaaar <pair>)
```

#### Description:
```
Equivalent to: (car (car (car (car <pair>))))
```

-------------------------------------------------------------------------------
### `caaadr`

#### Signatures:
```scheme
(caaadr <pair>)
```

#### Description:
```
Equivalent to: (car (car (car (cdr <pair>))))
```

-------------------------------------------------------------------------------
### `caaar`

#### Signatures:
```scheme
(caaar <pair>)
```

#### Description:
```
Equivalent to: (car (car (car <pair>)))
```

-------------------------------------------------------------------------------
### `caadar`

#### Signatures:
```scheme
(caadar <pair>)
```

#### Description:
```
Equivalent to: (car (car (cdr (car <pair>))))
```

-------------------------------------------------------------------------------
### `caaddr`

#### Signatures:
```scheme
(caaddr <pair>)
```

#### Description:
```
Equivalent to: (car (car (cdr (cdr <pair>))))
```

-------------------------------------------------------------------------------
### `caadr`

#### Signatures:
```scheme
(caadr <pair>)
```

#### Description:
```
Equivalent to: (car (car (cdr <pair>)))
```

-------------------------------------------------------------------------------
### `caar`

#### Signatures:
```scheme
(caar <pair>)
```

#### Description:
```
Equivalent to: (car (car <pair>))
```

-------------------------------------------------------------------------------
### `cadaar`

#### Signatures:
```scheme
(cadaar <pair>)
```

#### Description:
```
Equivalent to: (car (cdr (car (car <pair>))))
```

-------------------------------------------------------------------------------
### `cadadr`

#### Signatures:
```scheme
(cadadr <pair>)
```

#### Description:
```
Equivalent to: (car (cdr (car (cdr <pair>))))
```

-------------------------------------------------------------------------------
### `cadar`

#### Signatures:
```scheme
(cadar <pair>)
```

#### Description:
```
Equivalent to: (car (cdr (car <pair>)))
```

-------------------------------------------------------------------------------
### `caddar`

#### Signatures:
```scheme
(caddar <pair>)
```

#### Description:
```
Equivalent to: (car (cdr (cdr (car <pair>))))
```

-------------------------------------------------------------------------------
### `cadddr`

#### Signatures:
```scheme
(cadddr <pair>)
```

#### Description:
```
Equivalent to: (car (cdr (cdr (cdr <pair>))))
```

-------------------------------------------------------------------------------
### `caddr`

#### Signatures:
```scheme
(caddr <pair>)
```

#### Description:
```
Equivalent to: (car (cdr (cdr <pair>)))
```

-------------------------------------------------------------------------------
### `cadr`

#### Signatures:
```scheme
(cadr <pair>)
```

#### Description:
```
Equivalent to: (car (cdr <pair>))
```

-------------------------------------------------------------------------------
### `car`

#### Signatures:
```scheme
(car <pair>)
```

#### Description:
```
Access the first item in a pair.
```

-------------------------------------------------------------------------------
### `cdaaar`

#### Signatures:
```scheme
(cdaaar <pair>)
```

#### Description:
```
Equivalent to: (cdr (car (car (car <pair>))))
```

-------------------------------------------------------------------------------
### `cdaadr`

#### Signatures:
```scheme
(cdaadr <pair>)
```

#### Description:
```
Equivalent to: (cdr (car (car (cdr <pair>))))
```

-------------------------------------------------------------------------------
### `cdaar`

#### Signatures:
```scheme
(cdaar <pair>)
```

#### Description:
```
Equivalent to: (cdr (car (car <pair>)))
```

-------------------------------------------------------------------------------
### `cdadar`

#### Signatures:
```scheme
(cdadar <pair>)
```

#### Description:
```
Equivalent to: (cdr (car (cdr (car <pair>))))
```

-------------------------------------------------------------------------------
### `cdaddr`

#### Signatures:
```scheme
(cdaddr <pair>)
```

#### Description:
```
Equivalent to: (cdr (car (cdr (cdr <pair>))))
```

-------------------------------------------------------------------------------
### `cdadr`

#### Signatures:
```scheme
(cdadr <pair>)
```

#### Description:
```
Equivalent to: (cdr (car (cdr <pair>)))
```

-------------------------------------------------------------------------------
### `cdar`

#### Signatures:
```scheme
(cdar <pair>)
```

#### Description:
```
Equivalent to: (cdr (car <pair>))
```

-------------------------------------------------------------------------------
### `cddaar`

#### Signatures:
```scheme
(cddaar <pair>)
```

#### Description:
```
Equivalent to: (cdr (cdr (car (car <pair>))))
```

-------------------------------------------------------------------------------
### `cddadr`

#### Signatures:
```scheme
(cddadr <pair>)
```

#### Description:
```
Equivalent to: (cdr (cdr (car (cdr <pair>))))
```

-------------------------------------------------------------------------------
### `cddar`

#### Signatures:
```scheme
(cddar <pair>)
```

#### Description:
```
Equivalent to: (cdr (cdr (car <pair>)))
```

-------------------------------------------------------------------------------
### `cdddar`

#### Signatures:
```scheme
(cdddar <pair>)
```

#### Description:
```
Equivalent to: (cdr (cdr (cdr (car <pair>))))
```

-------------------------------------------------------------------------------
### `cddddr`

#### Signatures:
```scheme
(cddddr <pair>)
```

#### Description:
```
Equivalent to: (cdr (cdr (cdr (cdr <pair>))))
```

-------------------------------------------------------------------------------
### `cdddr`

#### Signatures:
```scheme
(cdddr <pair>)
```

#### Description:
```
Equivalent to: (cdr (cdr (cdr <pair>)))
```

-------------------------------------------------------------------------------
### `cddr`

#### Signatures:
```scheme
(cddr <pair>)
```

#### Description:
```
Equivalent to: (cdr (cdr <pair>))
```

-------------------------------------------------------------------------------
### `cdr`

#### Signatures:
```scheme
(cdr <pair>)
```

#### Description:
```
Access the second item in a pair.
```

-------------------------------------------------------------------------------
### `cons`

#### Signatures:
```scheme
(cons <car-obj> <cdr-obj>)
```

#### Description:
```
Create a pair containing <car-obj> & <cdr-obj>. Lists are created by nesting
pairs with <nil> terminating the chain. The following are equivalent:
  (cons 1 (cons 2 (cons 3 (quote ()))))
  (list 1 2 3)
```

-------------------------------------------------------------------------------
### `pair?`

#### Signatures:
```scheme
(pair? <obj>)
```

#### Description:
```
Returns whether <obj> is a pair.
```

-------------------------------------------------------------------------------
## Ports


-------------------------------------------------------------------------------
### `call-with-input-file`

#### Signatures:
```scheme
(call-with-input-file <filename-string> <unary-callable>)
```

#### Description:
```
Invoke <unary-callable> with (open-input-file <filename-string>) as its argument.
Automatically close that given port upon <unary-callable>'s return, and yield
<unary-callable>'s return value.
```

-------------------------------------------------------------------------------
### `call-with-output-file`

#### Signatures:
```scheme
(call-with-output-file <filename-string> <unary-callable>)
```

#### Description:
```
Invoke <unary-callable> with (open-output-file <filename-string>) as its argument.
Automatically close that given port upon <unary-callable>'s return, and yield
<unary-callable>'s return value.
Note that <filename-string> is cleared if it exists.
```

-------------------------------------------------------------------------------
### `call-with-output-file+`

#### Signatures:
```scheme
(call-with-output-file+ <filename-string> <unary-callable>)
```

#### Description:
```
Invoke <unary-callable> with (open-output-file <filename-string>) as its argument.
Automatically close that given port upon <unary-callable>'s return, and yield
<unary-callable>'s return value.
Note that <filename-string> is appended to if it exists.
```

-------------------------------------------------------------------------------
### `close-port!`

#### Signatures:
```scheme
(close-port! <port>)
```

#### Description:
```
Closes <port> if it hasn't been closed yet.
```

-------------------------------------------------------------------------------
### `closed-port?`

#### Signatures:
```scheme
(closed-port? <port>)
```

#### Description:
```
Returns whether <port> is closed.
```

-------------------------------------------------------------------------------
### `current-input-port`

#### Signatures:
```scheme
(current-input-port)
```

#### Description:
```
Returns the current input-port, used as the default value for <read> etc.
```

-------------------------------------------------------------------------------
### `current-output-port`

#### Signatures:
```scheme
(current-output-port)
```

#### Description:
```
Returns the current output-port, used as the default value for <write> etc.
```

-------------------------------------------------------------------------------
### `input-port?`

#### Signatures:
```scheme
(input-port? <obj>)
```

#### Description:
```
Returns whether <obj> is an input-port.
```

-------------------------------------------------------------------------------
### `open-input-file`

#### Signatures:
```scheme
(open-input-file <filename-string>)
```

#### Description:
```
Returns an input-port file handle to read from <filename-string>.
```

-------------------------------------------------------------------------------
### `open-output-file`

#### Signatures:
```scheme
(open-output-file)
(open-output-file <filename-string>)
```

#### Description:
```
Returns an output-port file handle to write to <filename-string>.
If <filename-string> exists, it is cleared.

If <filename-string> isn't given, generates a temporary file.
Access a temporary file's path via <port-path>.
Temporary files are automatically deleted upon exit by the VM.
```

-------------------------------------------------------------------------------
### `open-output-file+`

#### Signatures:
```scheme
(open-output-file+ <filename-string>)
```

#### Description:
```
Returns an output-port file handle to write to <filename-string>.
If <filename-string> exists, it is appended to.
```

-------------------------------------------------------------------------------
### `open-port?`

#### Signatures:
```scheme
(open-port? <port>)
```

#### Description:
```
Returns whether <port> is still open.
```

-------------------------------------------------------------------------------
### `output-port?`

#### Signatures:
```scheme
(output-port? <obj>)
```

#### Description:
```
Returns whether <obj> is an output-port.
```

-------------------------------------------------------------------------------
### `peek-port`

#### Signatures:
```scheme
(peek-port <input-port>)
```

#### Description:
```
Peek the first character in <input-port>. Returns #eof if empty.
```

-------------------------------------------------------------------------------
### `port-path`

#### Signatures:
```scheme
(port-path <port>)
```

#### Description:
```
Returns the <port>'s path as a string.
```

-------------------------------------------------------------------------------
### `port-position`

#### Signatures:
```scheme
(port-position <input-port>)
```

#### Description:
```
Returns a list: <input-port>'s (<line-number> <column-number>)
```

-------------------------------------------------------------------------------
### `port?`

#### Signatures:
```scheme
(port? <obj>)
```

#### Description:
```
Returns whether <obj> is a port.
```

-------------------------------------------------------------------------------
### `stdin?`

#### Signatures:
```scheme
(stdin? <input-port>)
```

#### Description:
```
Returns whether <input-port> handles the program's standard input.
```

-------------------------------------------------------------------------------
### `stdout?`

#### Signatures:
```scheme
(stdout? <output-port>)
```

#### Description:
```
Returns whether <output-port> handles the program's standard output.
```

-------------------------------------------------------------------------------
### `temp-port?`

#### Signatures:
```scheme
(temp-port? <obj>)
```

#### Description:
```
Returns whether <obj> is a port pointing to a temporary file.
Access a temporary file's path via <port-path>.
Temporary files are automatically deleted upon exit by the VM.
```

-------------------------------------------------------------------------------
### `with-input-from-file`

#### Signatures:
```scheme
(with-input-from-file <filename-string> <thunk-callable>)
```

#### Description:
```
Invoke <thunk-callable> with (open-input-file <filename-string>) as the
current-input-port. Automatically close that given port upon <thunk-callable>'s
return, and yield <thunk-callable>'s return value.
```

-------------------------------------------------------------------------------
### `with-output-to-file`

#### Signatures:
```scheme
(with-output-to-file <filename-string> <thunk-callable>)
```

#### Description:
```
Invoke <thunk-callable> with (open-output-file <filename-string>) as the
current-output-port. Automatically close that given port upon <thunk-callable>'s
return, and yield <thunk-callable>'s return value.
Note that <filename-string> is cleared if it exists.
```

-------------------------------------------------------------------------------
### `with-output-to-file+`

#### Signatures:
```scheme
(with-output-to-file+ <filename-string> <thunk-callable>)
```

#### Description:
```
Invoke <thunk-callable> with (open-output-file <filename-string>) as the
current-output-port. Automatically close that given port upon <thunk-callable>'s
return, and yield <thunk-callable>'s return value.
Note that <filename-string> is appended to if it exists.
```

-------------------------------------------------------------------------------
## Strings


-------------------------------------------------------------------------------
### `string`

#### Signatures:
```scheme
(string <obj> ...)
```

#### Description:
```
Create a new string by appending each displayed argument together.

Represents a Java <string> under the hood (hence immutable).
Literals are denoted via double-quotes.

Strings support the following control characters:
  1) "\t": tab,             represented as a char by #\tab
  2) "\n": newline,         represented as a char by #\newline
  3) "\f": form feed,       represented as a char by #\page
  4) "\r": carriage return, represented as a char by #\return
  5) "\b": backspace,       represented as a char by #\backspace

Octal literals may be used by prefixing up to 6 octal digits with "\", ranging from
\0-\177777 (0-65535 in decimal). This range ensures that each value fits neatly
within a single 16bit Java char internally.
  => Note this extends Java's octals, which only support \0-\377 (0-255 in decimal).

Java 16bit unicode literals may be used by prefixing up to 4 hex digits with "\u".
  => Adjacent unicode literals may be used to create "surrogate pairs" that render
     as a single unicode image for unicode values that require 32bit encoding.

EScheme also extends Java unicode literals with syntax for 32bit unicode values.
Prefixing up to 8 hex digits with "\U" compiles to 2 seperate "\u" instances.
  => For example, both "\U1f608" and "\ud83d\ude08" create the same string, but the
     former is easier to write out after referencing the "U+" code from the internet.
```

-------------------------------------------------------------------------------
### `string-ci<=?`

#### Signatures:
```scheme
(string-ci<=? <string> ...)
```

#### Description:
```
Returns whether "<string> <string> ..." are <= one another (case-insensitive).
```

-------------------------------------------------------------------------------
### `string-ci<?`

#### Signatures:
```scheme
(string-ci<? <string> ...)
```

#### Description:
```
Returns whether "<string> <string> ..." are < one another (case-insensitive).
```

-------------------------------------------------------------------------------
### `string-ci=?`

#### Signatures:
```scheme
(string-ci=? <string> ...)
```

#### Description:
```
Returns whether "<string> <string> ..." are equal to one another (case-insensitive).
```

-------------------------------------------------------------------------------
### `string-ci>=?`

#### Signatures:
```scheme
(string-ci>=? <string> ...)
```

#### Description:
```
Returns whether "<string> <string> ..." are >= one another (case-insensitive).
```

-------------------------------------------------------------------------------
### `string-ci>?`

#### Signatures:
```scheme
(string-ci>? <string> ...)
```

#### Description:
```
Returns whether "<string> <string> ..." are > one another (case-insensitive).
```

-------------------------------------------------------------------------------
### `string-contains`

#### Signatures:
```scheme
(string-contains <string> <substring>)
```

#### Description:
```
Returns the first index of <substring> in <string> if present, or #f if it isn't.
```

-------------------------------------------------------------------------------
### `string-contains-right`

#### Signatures:
```scheme
(string-contains-right <string> <substring>)
```

#### Description:
```
Returns the last index of <substring> in <string> if present, or #f if it isn't.
```

-------------------------------------------------------------------------------
### `string-downcase`

#### Signatures:
```scheme
(string-downcase <string>)
```

#### Description:
```
Returns <string> entirely lower-cased.
```

-------------------------------------------------------------------------------
### `string-escape`

#### Signatures:
```scheme
(string-escape <string>)
```

#### Description:
```
Returns <string> with special characters escaped (like when printing via <write>).
Note that this escapes surrogate pairs using EScheme's custom "\U" syntax.
<string-java-escape> should be used to escape such with 2 "\u" instances.
```

-------------------------------------------------------------------------------
### `string-java-escape`

#### Signatures:
```scheme
(string-java-escape <string>)
```

#### Description:
```
Returns <string> with special characters escaped (like when printing via <write>).
Note that this escapes surrogate pairs using 2 "\u" instances.
<string-escape> should be used to escape such with EScheme's custom "\U" syntax.
```

-------------------------------------------------------------------------------
### `string-java-length`

#### Signatures:
```scheme
(string-java-length <string>)
```

#### Description:
```
Returns the length of <string>, with surrogate pairs counting as 2 chars.
```

-------------------------------------------------------------------------------
### `string-join`

#### Signatures:
```scheme
(string-join <string-list>)
(string-join <string-list> <joiner-string>)
```

#### Description:
```
Returns a string made from joining the strings in <string-list> by splicing
<joiner-string> (defaults to "") between each item.
```

-------------------------------------------------------------------------------
### `string-prefix?`

#### Signatures:
```scheme
(string-prefix? <string> <prefix-string>)
```

#### Description:
```
Returns whether <string> starts with <prefix-string>. Also see <string-suffix?>.
```

-------------------------------------------------------------------------------
### `string-replace`

#### Signatures:
```scheme
(string-replace <string> <regex-string> <replacement-string>)
```

#### Description:
```
Replaces all instances of <regex-string> in <string> with <replacement-string>.
```

-------------------------------------------------------------------------------
### `string-split`

#### Signatures:
```scheme
(string-split <string>)
(string-split <string> <splitter-regex-string>)
```

#### Description:
```
Returns a list of strings made from splitting <string> at each
<splitter-regex> instance. Defaults to splitting into characters.
```

-------------------------------------------------------------------------------
### `string-suffix?`

#### Signatures:
```scheme
(string-suffix? <string> <suffix-string>)
```

#### Description:
```
Returns whether <string> ends with <suffix-string>. Also see <string-prefix?>.
```

-------------------------------------------------------------------------------
### `string-trim`

#### Signatures:
```scheme
(string-trim <string>)
```

#### Description:
```
Returns a string with the whitespace removed from both ends of <string>.
```

-------------------------------------------------------------------------------
### `string-unescape`

#### Signatures:
```scheme
(string-unescape <string>)
```

#### Description:
```
Returns <string> with special characters unescaped (like when printing via
<display>). Note that this also unescapes EScheme's custom "\U" syntax.
```

-------------------------------------------------------------------------------
### `string-unfold`

#### Signatures:
```scheme
(string-unfold <break?-callable> <mapper-callable> <update-callable> <seed>)
```

#### Description:
```
Unfolds a string from left to right, starting with <seed>. <break?-condition>
determines when unfolding stops, <mapper-callable> maps the <seed> to a value
in the unfolded string, and <update-callable> increments <seed> for the
next round of unfolding.

Note that the result of <mapper-callable> must always be a character.
```

-------------------------------------------------------------------------------
### `string-unfold-right`

#### Signatures:
```scheme
(string-unfold <break?-callable> <mapper-callable> <update-callable> <seed>)
```

#### Description:
```
Unfolds a string from right to left, starting with <seed>. <break?-condition>
determines when unfolding stops, <mapper-callable> maps the <seed> to a value
in the unfolded string, and <update-callable> increments <seed> for the
next round of unfolding.

Note that the result of <mapper-callable> must always be a character.
```

-------------------------------------------------------------------------------
### `string-upcase`

#### Signatures:
```scheme
(string-upcase <string>)
```

#### Description:
```
Returns <string> entirely upper-cased.
```

-------------------------------------------------------------------------------
### `string<=?`

#### Signatures:
```scheme
(string<=? <string> ...)
```

#### Description:
```
Returns whether "<string> <string> ..." are <= one another (case-sensitive).
```

-------------------------------------------------------------------------------
### `string<?`

#### Signatures:
```scheme
(string<? <string> ...)
```

#### Description:
```
Returns whether "<string> <string> ..." are < one another (case-sensitive).
```

-------------------------------------------------------------------------------
### `string=?`

#### Signatures:
```scheme
(string=? <string> ...)
```

#### Description:
```
Returns whether "<string> <string> ..." are equal to one another (case-sensitive).
```

-------------------------------------------------------------------------------
### `string>=?`

#### Signatures:
```scheme
(string>=? <string> ...)
```

#### Description:
```
Returns whether "<string> <string> ..." are >= one another (case-sensitive).
```

-------------------------------------------------------------------------------
### `string>?`

#### Signatures:
```scheme
(string>? <string> ...)
```

#### Description:
```
Returns whether "<string> <string> ..." are > one another (case-sensitive).
```

-------------------------------------------------------------------------------
### `string?`

#### Signatures:
```scheme
(string? <obj>)
```

#### Description:
```
Returns whether <obj> is a string.
```

-------------------------------------------------------------------------------
### `stringf`

#### Signatures:
```scheme
(stringf <format-string> <arg> ...)
```

#### Description:
```
Returns a new string created from formatting <format-string> with "<arg> ...".
>> <format-string> is like Java's printf with unique formatting patterns:
   ----------------------------------------------------------------------
   %a = display anything
   %wa = write anything
   %pa = pretty-print anything
   ----------------------------------------------------------------------
   %... = display unpacked list/vector/hashmap
   %w... = write unpacked list/vector/hashmap
   %p... = pretty-print unpacked list/vector/hashmap
   ----------------------------------------------------------------------
   %n = number
   %+n = number (show sign if positive too)
   %,n = number with commas
   %En = %en = number (coerced to exact)
   %In = %in = number (coerced to inexact)
   %#rn = %#Rn = number (in radix <#> from 2 to 36)
   %#n = number (left-padded with 0s to a width of <#> characters)
   %.#n = number (with <#> digits of precision)
   -> IE "%+e2rn": make exact in binary with sign
   -> NOTE: 1) 0-padding & precision MUST be of 2 digits or less!
            2) Can't have radix with I-coercion or precision!
   ----------------------------------------------------------------------
   %$ = display real finite as a dollar value
   %,$ = display real finite as a dollar value seperated by commas
   ----------------------------------------------------------------------
   %s = display string
   %#s = display string & pad left with # spaces
   %-#s = display string & pad right with # spaces
   %ws = write string
   -> NOTE: padding MUST be of 3 digits or less (ie from -999 to 999)
   ----------------------------------------------------------------------
   %b  = bool
   %wb = write "true" or "false" instead of "#t" or "#f"
   ----------------------------------------------------------------------
   %%  = "%" (escapes a "%")
   ----------------------------------------------------------------------
```

-------------------------------------------------------------------------------
## Symbols


-------------------------------------------------------------------------------
### `symbol?`

#### Signatures:
```scheme
(symbol? <obj>)
```

#### Description:
```
Returns whether <obj> is a symbol.
```

-------------------------------------------------------------------------------
## System


-------------------------------------------------------------------------------
### `call-stack`

#### Signatures:
```scheme
(call-stack)
```

#### Description:
```
Returns the current call-stack (prior to calling the primitive) as an
associative list: ((<function-name-string> <source-information>) ...)
  * <source-information> := #f ; if doesn't exist, else:
                          | (<filename-string> <line-number> <column-number>)
```

-------------------------------------------------------------------------------
### `escm`

#### Signatures:
```scheme
(escm <escm-file>)
(escm <escm-file> <argv> ...)
(escm <millisecond-timeout> <escm-file>)
(escm <millisecond-timeout> <escm-file> <argv> ...)
```

#### Description:
```
Execute an EScheme program in a seperate process. Effectively a wrapper 
around <system> that references <*escm-execution-command*>. Displays each 
<argv> to generate a single command string with <escm-file>.

If <millisecond-timeout> (a real number) is given and exceeded, the
spawned process will terminate. Note that this is NOT a hard cap 
though: hence passing 0 as <millisecond-timeout> may still have 
system-wide side effects.

Returns a list:
  (<program-stdout-str> <program-stderr-str> <program-exit-code>)
```

-------------------------------------------------------------------------------
### `exit`

#### Signatures:
```scheme
(exit)
(exit <integer-code>)
```

#### Description:
```
Terminate the current EScheme session with <integer-code> (defaults to 0).
```

-------------------------------------------------------------------------------
### `garbage-collector`

#### Signatures:
```scheme
(garbage-collector)
```

#### Description:
```
Hints the JVM to launch its garbage collector (GC). Does not guarentee
immediate GC execution.
```

-------------------------------------------------------------------------------
### `getenv`

#### Signatures:
```scheme
(getenv)
(getenv <variable-name-string>)
```

#### Description:
```
If given no arguments, return a hashmap of name:value string environment 
variable associations.

If given an environment variable name string, returns its string value. 
If the given string is not an accessable environment variable, returns #f
```

-------------------------------------------------------------------------------
### `load`

#### Signatures:
```scheme
(load <filename-string>)
(load <directory-string> <filename-string>)
```

#### Description:
```
Reads and evaluates <filename-str>'s EScheme contents in the global environment.
Works for both regular & <serialize>d EScheme files.
If given <directory-str>, loads <filename-str> from <directory-str>. Use:

  (load #path <filename-str>)

as a portable alternative to (load <filename-str>) if <filename-str> is a
relative path, since <load> only operates relative to (current-directory).

Note that <load-once> should be preferred to prevent cyclic loading.
```

-------------------------------------------------------------------------------
### `load-once`

#### Signatures:
```scheme
(load-once <filename-string>)
(load-once <directory-string> <filename-string>)
```

#### Description:
```
Works exactly like <load>, but only loads unloaded files. Use:

  (load-once #path <filename-str>)

as a portable alternative to (load-once <filename-str>) if <filename-str> is a
relative path, since <load-once> only operates relative to (current-directory).
```

-------------------------------------------------------------------------------
### `module-bindings`

#### Signatures:
```scheme
(module-bindings <module>)
```

#### Description:
```
Returns a list of the symbols defined in <module>.
Be warned: every module has its own copy of the standard library defined too!
```

-------------------------------------------------------------------------------
### `module-path`

#### Signatures:
```scheme
(module-path <module>)
```

#### Description:
```
Returns the absolute file path of <module>'s original location.
```

-------------------------------------------------------------------------------
### `module?`

#### Signatures:
```scheme
(module? <obj>)
```

#### Description:
```
Returns whether <obj> is a module object.
```

-------------------------------------------------------------------------------
### `system`

#### Signatures:
```scheme
(system <command-str>)
(system <command-str> <env-var-str-list>)
(system <command-str> <directory-str>)
(system <command-str> <env-var-str-list> <directory-str>)
(system <millisecond-timeout> <command-str>)
(system <millisecond-timeout> <command-str> <env-var-str-list>)
(system <millisecond-timeout> <command-str> <directory-str>)
(system <millisecond-timeout> <command-str> <env-var-str-list> <directory-str>)
```

#### Description:
```
Executes a command, using the environment variable bindings in
<env-var-str-list> (defaults to those of the current environment), 
in the <directory-str> directory (defaults to the current working 
directory).

Note that each environment variable string in <env-var-str-list>
should follow the "name=value" format.

If <millisecond-timeout> (a real number) is given and exceeded, the
spawned process will terminate. Note that this is NOT a hard cap 
though: hence passing 0 as <millisecond-timeout> may still have 
system-wide side effects.

Ultimately passed to Java's <Runtime.getRuntime().exec()>.
Referenced by <escm>.

Returns a list:
  (<command-stdout-str> <command-stderr-str> <command-exit-code>)
```

-------------------------------------------------------------------------------
## Type-Coercions


-------------------------------------------------------------------------------
### `char->integer`

#### Signatures:
```scheme
(char->integer <character>)
```

#### Description:
```
Convert a character to an integer codepoint value.
```

-------------------------------------------------------------------------------
### `display-to-string`

#### Signatures:
```scheme
(display-to-string <obj>)
```

#### Description:
```
Write <obj> to a string in human-readable form.
```

-------------------------------------------------------------------------------
### `hashmap->list`

#### Signatures:
```scheme
(hashmap->list <hashmap>)
```

#### Description:
```
Convert a hashmap to a list of keys & values.
```

-------------------------------------------------------------------------------
### `hashmap->vector`

#### Signatures:
```scheme
(hashmap->vector <hashmap>)
```

#### Description:
```
Convert a hashmap to a vector of keys & values.
```

-------------------------------------------------------------------------------
### `integer->char`

#### Signatures:
```scheme
(integer->char <integer>)
```

#### Description:
```
Convert an integer codepoint value to a character.
```

-------------------------------------------------------------------------------
### `keyword->string`

#### Signatures:
```scheme
(keyword->string <keyword>)
```

#### Description:
```
Convert a keyword to a string.
```

-------------------------------------------------------------------------------
### `keyword->symbol`

#### Signatures:
```scheme
(keyword->symbol <keyword>)
```

#### Description:
```
Convert a keyword to a symbol.
```

-------------------------------------------------------------------------------
### `list->hashmap`

#### Signatures:
```scheme
(list->hashmap <list>)
```

#### Description:
```
Convert a list of keys & values to a hashmap.
```

-------------------------------------------------------------------------------
### `list->string`

#### Signatures:
```scheme
(list->string <list>)
```

#### Description:
```
Convert a list of chars to a string.
```

-------------------------------------------------------------------------------
### `list->vector`

#### Signatures:
```scheme
(list->vector <list>)
```

#### Description:
```
Convert a list to a vector.
```

-------------------------------------------------------------------------------
### `number->string`

#### Signatures:
```scheme
(number->string <number>)
(number->string <number> <radix>)
```

#### Description:
```
Convert a number to a string in base <radix> (defaults to 10).
<radix> must be between <*min-radix*> and <*max-radix*>.
Returns #f if failed coercion.
```

-------------------------------------------------------------------------------
### `pretty-print-to-string`

#### Signatures:
```scheme
(pretty-print-to-string <obj>)
```

#### Description:
```
Write <obj> to a string in indented, machine-readable form.
Aliased by <pprint-to-string>.
```

-------------------------------------------------------------------------------
### `string->keyword`

#### Signatures:
```scheme
(string->keyword <string>)
```

#### Description:
```
Convert a string to a keyword.
```

-------------------------------------------------------------------------------
### `string->list`

#### Signatures:
```scheme
(string->list <string>)
```

#### Description:
```
Convert a string to a list of chars.
```

-------------------------------------------------------------------------------
### `string->number`

#### Signatures:
```scheme
(string->number <string>)
(string->number <string> <radix>)
```

#### Description:
```
Convert a string to a number in base <radix> (defaults to 10).
<radix> must be between <*min-radix*> and <*max-radix*>.
Returns #f if failed coercion.
```

-------------------------------------------------------------------------------
### `string->symbol`

#### Signatures:
```scheme
(string->symbol <string>)
```

#### Description:
```
Convert a string to a symbol.
```

-------------------------------------------------------------------------------
### `string->vector`

#### Signatures:
```scheme
(string->vector <string>)
```

#### Description:
```
Convert a string to a vector of chars.
```

-------------------------------------------------------------------------------
### `symbol->keyword`

#### Signatures:
```scheme
(symbol->keyword <symbol>)
```

#### Description:
```
Convert a symbol to a keyword.
```

-------------------------------------------------------------------------------
### `symbol->string`

#### Signatures:
```scheme
(symbol->string <symbol>)
```

#### Description:
```
Convert a symbol to a string.
```

-------------------------------------------------------------------------------
### `vector->hashmap`

#### Signatures:
```scheme
(vector->hashmap <vector>)
```

#### Description:
```
Convert a vector of keys & values to a hashmap.
```

-------------------------------------------------------------------------------
### `vector->list`

#### Signatures:
```scheme
(vector->list <vector>)
```

#### Description:
```
Convert a vector to a list.
```

-------------------------------------------------------------------------------
### `vector->string`

#### Signatures:
```scheme
(vector->string <vector>)
```

#### Description:
```
Convert a vector of chars to a string.
```

-------------------------------------------------------------------------------
### `write-to-string`

#### Signatures:
```scheme
(write-to-string <obj>)
```

#### Description:
```
Write <obj> to a string in machine-readable form.
```

-------------------------------------------------------------------------------
## Utilities


-------------------------------------------------------------------------------
### `call-with-current-continuation`

#### Signatures:
```scheme
(call-with-current-continuation <unary-callable>)
```

#### Description:
```
Calls <unary-callable> with the current unary continuation as its argument.
Aliased by <call/cc>.
```

-------------------------------------------------------------------------------
### `call-with-values`

#### Signatures:
```scheme
(call-with-values <producer-thunk-callable> <consumer-callable>)
```

#### Description:
```
Applies the <values> object returned by <producer-thunk> as args to <consumer>.
Used in conjunction with <values>.
See the <let-values> macro for a simplfied syntax to bind <value> expressions.
For example:
  ;; Returns '(james . bond)
  (call-with-values
    (lambda () (values 'bond 'james))
    (lambda (x y) (cons y x)))
```

-------------------------------------------------------------------------------
### `copy`

#### Signatures:
```scheme
(copy <obj>)
```

#### Description:
```
Returns a shallow (structural) copy of <obj>.
```

-------------------------------------------------------------------------------
### `dynamic-wind`

#### Signatures:
```scheme
(dynamic-wind <enter-thunk-callable>
  <body-thunk-callable>
  <exit-thunk-callable>)
```

#### Description:
```
Executes <enter-thunk>, <body-thunk>, <exit-thunk> in that order. <exit-thunk>
is guarenteed to execute even if <body-thunk> escapes via a continuation.
```

-------------------------------------------------------------------------------
### `error`

#### Signatures:
```scheme
(error <reason> <arg> ...)
```

#### Description:
```
Triggers a fatal error, with <reason> displayed and "<arg> ..." written.
```

-------------------------------------------------------------------------------
### `errorf`

#### Signatures:
```scheme
(errorf <format-string> <arg> ...)
```

#### Description:
```
Triggers a fatal error, with <format-string> displayed & formatted with "<arg> ...".
See <stringf> for more information on formatting.
```

-------------------------------------------------------------------------------
### `force`

#### Signatures:
```scheme
(force <delayed>)
```

#### Description:
```
Forces an expression delayed by the <delay> macro to be evaluated.
```

-------------------------------------------------------------------------------
### `raise`

#### Signatures:
```scheme
(raise <obj>)
```

#### Description:
```
Raises an exception by passing <obj> to the <unary-callable-handler>
of <with-exception-handler>, or as the variable of <guard>.
For example:
  ;; Prints <'an-error>:
  (call-with-current-continuation
   (lambda (k)
     (with-exception-handler (lambda (x)
                               (display "condition: ")
                               (write x)
                               (newline)
                               (k 'exception))
       (lambda ()
         (+ 1 (raise 'an-error))))))
```

-------------------------------------------------------------------------------
### `serialize`

#### Signatures:
```scheme
(serialize <escm-file-path> <serialized-file-path>)
```

#### Description:
```
Load and serialize the contents of <escm-file-path> into <serialized-file-path>.

Serialization works by storing the Java bytecode of the assembled instruction
set object holding <escm-file-path>'s EScheme bytecode in <serialized-file-path>.
The contents must first be evaluated in order to expand macros properly during
compilation.

Totally optional to use, this facility is provided in order to serve as a portable 
obfuscation tactic to generate a rough equivalent to "EScheme binaries". 

However, it is important to note that - strangely enough - serialized files tend to 
load _SLOWER_ than regular EScheme files. Despite allowing us to circumvent the 
reading, compilation, and assembly of EScheme code, it turns out that EScheme's 
facilities to do so are all actually so fast as to beat out Java's native 
deserialization functions!

Hence, only serialize your EScheme code if you'd like to make the file unreadable
for humans.

All EScheme objects serialize just as you'd imagine, save for Threads and Ports.
By default, Java doesn't enable these items to be serialized since they rely on
system-dependant components that don't translate to a different machine's processes.
However, EScheme allows such to occur using the following rules:

  1. Threads
     * Threads serialize to their default, "pre-run" state. This means that
       serialized threads save their name and runnable-callable. It does _NOT_
       save any information about the executing sub-process if serialized mid-run.
  2. Output-Ports
     * Output-Ports serialize their absolute file path, and always deserialize
       without appending (as if created via <open-output-file>).
       - This is because deserialization re-loads the entire script containing
         the port, and hence any writing operations conducted by the script ought
         to reoccur during the script's loading process.
     * Hence Output-Ports _MUST_ be serialized & deserialized on a machine with the
       same directory layout, to avoid errors with the stored absolute-path!
  3. Input-Ports
     * Input-Ports serialize their absolute file path, and always deserialize with
       their current line & column both set to 1 (reading from the start of the file).
     * Hence Input-Ports _MUST_ be serialized & deserialized on a machine with the
       same directory layout, to avoid errors with the stored absolute-path!

OF NOTE: The semantics of thread/port serialization won't be an issue for 99.99999%
*******  of serialized programs that use macros in a remotely sane way.

         Only by intentionally returning a value that contains an initialized thread
         or port datum from a macro can one tease out this behavior.

         Given that most macros only return code as data structures (rather than
         data values), these semantics are only something worth thinking about
         if you're intentionally employing some genuinely tricky (& almost
         certainly questionable) EScheme meta-programming techniques.

Use <serialize-module> to instead load <escm-file-path> (as part of its 
compilation) in a unique environment, rather than in the current one (as 
done by <serialize>).

Load serialized EScheme code with the <load> primitive or <import> macro.
```

-------------------------------------------------------------------------------
### `serialize-module`

#### Signatures:
```scheme
(serialize-module <escm-file-path> <serialized-file-path>)
```

#### Description:
```
Operates exactly like <serialize>, but loads <escm-file-path> in a seperate
environment rather than the current one.

See <serialize> for details on the serialization process, and <serialized?>
to learn how to check whether a file is serialized or not.

Load serialized EScheme code with the <load> primitive or <import> macro.
```

-------------------------------------------------------------------------------
### `serialized?`

#### Signatures:
```scheme
(serialized? <file-path-string>)
```

#### Description:
```
Returns whether <file-path> is a serialized file created by <serialize> or
<serialize-module>.
```

-------------------------------------------------------------------------------
### `time`

#### Signatures:
```scheme
(time <callable> <arg> ...)
```

#### Description:
```
Returns a list after applying <callable> to "<arg> ...":
  (<execution-time-in-milliseconds> <result>)
```

-------------------------------------------------------------------------------
### `typeof`

#### Signatures:
```scheme
(typeof <obj>)
```

#### Description:
```
Returns <obj>'s intrinsic type name as a symbol. Note that it does not 
return a keyword type as described in <type-system> from <Topics>: for 
example, (typeof 42) yields 'number despite also matching :int
```

-------------------------------------------------------------------------------
### `values`

#### Signatures:
```scheme
(values <obj> ...)
```

#### Description:
```
Packs <obj> into a "value object" list.
Used in conjunction with <call-with-values>.
See the <let-values> macro for a simplfied syntax to bind <value> expressions.
For example:
  ;; Returns '(james . bond)
  (call-with-values
    (lambda () (values 'bond 'james))
    (lambda (x y) (cons y x)))
```

-------------------------------------------------------------------------------
### `with-exception-handler`

#### Signatures:
```scheme
(with-exception-handler <unary-callable-handler> <thunk-callable>)
```

#### Description:
```
Calls <thunk-callable>. If an exception is raised via <raise>,
that exception is passed to <unary-callable-handler>.
Note that the handler should end by calling an escape continuation.
Also look into <guard> for nicer exception-handling syntax via macros.
For example:
  ;; Prints <'an-error>:
  (call-with-current-continuation
   (lambda (k)
     (with-exception-handler (lambda (x)
                               (display "condition: ")
                               (write x)
                               (newline)
                               (k 'exception))
       (lambda ()
         (+ 1 (raise 'an-error))))))
```

-------------------------------------------------------------------------------
## Vectors


-------------------------------------------------------------------------------
### `make-vector`

#### Signatures:
```scheme
(make-vector <length> <fill-value>)
```

#### Description:
```
Construct a vector of <length> instances of <fill-value>.
```

-------------------------------------------------------------------------------
### `vector`

#### Signatures:
```scheme
(vector <obj> ...)
```

#### Description:
```
Construct a vector containing "<obj> ...".
Create vector literals via the [<item> ...] syntax.
Vectors are applicable to an index to get an entry: (<vector> <index>)
```

-------------------------------------------------------------------------------
### `vector-append!`

#### Signatures:
```scheme
(vector-append! <vector> ...)
```

#### Description:
```
Append vectors to <vector> (thereby mutating <vector>).
```

-------------------------------------------------------------------------------
### `vector-delete!`

#### Signatures:
```scheme
(vector-delete! <vector> <index>)
```

#### Description:
```
Deletes the entry at <index> in <vector>. Returns the deleted item.
```

-------------------------------------------------------------------------------
### `vector-fill!`

#### Signatures:
```scheme
(vector-fill! <vector> <fill-value>)
```

#### Description:
```
Fill all of <vector> with <fill-value>.
```

-------------------------------------------------------------------------------
### `vector-grow!`

#### Signatures:
```scheme
(vector-grow! <vector> <length> <fill-value>)
```

#### Description:
```
Grow <vector> by <length> items set to <fill-value>.
```

-------------------------------------------------------------------------------
### `vector-insert!`

#### Signatures:
```scheme
(vector-insert! <vector> <index> <obj>)
```

#### Description:
```
Insert <obj> at <index> in <vector>, shifting back following items as needed.
```

-------------------------------------------------------------------------------
### `vector-member`

#### Signatures:
```scheme
(vector-member <vector> <obj>)
```

#### Description:
```
Returns the index of <obj> in <vector>, or #f if its missing.
Uses <equal?> for comparisons.
```

-------------------------------------------------------------------------------
### `vector-memq`

#### Signatures:
```scheme
(vector-memq <vector> <obj>)
```

#### Description:
```
Returns the index of <obj> in <vector>, or #f if its missing.
Uses <eq?> for comparisons.
```

-------------------------------------------------------------------------------
### `vector-pop!`

#### Signatures:
```scheme
(vector-pop! <vector>)
```

#### Description:
```
Pops <obj> from the back of <vector>. Returns the popped item.
```

-------------------------------------------------------------------------------
### `vector-pop-front!`

#### Signatures:
```scheme
(vector-pop-front! <vector>)
```

#### Description:
```
Pops <obj> from the front of <vector>. Returns the popped item.
```

-------------------------------------------------------------------------------
### `vector-push!`

#### Signatures:
```scheme
(vector-push! <vector> <obj>)
```

#### Description:
```
Pushes <obj> to the back of <vector>.
```

-------------------------------------------------------------------------------
### `vector-push-front!`

#### Signatures:
```scheme
(vector-push-front! <vector> <obj>)
```

#### Description:
```
Pushes <obj> to the front of <vector>.
```

-------------------------------------------------------------------------------
### `vector-set!`

#### Signatures:
```scheme
(vector-set! <vector> <index> <obj>)
```

#### Description:
```
Sets the entry at <index> in <vector> to be <obj>. Note this can also be done
by directly applying the vector to the index and new value (like a function).
```

-------------------------------------------------------------------------------
### `vector-unfold`

#### Signatures:
```scheme
(vector-unfold <break?-callable> <mapper-callable> <update-callable> <seed>)
```

#### Description:
```
Unfolds a vector from left to right, starting with <seed>. <break?-condition>
determines when unfolding stops, <mapper-callable> maps the <seed> to a value
in the unfolded vector, and <update-callable> increments <seed> for the
next round of unfolding.
```

-------------------------------------------------------------------------------
### `vector-unfold-right`

#### Signatures:
```scheme
(vector-unfold-right <break?-callable>
  <mapper-callable>
  <update-callable>
  <seed>)
```

#### Description:
```
Unfolds a vector from right to left, starting with <seed>. <break?-condition>
determines when unfolding stops, <mapper-callable> maps the <seed> to a value
in the unfolded vector, and <update-callable> increments <seed> for the
next round of unfolding.
```

-------------------------------------------------------------------------------
### `vector?`

#### Signatures:
```scheme
(vector? <obj>)
```

#### Description:
```
Returns whether <obj> is a vector.
```

-------------------------------------------------------------------------------
## Void


-------------------------------------------------------------------------------
### `void?`

#### Signatures:
```scheme
(void? <obj>)
```

#### Description:
```
Returns whether <obj> has type #<void>.
Returned by mutative actions like <set!>.
```

-------------------------------------------------------------------------------
# Syntax


-------------------------------------------------------------------------------
## Core


-------------------------------------------------------------------------------
### `-<>`

#### Signatures:
```scheme
(-<> <expression> ...)
```

#### Description:
```
Execute each expression, with "<>" bound as the result of the last expression.
For example:
  ; The below results in 64:
  (-<> (* 2 2)
       (+ <> <>)
       (* <> <>))
```

-------------------------------------------------------------------------------
### `and`

#### Signatures:
```scheme
(and <obj> ...)
```

#### Description:
```
Returns whether none of "<obj> ..." are #f.
```

-------------------------------------------------------------------------------
### `begin`

#### Signatures:
```scheme
(begin <expression> ...)
```

#### Description:
```
Combines a series of EScheme expressions into a single expression.
Similar to how {} is used in C++/Java, BUT <begin> does NOT have
its own scope (use <let> for such instead).
```

-------------------------------------------------------------------------------
### `bytecode`

#### Signatures:
```scheme
(bytecode <instruction> ...)
```

#### Description:
```
# An Introduction to EScheme's Bytecode

## Brief Insight

EScheme's bytecode structure can be thought of as a chain of instruction sets
serving as function bodies.

The instructions (as outlined below) are designed **exclusively** to serve as a
means by which to perform control flow and bind items to the current environment.

**_All logical operations are abstracted away via user-defined & primitive functions!_**

Such is the crux of our incredibly reduced instruction set, and the fact that
we can get away with having all of our instructions be either unary or nullary.
For example, unlike most flavors of assembly, there is no `add` instruction.
Rather, to us, `+` is called like any other function.

## Data Manipulation

All data is handled via 2 mediums in our bytecode: the "current value register"
(CVR), and the function stack.

The CVR psuedo-register is never explicitely referenced in EScheme bytecode,
rather it is implicitly used by certain instructions. For example, compiling
`(define life 42)` yields the bytecode `(load 42) (define life)`. The `load`
instruction implicitly sets CVR to its argument, and `define` implicitly binds
its argument to CVR in the environment.

The function stack is referred to via the `push`, `pop`, and `call` instructions.
`call` affects the stack because, due to our lack of multiple registers, we push
our arguments (and the function we're calling) onto the stack rather than putting
them in registers. More details on the `call` instruction may be found below.

## Why Write Bytecode?

The vast majority of the bytecode that the interpreter works with is the result of
compiling EScheme expressions. However, it may be desirable to write out bytecode
by hand under very specific circumstances.

For example, while EScheme certainly supports iteration via recursion, there is no
"true" iteration by default a la `for`/`while` loop in Java or C++. However, by
leveraging inlined bytecode, we can write a `while` macro that **does** have true
iteration (such is outlined as an example below)!

## Writing Bytecode

In order to write out bytecode, simply follow the instruction syntax outlined below.
Note that any arguments specified as `<symbol>` or `<integer>` MUST be symbolic or
integer literals respectively. For example, `jump` and `call` both accept integers,
but whereas `jump` requires that it's argument is an integer literal, `call` also
accepts variables that evaluate to integers.

### NIL

A note on writing NIL literals: use `()`. For example, `(define n ())`
compiles to `(load ()) (define n)`. You may alternatively use the `#nil` reader
literal.

### VOID

A note on writing VOID literals: use `#void`. For example, `(define n #void)`
compiles to `(load #void) (define n)`.

### Closures

A note on writing closures: you may nest the `load-closure` syntax as needed in order
to denote closures. The `load-closure` syntax is as follows:

```
(load-closure
  <optional-docstring>
  (<optional-return-type> (<optional-type> <argument> ...)
    <instruction> ...) ...)
```

This syntax will load into CVR the equivalent of:

```
(fn
  <optional-docstring>
  (<optional-return-type> (<optional-type> <argument> ...)
    <instruction> ...) ...)
```

Further note that closure expressions by default return the value left in
CVR upon terminating execution. `<docstring>` may be optionally provided to yield
further information on the closure's intended purpose in the `help` menu. Types
may be provided as keywords to perform runtime checks on function arguments and
return values.

For example, you may write:

```scheme
(define (my-function y)
  (map (lambda (x) (* x y)) (list 1 2 3)))
```

As:

```
(load-closure
  ((y)
    (push map)
    (load-closure
      ((x)
        (push *)
        (push x)
        (push y)
        (call 3)))
    (push)
    (push list)
    (push 1)
    (push 2)
    (push 3)
    (call 4)
    (push)
    (call 3)))
(define my-function)
```

### Object Access Chains

The escm VM has built-in support for interpretting object access chains, hence
`(define obj.prop1.prop2)` is perfectly valid bytecode syntax.

As such, all instructions that set or evaluate a symbolic datum support this
syntax. These include: `define`, `set!`, `load`, `call`, `push`, & `return`.

---

# The EScheme Bytecode Instruction Set

```
(define <symbol>) ; bind <symbol> to CVR [sets CVR to <void>]

(set! <symbol>) ; set! <symbol> to CVR [sets CVR to <void>]

(defined? <symbol>) ; determine if <symbol> is defined as a variable [sets CVR to the boolean result]

(ifn <integer>) ; if CVR is NOT truthy, jump <integer> instructions [sets CVR to <void>]

(jump <integer>) ; jump <integer> instructions

(quote <datum>) ; quote <datum> and load it into CVR. may recurse infinitely for cyclic vectors/hashmaps.

(load <datum>) ; evaluate <datum> and load it into CVR

(call <datum>) ; <datum> must evaluate to an integer. get the fcn & arguments being applied from the stack.
               ; positive <integer> denotes pushes from left to right & negative denotes pushes from right
               ; to left (when compiling the application expressions). pops (abs <datum>) items off of the
               ; stack after the call, and places the returned value of the fcn application in CVR.

(push)         ; push CVR to the stack
(push <datum>) ; push <datum> to the stack

(pop) ; pop a value off of the stack into CVR

(return)         ; returns the value in CVR (effectively jumps to the end of the function)
(return <datum>) ; returns the value in <datum> (effectively jumps to the end of the function)
```

---

## Helper `bytecode` Syntax

### Psuedo-instruction(s) converted into "real" instruction(s) by the assembler

```
(load-closure ; syntax to load a closure
  <optional-docstring>
  (<optional-return-type> (<optional-type> <argument> ...)
    <instruction> ...) ...)
```

---

# Examples

```scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Demoing the compilation of closures

;; Source Code
(define (my-function y)
  (map (lambda (x) (* x y)) (list 1 2 3)))

;; Compiled Bytecode
(load-closure
  ((y)
    (push map)
    (load-closure
      ((x)
        (push *)
        (push x)
        (push y)
        (call 3)))
    (push)
    (push list)
    (push 1)
    (push 2)
    (push 3)
    (call 4)
    (push)
    (call 3)))
(define my-function)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defining a "(while (<condition> <return-expr> ...) <body> ...)" macro using
;; inlined bytecode & compilation

(define-syntax while
  (lambda (condition-returns . body)
    (define compiled-condition (compile (car condition-returns)))
    (define compiled-body (apply append (map compile body)))
    (define compiled-returns (apply append (map compile (cdr condition-returns))))
    `(bytecode
      ,@compiled-condition
      (ifn ,(+ (length compiled-body) 2))
      ,@compiled-body
      (jump ,(- 0 (length compiled-condition) (length compiled-body) 1))
      (load #void)
      ,@compiled-returns)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing "apply" in bytecode (impossible in native Scheme!)

(load-closure
  ((f args-list)
    (load 1)
    (define count)
    (push f)
    (push null?)
    (push args-list)
    (call 2)
    (ifn 2)
    (jump 15)
    (push car)
    (push args-list)
    (call 2)
    (push) ; save an extracted value
    (push cdr)
    (push args-list)
    (call 2)
    (set! args-list)
    (push +)
    (push 1)
    (push count)
    (call 3)
    (set! count)
    (jump -18)
    (call count)))
(define apply)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing factorial (because of course we do so)

;; Source Code
(define (factorial n)
  (if (< n 2)
      1
      (* n (factorial (- n 1)))))

;; Compiled Bytecode
(load-closure
  ((n)
    (push <)
    (push n)
    (push 2)
    (call 3)
    (ifn 2)
    (return 1)
    (push *)
    (push n)
    (push factorial)
    (push -)
    (push n)
    (push 1)
    (call 3)
    (push)
    (call 2)
    (push)
    (call 3)))
(define factorial)
```
```

-------------------------------------------------------------------------------
### `case`

#### Signatures:
```scheme
(case <value>
  ((<key> ...) <expr> ...)
  ((<key> ...) => <callable>)
  (else <expr> ...)
  ...)
```

#### Description:
```
Conditional "switch" dispatch. If <value> matches any key in "<key> ...",
execute <body>. With '=>' syntax, (member <value> (list <key> ...)) is passed
to <callable> iff <value> matches any key in "<key> ...".
<else> matches against all <value>s.
```

-------------------------------------------------------------------------------
### `cond`

#### Signatures:
```scheme
(cond (<condition> <expr> ...)
  (<condition> => <callable>)
  (else <expr> ...)
  ...)
```

#### Description:
```
Conditional if-else chains. If <condition> is true, execute <body>.
With '=>' syntax, <condition> is passed to <callable> iff <condition> is #t.
<else> is equivalent to #t.
```

-------------------------------------------------------------------------------
### `curry`

#### Signatures:
```scheme
(curry (<parameter> ...) <optional-docstring> <body> ...)
```

#### Description:
```
<lambda> alternative to create a curried procedure! Suppose
(define fcn (curry (a b) a)). <fcn> may be invoked as either
(fcn 1 2) or ((fcn 1) 2).

  => NOTE: It is UNDEFINED BEHAVIOR to have a VARIADIC CURRIED procedure!

Optionally include <docstring> to yield further details on the
procedure's intended purpose in the <help> menu.

Optionally include keyword types to perform runtime type checks! Argument
types are placed prior their names, and the return type is placed prior the
parameters. For example, (curry :int (:list x) (length x)) ensures <x> is
a list, and that the <curry> returns an integer.
  => Note that the return type is only checked once all parameters have
     been applied to the curried function.
  => See <type-system> in <Topics> for more details on EScheme's types!
```

-------------------------------------------------------------------------------
### `define`

#### Signatures:
```scheme
(define <symbol> <obj>)
(define <symbol1> <symbol2> ... <symbolN> <N-length-list-expression>)
(define (<function-name> <parameter> ...) <optional-docstring> <body> ...)
```

#### Description:
```
Binds <symbol> to <obj> in the current environment. Note that <symbol> 
may be an object property chain too, hence (define obj.prop 42) is valid 
syntax!

May bind several variables to values in a list. For example:
(define a b (list 1 2)) binds variables <a> to 1 and <b> to 2.
  * Alias <list> with <ls> to quickly create multiple values!

The 3rd signature is equivalent to:
  (define <function-name> (lambda (<parameter> ...) <obj>))

Optionally include <docstring> to yield further details on
the procedure's intended purpose in the <help> menu.

Optionally include keyword types to perform runtime type checks! Argument
types are placed prior their names, and the return type is placed prior the
parameter/name clause. For example, (define :int (f :list x) (length x))
ensures <x> is a list, and that <f> returns an integer.
  => See <type-system> in <Topics> for more details on EScheme's types!

Aliased by <def>.
```

-------------------------------------------------------------------------------
### `defined?`

#### Signatures:
```scheme
(defined? <symbol>)
```

#### Description:
```
Checks if <symbol> is defined in the current environment.
Aliased by <def?>. Note that <symbol> may be an object
property chain too, hence (defined? obj.prop) is valid syntax!
```

-------------------------------------------------------------------------------
### `define-parameter`

#### Signatures:
```scheme
(define-parameter <symbol> <obj>)
```

#### Description:
```
Defines a parameter variable. Parameter variables are stored in the
'meta-environment', which all module global environments inherit from.
Thus parameter variable states are shared across modules, and are global in
scope.

See <set-parameter!> to set pre-existing parameters.
See <get-parameter> to explicitely evaluate a parameter variable.
See the <parameter?> <help> entry to determine if a variable is a parameter.

Note that *dosync-lock* is a parameter variable!
```

-------------------------------------------------------------------------------
### `define-syntax`

#### Signatures:
```scheme
(define-syntax <name> <callable>)
```

#### Description:
```
Defines <macro-name> to be a macro that uses to <callable>
to perform its code expansion.

For example:
  (define-syntax and
    (lambda (. conditions)
      (fold (lambda (acc item) (list (quote if) acc item #f))
            #t
            conditions)))
```

-------------------------------------------------------------------------------
### `define-type`

#### Signatures:
```scheme
(define-type <alias-symbol> <type-keyword>)
```

#### Description:
```
Convenience macro wrapping <define> and <type-alias>. See <type-alias>
for more details.

Aliased by <deftype>.
```

-------------------------------------------------------------------------------
### `defn`

#### Signatures:
```scheme
(defn <function-name>
  <optional-docstring>
  (() <body> ...)
  (<variadic-parameter> <body> ...)
  ((<parameter> ...) <body> ...)
  ((<parameter> ... . <variadic-parameter>) <body> ...)
  ((<parameter> ... (<optional-parameter> <default-value>) ...) <body> ...)
  ((<parameter> ...
      (<optional-parameter> <default-value>)
      ...
      .
      <variadic-parameter>)
    <body>
    ...)
  ...)
```

#### Description:
```
Combines <define> and <fn> by binding <function-name> to
(fn (<parameters> <body> ...) ...)

Optionally include <docstring> to yield further details
on the procedure's intended purpose in the <help> menu.

Optionally include keyword types to perform runtime type checks! Argument
types are placed prior their names, and return types are placed prior
parameter clauses. For example, (defn f (:int (:list x) (length x)))
ensures <x> is a list, and that <f> returns an integer.
  => See <type-system> in <Topics> for more details on EScheme's types!
```

-------------------------------------------------------------------------------
### `delay`

#### Signatures:
```scheme
(delay <expression>)
```

#### Description:
```
Delay <expression>'s evaluation until it is forced via <force>.
For example: (force (delay (+ 1 2))) ; => 3
```

-------------------------------------------------------------------------------
### `do`

#### Signatures:
```scheme
(do () () <body> ...)
(do ((<var> <initial-value>) ...) () <body> ...)
(do ((<var> <initial-value> <update-expression>) ...) () <body> ...)
(do ((<var> <initial-value> <update-expression>) ...)
  (<break-condition>)
  <body>
  ...)
(do ((<var> <initial-value> <update-expression>) ...)
  (<break-condition> <return-expression> ...)
  <body>
  ...)
```

#### Description:
```
(do ((<var> <initial-val> <update-expr>) ...) 
    (<break-condition> <return-expr> ...) 
    <body> ...)

Execute "<body> ..." while <break-condition> is #f. Once <break-condition> is
#t, return "<return-expr> ...". "<var>" is set to "<initial-val>" at first, then
to "<update-expr>" repeatedly after each iteration.

Note: 
  1. "<update-expr>" is optional
  2. If "<update-expr>" is ommited, "<var> <initial-val>" is optional
  3. "<return-expr> ..." is optional
  4. If "<return-expr> ..." is ommited, "<break-condition>" is optional
  5. "<body> ..." is optional

Hence the most minimal form of "do" is "(do () ())" (an infinite loop).
```

-------------------------------------------------------------------------------
### `fn`

#### Signatures:
```scheme
(fn <optional-docstring>
  (() <body> ...)
  (<variadic-parameter> <body> ...)
  ((<parameter> ...) <body> ...)
  ((<parameter> ... . <variadic-parameter>) <body> ...)
  ((<parameter> ... (<optional-parameter> <default-value>) ...) <body> ...)
  ((<parameter> ...
      (<optional-parameter> <default-value>)
      ...
      .
      <variadic-parameter>)
    <body>
    ...)
  ...)
```

#### Description:
```
Create a multi-arity procedure, with support for optional parameters via the
"(<optional-parameter> <default-value>)" syntax. Denote variadic parameters via
the "(<parameter> ... . <variadic-parameter>)" or "<variadic-parameter>" syntax.

Optionally include <docstring> to yield further details on the procedure's
intended purpose in the <help> menu.

Optionally include keyword types to perform runtime type checks! Argument
types are placed prior their names, and return types are placed prior
parameter clauses. For example, (fn (:int (:list x) (length x))) ensures
<x> is a list, and that the <fn> returns an integer.
  => See <type-system> in <Topics> for more details on EScheme's types!

For example:
  ; Using Mult-arity
  (define factorial 
    (fn ((n) (factorial n 1))
        ((n p)
          (if (< n 2)
              p
              (factorial (- n 1) (* n p))))))


  ; Using optional parameters: p is <1> by default
  (define factorial
    (fn ((n (p 1))
          (if (< n 2)
              p
              (factorial (- n 1) (* n p))))))


  ; Combine <fn> and <define> via <defn>:
  (defn factorial 
    ((n) (factorial n 1))
    ((n p)
      (if (< n 2)
          p
          (factorial (- n 1) (* n p)))))
```

-------------------------------------------------------------------------------
### `for`

#### Signatures:
```scheme
(for () () <body> ...)
(for ((<var> <initial-value>) ...) () <body> ...)
(for ((<var> <initial-value> <update-expression>) ...) () <body> ...)
(for ((<var> <initial-value> <update-expression>) ...)
  (<break-condition>)
  <body>
  ...)
(for ((<var> <initial-value> <update-expression>) ...)
  (<break-condition> <return-expression> ...)
  <body>
  ...)
```

#### Description:
```
(for ((<var> <initial-val> <update-expr>) ...) 
     (<break-condition> <return-expr> ...) 
     <body> ...)

Execute "<body> ..." while <break-condition> is #f. Once <break-condition> is
#t, return "<return-expr> ...". "<var>" is set to "<initial-val>" at first, then
to "<update-expr>" repeatedly after each iteration.

Note: 
  1. "<update-expr>" is optional
  2. If "<update-expr>" is ommited, "<var> <initial-val>" is optional
  3. "<return-expr> ..." is optional
  4. If "<return-expr> ..." is ommited, "<break-condition>" is optional
  5. "<body> ..." is optional

Hence the most minimal form of "for" is "(for () ())" (an infinite loop).

Note that this mirror's R4RS Scheme's "do" macro, but using true iteration 
internally.
```

-------------------------------------------------------------------------------
### `get-parameter`

#### Signatures:
```scheme
(get-parameter <symbol>)
```

#### Description:
```
Get a parameter variable's value. Parameter variables are stored in the
'meta-environment', which all module global environments inherit from.
Thus parameter variable states are shared across modules, and are global in
scope.

Note that parameter variables can also just be referenced by name, though
beware of shadowing them with a regular <define>!

See <define-parameter> to create new parameters.
See <set-parameter!> to set pre-existing parameters.
See the <parameter?> <help> entry to determine if a variable is a parameter.

Note that *dosync-lock* is a parameter variable!
```

-------------------------------------------------------------------------------
### `guard`

#### Signatures:
```scheme
(guard (<raised-var> (<condition> <expression> ...) ...) <body> ...)
(guard (<raised-var> (<condition> <expression> ...) ... (else <expression> ...))
  <body>
  ...)
```

#### Description:
```
Executes "<body> ..." while guarding against a <raise>d exception. The
<raise>d exception value is bound to <raised-var>, and <raised-var> is
then passed to the <cond>-style "(<condition> <expression> ...)" clauses.
If a condition is satisfied, then "<expression> ..." is executed. <else>
matches against all exception types.

For example:
  (guard (condition
           (else
            (display "condition: ")
            (write condition)
            (newline)
            'exception))
    (+ 1 (raise 'an-error)))
```

-------------------------------------------------------------------------------
### `if`

#### Signatures:
```scheme
(if <condition> <consequence>)
(if <condition> <consequence> <alternative>)
```

#### Description:
```
Conditional branching:
  if (not (eq? #f <condition>)): execute <consequence>
  else: execute <alternative> (defaults to #void).
```

-------------------------------------------------------------------------------
### `lambda`

#### Signatures:
```scheme
(lambda () <optional-docstring> <body> ...)
(lambda <variadic-parameter> <optional-docstring> <body> ...)
(lambda (<parameter> ...) <optional-docstring> <body> ...)
(lambda (<parameter> ... . <variadic-parameter>)
  <optional-docstring>
  <body>
  ...)
(lambda (<parameter> ... (<optional-parameter> <default-value>) ...)
  <optional-docstring>
  <body>
  ...)
(lambda 
  (<parameter> ...
    (<optional-parameter> <default-value>)
    ...
    .
    <variadic-parameter>)
  <optional-docstring>
  <body>
  ...)
```

#### Description:
```
Shorthand for single-arity procedures. Expands to <fn> under the hood.
Supports optional parameters via the "(<optional-parameter> <default-value>)"
syntax. Denote variadic parameters via "(<parameter> ... . <variadic-parameter>)"
or "<variadic-parameter>" syntax. Reader-shorthand support via the <#(> syntax.

Optionally include <docstring> to yield further details on the procedure's
intended purpose in the <help> menu.

Optionally include keyword types to perform runtime type checks! Argument
types are placed prior their names, and the return type is placed prior the
parameters. For example, (lambda :int (:list x) (length x)) ensures <x> is
a list, and that the <lambda> returns an integer.
  => See <type-system> in <Topics> for more details on EScheme's types!

For example:
  ; Using optional parameters: p is <1> by default
  (define factorial
    (lambda (n (p 1))
      (if (< n 2)
          p
          (factorial (- n 1) (* n p)))))


  ; Combine <lambda> and <define> via:
  (define (factorial n (p 1))
    (if (< n 2)
        p
        (factorial (- n 1) (* n p))))


  ;; Reader-shorthand via <#(>: #<expr> => (lambda () <expr>)
  ;; Denote the Nth parameter via %N, with N starting at 1. 
  ;; Use %% to denote the variadic parameter.

  ; Add 2:
  (define add2 #(+ %1 2))

  ; Get the 2nd parameter:
  (define get-2nd #(begin %2))

  ; The 'id' procedure:
  (define id #(begin %1))

  ; The 'list' procedure:
  (define list #(begin %%))
```

-------------------------------------------------------------------------------
### `let`

#### Signatures:
```scheme
(let ((<symbol> <obj>) ...) <body> ...)
(let <function-name>
  ((<parameter> <initial-value>) ...)
  <optional-docstring>
  <body>
  ...)
```

#### Description:
```
Signature 1 (nameless let):
  Bind "(<symbol> <obj>) ..." in a local scope and execute <body>.
Signature 2 (named let):
  Bind <function-name> to (lambda (<parameter> ...) <body> ...) in a temporary
  scope, and call it with "<initial-value> ...". Optionally include <docstring>
  to yield further details on the procedure's intended purpose via <help>.
```

-------------------------------------------------------------------------------
### `let*`

#### Signatures:
```scheme
(let* ((<symbol> <obj>) ...) <body> ...)
```

#### Description:
```
Equivalent to <let>, but later <symbol>s may refer to earlier ones.
```

-------------------------------------------------------------------------------
### `let-values`

#### Signatures:
```scheme
(let-values (((<variable> ...) <'values'-expression>) ...) <body> ...)
```

#### Description:
```
Wrapper to locally bind results of expressions that yield a <values> object.
Expands to nested <call-with-values> expressions.

See the <values> and <call-with-values> <help> entries for more information
and examples on using <values> expressions.
```

-------------------------------------------------------------------------------
### `letrec`

#### Signatures:
```scheme
(letrec ((<symbol> <obj>) ...) <body> ...)
```

#### Description:
```
Like <let>, but <value> may be a recursive function that calls <symbol>.
```

-------------------------------------------------------------------------------
### `letrec*`

#### Signatures:
```scheme
(letrec* ((<symbol> <obj>) ...) <body> ...)
```

#### Description:
```
Equivalent to <letrec>, but later <symbol>s may refer to earlier ones.
```

-------------------------------------------------------------------------------
### `or`

#### Signatures:
```scheme
(or <obj> ...)
```

#### Description:
```
Returns whether any of "<obj> ..." are #t.
```

-------------------------------------------------------------------------------
### `parameter?`

#### Signatures:
```scheme
(parameter? <symbol>)
```

#### Description:
```
Determine if <name> is a parameter variable. See the <define-parameter>
<help> entry to learn more about what parameter variables are.
```

-------------------------------------------------------------------------------
### `quasiquote`

#### Signatures:
```scheme
(quasiquote <obj>)
```

#### Description:
```
Returns <obj> as a data structure. Any <unquote>d <obj>s are evaluated first,
and <unquote-splicing>ed <obj>s will be evaluated and spliced into the list.

NOTE: Never quasiquote cyclic vectors/hashmaps during tricky (likely incorrect)
      macro use! <quasiquote> has to deep copy vectors and hashmaps at runtime
      to avoid sharing state in a single compiled function body between threads.

Note that the reader will expand:
  "`<obj>" => "(quasiquote <obj>)"
  ",<obj>" => "(unquote <obj>)"
  ",@<obj>" => "(unquote-splicing <obj>)"
```

-------------------------------------------------------------------------------
### `quote`

#### Signatures:
```scheme
(quote <obj>)
```

#### Description:
```
Returns <obj> as a data structure. Note that the reader will expand "'<obj>"
to "(quote <obj>)".

NOTE: Never quote cyclic vectors/hashmaps during tricky (likely incorrect)
      macro use! <quote> has to deep copy vectors and hashmaps at runtime, to
      avoid sharing state in a single compiled function body between threads.

For example:
  (quote a)  ; a
  (quote 1)  ; 1
  (quote "") ; ""
  (quote #t) ; #t
  (quote ()) ; #nil
  (quote (1 2 . 3)) ; (cons 1 (cons 2 3))
  (quote (1 2 3))   ; (list 1 2 3)
```

-------------------------------------------------------------------------------
### `set!`

#### Signatures:
```scheme
(set! <symbol> <obj>)
(set! <symbol1> <symbol2> ... <symbolN> <N-length-list-expression>)
```

#### Description:
```
Assigns <symbol> to <obj>. <symbol> must have been previously
defined. Note that <symbol> may be an object property chain too,
hence (set! obj.prop 42) is valid syntax!

May assign several variables to values in a list. For example:
(set! a b (list 1 2)) sets variables <a> to 1 and <b> to 2.
  * Alias <list> with <ls> to quickly create multiple values!
```

-------------------------------------------------------------------------------
### `set-parameter!`

#### Signatures:
```scheme
(set-parameter! <symbol> <obj>)
```

#### Description:
```
Sets an existing parameter variable. Parameter variables are stored in the
'meta-environment', which all module global environments inherit from.
Thus parameter variable states are shared across modules, and are global in
scope.

See <define-parameter> to create new parameters.
See <get-parameter> to explicitely evaluate a parameter variable.
See the <parameter?> <help> entry to determine if a variable is a parameter.

Note that *dosync-lock* is a parameter variable!
```

-------------------------------------------------------------------------------
### `unless`

#### Signatures:
```scheme
(unless <condition> <body> ...)
```

#### Description:
```
Execute "<body> ..." if <condition> is not true. Can be combined with the
'*import*' global variable to mimic Python's "if __name__=='__main__':"
pattern:

  (unless *import*
    <execute-main-escheme-code-here> ...)

Opposite of <when>.
```

-------------------------------------------------------------------------------
### `when`

#### Signatures:
```scheme
(when <condition> <body> ...)
```

#### Description:
```
Execute "<body> ..." if <condition> is true. Opposite of <unless>.
```

-------------------------------------------------------------------------------
### `while`

#### Signatures:
```scheme
(while (<condition>) <body> ...)
(while (<condition> <return-expr> ...) <body> ...)
```

#### Description:
```
Execute "<body> ..." while <condition> is true.
Return "<return-expr> ..." (defaults to #void) upon completion.
Note that <while> uses true iteration (not recursion!)
```

-------------------------------------------------------------------------------
## Generators


-------------------------------------------------------------------------------
### `complete-all-generators!`

#### Signatures:
```scheme
(complete-all-generators! <generator-object> ...)
```

#### Description:
```
Continues cycling through the generators until they all complete.
```

-------------------------------------------------------------------------------
### `complete-n-generators!`

#### Signatures:
```scheme
(complete-n-generators! <count> <generator-object> ...)
```

#### Description:
```
Continues cycling through the generators until at least <n> of them have completed.
```

-------------------------------------------------------------------------------
### `define-generator`

#### Signatures:
```scheme
(define-generator (<generator-name> <parameter> ...)
  <optional-docstring>
  <body>
  ...)
```

#### Description:
```
Define a generator constructor! Calling <generator-name>
returns a generator thunk, which can suspend its operation
(until it gets invoked again) via the <yield> macro.

Note that due to being a "generator-constructor", <generator-name>
should not be called recursively to emulate looping.
Instead, define an inner procedure or use the "named let" construct.

Optionally include <docstring> to yield further details on the generator
constructor's intended purpose in the <help> menu.

Optionally include keyword types to perform runtime type checks! Argument
types go prior their names. For example, (define-generator (f :int x) x)
ensures <x> is an int.
  => Note that <define-generator> does not support return types, as such
     could conflict with the final '*generator-complete* value.
  => See <type-system> in <Topics> for more details on EScheme's types!

Aliased by <defgen>.

For example:
  ;; Printing Numbers and Strings:
  (define-generator (print-numbers start total)
    (let loop ((i start))
      (if (< i total)
            (begin 
              (write i)
              (display " ")
              (yield) ; pause execution until re-invoked
              (loop (+ i 1))))))
  
  (define-generator (print-strings start total)
    (let loop ((i start))
      (if (< i total)
            (begin 
              (write (number->string i))
              (display " ")
              (yield) ; pause execution until re-invoked
              (loop (+ i 1))))))
  
  (complete-all-generators! (print-numbers 0 10) (print-strings 0 10))
  (newline)
  
  
  ;; Creating a stream of integers from a starting number (0 by default):
  (define-generator (ints-from (start 0))
    (let loop ((n start))
      (yield n)
      (loop (+ n 1))))
  
  (define ints (ints-from 42))
  (display (ints)) 
  (newline)
  (display (ints)) 
  (newline)
  (display (ints)) 
  (newline)
```

-------------------------------------------------------------------------------
### `yield`

#### Signatures:
```scheme
(yield)
(yield <obj>)
```

#### Description:
```
Pause the current execution of the generator and return <obj> (#void by
default). Only valid within the "<body> ..." of <define-generator>.
```

-------------------------------------------------------------------------------
## Modules


-------------------------------------------------------------------------------
### `from`

#### Signatures:
```scheme
(from <module-path-symbol> :import <variable-symbol> ...)
(from <module-path-symbol> :import <variable-symbol> ... :as <alias-symbol> ...)
(from <filepath-string> <module-path-symbol> :import <variable-symbol> ...)
(from <filepath-string>
  <module-path-symbol>
  :import
  <variable-symbol>
  ...
  :as
  <alias-symbol>
  ...)
```

#### Description:
```
Import '<var1-sym> <var2-sym> ...' from <module-path-sym>, without
exposing the module itself as a variable within the current environment.

Equivalent to:
  
  (begin
    (import <module-path-sym> :as <hidden-name>)
    (define <var1-sym> <hidden-name>.<var1-sym>)
    (define <var2-sym> <hidden-name>.<var2-sym>)
    ...)

Or, if '<alias1-sym> ...' is provided:

  (begin
    (import <module-path-sym> :as <hidden-name>)
    (define <alias1-sym> <hidden-name>.<var1-sym>)
    (define <alias2-sym> <hidden-name>.<var2-sym>)
    ...)

If given <filepath-str>, <from> simply adds it to the above 'import'
statement. See the <import> <help> entry for more details on how EScheme
loads modules, and see the <module> <help> entry for general details of
the overall module system itself.

Like <import>, also automatically sets the module's '*import*' global
variable to #t.
```

-------------------------------------------------------------------------------
### `import`

#### Signatures:
```scheme
(import <module-path-symbol>)
(import <module-path-symbol> :as <module-alias-symbol>)
(import <filepath-string> <module-path-symbol>)
(import <filepath-string> <module-path-symbol> :as <module-alias-symbol>)
```

#### Description:
```
Import <module-path-symbol> as a module variable in the current environment,
where <module-path-symbol> represents an Escheme (or <serialize>d!) file.

Module variables are named <module-path-symbol> by default, though there are
2 caveats:

  1. <module-alias-symbol> is provided: this overrides the default name and
     will be what the module is loaded into the environment as.

  2. <module-path-symbol> is a dotted list of symbols: this is how we denote
     access to a module that is in a different folder than the current
     directory. For example, suppose we have the following directory layout:

       Root
       |____ Folder1
       |     |_______ Module.scm
       |
       |____ Folder2
             |_______ Main.scm

     For 'Main.scm' to import 'Module.scm', it would contain:

       (import Root.Folder1.Module) ; within 'Main.scm'

     In this example, the module variable would be named 'Module'. Note that
     the file extension of the target module file is left out from the 'import'
     expression.

With regards to locating the file pointed to by <module-path-symbol>, EScheme
will first attempt to find it along the file path tree from the location that
invoked the 'import' expression. Note that this makes executing '(import #path Module)
redundant, in contrast to the 'load' function.

Imported modules are also cached across modules, so every instance of '(import Module)
will reference the same 'Module' module object. Note that you can force a module
to be reloaded via the <reload> macro.

Automatically sets the module's '*import*' global variable to #t.

Lastly, note that the concept of 'parameter' variables exist in order to
have global state shared across modules. See the <define-parameter> and
<parameter?> <help> entries for more details.

See the <from> <help> entry for an alternative to <import> that extracts specific
fields from the <module-path-symbol> module, without adding the module itself to
your current environment's namespace.

See the <module> entry in <Intrinsic-Types> for more details on EScheme's
module system.
```

-------------------------------------------------------------------------------
### `reload`

#### Signatures:
```scheme
(reload <module-alias-symbol>)
```

#### Description:
```
Forcefully reload <module-alias-symbol>. Note that <import> caches modules by default.
```

-------------------------------------------------------------------------------
## Objects


-------------------------------------------------------------------------------
### `apply-super!`

#### Signatures:
```scheme
(apply-super! <parameter> ...)
```

#### Description:
```
Alternative to "super!" that initializes the super object by
applying its <new> constructor to <list-of-args>.

ONLY valid as the FIRST expression in a class constructor:
any other use risks undefined behavior!

See <object-system> in <Topics> for more high-level object orientation details.
See <class> for more detailed object orientation
details. See <meta-object> in <Intrinsic-Types> for more type details.
```

-------------------------------------------------------------------------------
### `class`

#### Signatures:
```scheme
(class (:extends <super>)
  (:implements <interface> ...)
  <optional-docstring>
  (<field-name> <default-value>)
  ((<method-name> <parameter> ...) <body> ...)
  (:static <field-name> <default-value>)
  (:static (<method-name> <parameter> ...) <body> ...)
  ...)
```

#### Description:
```
Creates an anonymous class. See <define-class> to bind a name to a class
in one expression. See <object-system> in <Topics> for more high-level object
orientation details.

Regarding inter-class/interface relations: 
  1. <super> MUST be an expression that evals to a class type.
  2. <interface> MUST be an expression that evals to an interface type.
  3. Both :extends and :implements are optional.

Optionally include <docstring> to detail information on the class in <help>.
Methods support keyword runtime types exactly like <lambda>.


Similar to Java, we support single inheritance for classes and multiple
inheritance for interfaces.

The "new" pseudo-method acts as the class constructor (doesn't correlate 
to a real method). Invoke the class constructor by calling the class 
object as if it were a procedure.

The "->procedure" method overloads applications for objects, making them
a "functor" (function object).

The "name" field is automatically defined for classes and interfaces.
The "class" field is automatically defined for objects.

Use "self" and "super" in methods to refer to the calling object and the
class's super object respectively. Qualification of "self." is required
when referring to class fields in a method.
  (define n 0) ; the "global n"
  (define-class C
    (n 1)      ; the "local n"
    ((method)
      n        ; refers to the "global n"
      self.n)) ; refers to the "local n"


Refer to a field via: object.property1.property2
Refer to a method as: (object.inner-obj.method <arg> ...)

Objects (classes and interfaces for static methods) support the following:
  (define object.property <value>) ; define <property> as a new field
  (set! object.property <value>)   ; set the existing <property> field


Use the ":static" keyword qualifier to indicate that the field/method belongs
to the class, rather than one of its instances.
  (define-class C (:static VALUE 10))
  C.VALUE ; 10


Initialization of the super object's non-nullary constructor is achieved
via the "(super! <arg> ...)" macro. Alternatively use the "apply-super!"
macro to initialize the super object with a list of values.

  =================================================================
  * NOTE THAT "super!", IF USED, MUST BE THE FIRST EXPRESSION IN A 
    CONSTRUCTOR IN ORDER TO AVOID INVOKING UNDEFINED BEHAVIOR !!!!
  =================================================================

  ((new)
    (super! 42) ; valid
    (define self.value 314)) 

  ((new)
    (define self.value 314)
    (super! 42)) ; undefined behavior


For example:
  (define-class Rectangle
    (length 0)
    (width 0)
    ((new l w)
      (set! self.length l)
      (set! self.width w))
    ((area)
      (* self.length self.width))
    ((perimeter)
      (* 2 (+ self.length self.width))))


  (define-class Square (:extends Rectangle)
    ((new l)
      (super! l l))) ; init the super object


  (define s (Square 5))

  (display (s.area)) ; Square "inherited" <area> method. Prints 25



Class reflection is provided by primtive functions prefixed with "oo-".

See <meta-object> in <Intrinsic-Types> for more type details.
```

-------------------------------------------------------------------------------
### `define-class`

#### Signatures:
```scheme
(define-class <class-name>
  (:extends <super>)
  (:implements <interface> ...)
  <optional-docstring>
  (<field-name> <default-value>)
  ((<method-name> <parameter> ...) <body> ...)
  (:static <field-name> <default-value>)
  (:static (<method-name> <parameter> ...) <body> ...)
  ...)
```

#### Description:
```
Simple wrapper macro combining <define> and <class> to bind <class-name>.
Also generates a (<class-name>? <obj>) predicate procedure!

Optionally include <docstring> to detail information on the class in <help>.
Methods support keyword runtime types exactly like <lambda>.

Aliased by <defclass>.

See <object-system> in <Topics> for more high-level object orientation details.
See <class> for more detailed object orientation
details. See <meta-object> in <Intrinsic-Types> for more type details.
```

-------------------------------------------------------------------------------
### `define-interface`

#### Signatures:
```scheme
(define-interface <interface-name>
  (:extends <interface> ...)
  <optional-docstring>
  <field-name>
  (:static <field-name> <default-value>)
  (:static (<method-name> <parameter> ...) <body> ...)
  ...)
```

#### Description:
```
Simple wrapper macro combining <define> and <interface> to bind <interface-name>.
Also generates a (<interface-name>? <obj>) predicate procedure!

Optionally include <docstring> to detail information on the interface in <help>.
Static methods support keyword runtime types exactly like <lambda>.

Aliased by <definterface>.

See <object-system> in <Topics> for more high-level object orientation details.
See <class> for more detailed object orientation
details. See <meta-object> in <Intrinsic-Types> for more type details.
```

-------------------------------------------------------------------------------
### `interface`

#### Signatures:
```scheme
(interface (:extends <interface> ...)
  <optional-docstring>
  <field-name>
  (:static <field-name> <default-value>)
  (:static (<method-name> <parameter> ...) <body> ...)
  ...)
```

#### Description:
```
Creates an anonymous interface. Similar to classes, BUT cannot be instantiated
via a constructor. Required property names are denoted by a symbolic property,
(<field-name> above).

Use :extends to optionally inherit required fields from other interface objects.

Optionally include <docstring> to detail information on the interface in <help>.
Static methods support keyword runtime types exactly like <lambda>.

See <object-system> in <Topics> for more high-level object orientation details.
See <class> for more detailed object orientation
details. See <meta-object> in <Intrinsic-Types> for more type details.

For example:
  (define-interface IHasName
    (:static VALUE 10)
    name)

  (define-interface IHasAge
    age)

  (define-interface IPerson (:extends IHasName IHasAge))

  (define-class Person (:implements IPerson)
    (name "")
    (age 0))
```

-------------------------------------------------------------------------------
### `super!`

#### Signatures:
```scheme
(super! <parameter> ...)
```

#### Description:
```
Initialize the super object via its non-nullary constructor.

ONLY valid as the FIRST expression in a class constructor:
any other use risks undefined behavior!

See <object-system> in <Topics> for more high-level object orientation details.
See <class> for more detailed object orientation
details. See <meta-object> in <Intrinsic-Types> for more type details.
```

-------------------------------------------------------------------------------
## Streams


-------------------------------------------------------------------------------
### `scaaaar`

#### Signatures:
```scheme
(scaaaar <stream-pair>)
```

#### Description:
```
Equivalent to: (scar (scar (scar (scar <stream-pair>))))
```

-------------------------------------------------------------------------------
### `scaaadr`

#### Signatures:
```scheme
(scaaadr <stream-pair>)
```

#### Description:
```
Equivalent to: (scar (scar (scar (scdr <stream-pair>))))
```

-------------------------------------------------------------------------------
### `scaaar`

#### Signatures:
```scheme
(scaaar <stream-pair>)
```

#### Description:
```
Equivalent to: (scar (scar (scar <stream-pair>)))
```

-------------------------------------------------------------------------------
### `scaadar`

#### Signatures:
```scheme
(scaadar <stream-pair>)
```

#### Description:
```
Equivalent to: (scar (scar (scdr (scar <stream-pair>))))
```

-------------------------------------------------------------------------------
### `scaaddr`

#### Signatures:
```scheme
(scaaddr <stream-pair>)
```

#### Description:
```
Equivalent to: (scar (scar (scdr (scdr <stream-pair>))))
```

-------------------------------------------------------------------------------
### `scaadr`

#### Signatures:
```scheme
(scaadr <stream-pair>)
```

#### Description:
```
Equivalent to: (scar (scar (scdr <stream-pair>)))
```

-------------------------------------------------------------------------------
### `scaar`

#### Signatures:
```scheme
(scaar <stream-pair>)
```

#### Description:
```
Equivalent to: (scar (scar <stream-pair>))
```

-------------------------------------------------------------------------------
### `scadaar`

#### Signatures:
```scheme
(scadaar <stream-pair>)
```

#### Description:
```
Equivalent to: (scar (scdr (scar (scar <stream-pair>))))
```

-------------------------------------------------------------------------------
### `scadadr`

#### Signatures:
```scheme
(scadadr <stream-pair>)
```

#### Description:
```
Equivalent to: (scar (scdr (scar (scdr <stream-pair>))))
```

-------------------------------------------------------------------------------
### `scadar`

#### Signatures:
```scheme
(scadar <stream-pair>)
```

#### Description:
```
Equivalent to: (scar (scdr (scar <stream-pair>)))
```

-------------------------------------------------------------------------------
### `scaddar`

#### Signatures:
```scheme
(scaddar <stream-pair>)
```

#### Description:
```
Equivalent to: (scar (scdr (scdr (scar <stream-pair>))))
```

-------------------------------------------------------------------------------
### `scadddr`

#### Signatures:
```scheme
(scadddr <stream-pair>)
```

#### Description:
```
Equivalent to: (scar (scdr (scdr (scdr <stream-pair>))))
```

-------------------------------------------------------------------------------
### `scaddr`

#### Signatures:
```scheme
(scaddr <stream-pair>)
```

#### Description:
```
Equivalent to: (scar (scdr (scdr <stream-pair>)))
```

-------------------------------------------------------------------------------
### `scadr`

#### Signatures:
```scheme
(scadr <stream-pair>)
```

#### Description:
```
Equivalent to: (scar (scdr <stream-pair>))
```

-------------------------------------------------------------------------------
### `scar`

#### Signatures:
```scheme
(scar <stream-pair>)
```

#### Description:
```
Access the first item in a stream-pair.
```

-------------------------------------------------------------------------------
### `scdaaar`

#### Signatures:
```scheme
(scdaaar <stream-pair>)
```

#### Description:
```
Equivalent to: (scdr (scar (scar (scar <stream-pair>))))
```

-------------------------------------------------------------------------------
### `scdaadr`

#### Signatures:
```scheme
(scdaadr <stream-pair>)
```

#### Description:
```
Equivalent to: (scdr (scar (scar (scdr <stream-pair>))))
```

-------------------------------------------------------------------------------
### `scdaar`

#### Signatures:
```scheme
(scdaar <stream-pair>)
```

#### Description:
```
Equivalent to: (scdr (scar (scar <stream-pair>)))
```

-------------------------------------------------------------------------------
### `scdadar`

#### Signatures:
```scheme
(scdadar <stream-pair>)
```

#### Description:
```
Equivalent to: (scdr (scar (scdr (scar <stream-pair>))))
```

-------------------------------------------------------------------------------
### `scdaddr`

#### Signatures:
```scheme
(scdaddr <stream-pair>)
```

#### Description:
```
Equivalent to: (scdr (scar (scdr (scdr <stream-pair>))))
```

-------------------------------------------------------------------------------
### `scdadr`

#### Signatures:
```scheme
(scdadr <stream-pair>)
```

#### Description:
```
Equivalent to: (scdr (scar (scdr <stream-pair>)))
```

-------------------------------------------------------------------------------
### `scdar`

#### Signatures:
```scheme
(scdar <stream-pair>)
```

#### Description:
```
Equivalent to: (scdr (scar <stream-pair>))
```

-------------------------------------------------------------------------------
### `scddaar`

#### Signatures:
```scheme
(scddaar <stream-pair>)
```

#### Description:
```
Equivalent to: (scdr (scdr (scar (scar <stream-pair>))))
```

-------------------------------------------------------------------------------
### `scddadr`

#### Signatures:
```scheme
(scddadr <stream-pair>)
```

#### Description:
```
Equivalent to: (scdr (scdr (scar (scdr <stream-pair>))))
```

-------------------------------------------------------------------------------
### `scddar`

#### Signatures:
```scheme
(scddar <stream-pair>)
```

#### Description:
```
Equivalent to: (scdr (scdr (scar <stream-pair>)))
```

-------------------------------------------------------------------------------
### `scdddar`

#### Signatures:
```scheme
(scdddar <stream-pair>)
```

#### Description:
```
Equivalent to: (scdr (scdr (scdr (scar <stream-pair>))))
```

-------------------------------------------------------------------------------
### `scddddr`

#### Signatures:
```scheme
(scddddr <stream-pair>)
```

#### Description:
```
Equivalent to: (scdr (scdr (scdr (scdr <stream-pair>))))
```

-------------------------------------------------------------------------------
### `scdddr`

#### Signatures:
```scheme
(scdddr <stream-pair>)
```

#### Description:
```
Equivalent to: (scdr (scdr (scdr <stream-pair>)))
```

-------------------------------------------------------------------------------
### `scddr`

#### Signatures:
```scheme
(scddr <stream-pair>)
```

#### Description:
```
Equivalent to: (scdr (scdr <stream-pair>))
```

-------------------------------------------------------------------------------
### `scdr`

#### Signatures:
```scheme
(scdr <stream-pair>)
```

#### Description:
```
Access the second item in a stream-pair.
```

-------------------------------------------------------------------------------
### `scons`

#### Signatures:
```scheme
(scons <scar-obj> <scdr-obj>)
```

#### Description:
```
Create a stream-pair of "<obj> <obj>". Streams don't evaluate their
items until the items are accessed via <scar>, <scdr>, <scaar>, etc.
Create a stream by nesting stream pairs that end with (quote ()) [think lists].
```

-------------------------------------------------------------------------------
### `stream->generator`

#### Signatures:
```scheme
(stream->generator <stream>)
```

#### Description:
```
Return a thunk (nullary procedure) that, upon invocation, returns the
next item in the stream.
Returns <*generator-complete*> once at the end of the stream.
```

-------------------------------------------------------------------------------
### `stream->list`

#### Signatures:
```scheme
(stream->list <stream> <list-length>)
```

#### Description:
```
Return the first <list-length> items in <stream> as a list.
```

-------------------------------------------------------------------------------
### `stream-append`

#### Signatures:
```scheme
(stream-append <stream> <stream> ...)
```

#### Description:
```
Returns a new stream of "<stream> ..." appended to one another.
```

-------------------------------------------------------------------------------
### `stream-constant`

#### Signatures:
```scheme
(stream-constant <obj> ...)
```

#### Description:
```
Return an infinite stream of "<obj> ..." repeating in a cycle. Executes lazily.
```

-------------------------------------------------------------------------------
### `stream-drop`

#### Signatures:
```scheme
(stream-drop <stream> <length>)
```

#### Description:
```
Returns <stream> without its first <length> items as another stream.
```

-------------------------------------------------------------------------------
### `stream-drop-while`

#### Signatures:
```scheme
(stream-drop-while <continue?-callable> <stream>)
```

#### Description:
```
Returns <stream> without all of the leading items that satisfy <continue?-callable>
as another stream. Note that this will run forever if <stream> is infinite
and every item in it satisfies <predicate?>.
```

-------------------------------------------------------------------------------
### `stream-filter`

#### Signatures:
```scheme
(stream-filter <keep?-callable> <stream>)
```

#### Description:
```
Generate a new stream by filtering out items in <stream> that don't
satisfy <keep?-callable>. Executes lazily.
```

-------------------------------------------------------------------------------
### `stream-interleave`

#### Signatures:
```scheme
(stream-interleave <stream1> <stream2>)
```

#### Description:
```
Return a stream with the interleaved values of <stream1> and <stream2>.
Executes lazily.
```

-------------------------------------------------------------------------------
### `stream-iterate`

#### Signatures:
```scheme
(stream-iterate <update-callable> <obj>)
```

#### Description:
```
Generate an infinite stream by starting with <seed> and updating it
with <update-callable>. Executes lazily.
```

-------------------------------------------------------------------------------
### `stream-map`

#### Signatures:
```scheme
(stream-map <callable> <stream> ...)
```

#### Description:
```
Generate a new stream by applying <callable> to each of the items in
"<stream> ...". Executes lazily.
```

-------------------------------------------------------------------------------
### `stream-member`

#### Signatures:
```scheme
(stream-member <obj> <stream>)
```

#### Description:
```
Returns the substream in <stream> starting with <obj> based on <equal?> item
equality. Returns #f if <obj> isn't in <stream>. Note that this will run
forever if <obj> isn't in <stream> and <stream> is infinite.
```

-------------------------------------------------------------------------------
### `stream-memq`

#### Signatures:
```scheme
(stream-memq <obj> <stream>)
```

#### Description:
```
Returns the substream in <stream> starting with <obj> based on <eq?> item
equality. Returns #f if <obj> isn't in <stream>. Note that this will run
forever if <obj> isn't in <stream> and <stream> is infinite.
```

-------------------------------------------------------------------------------
### `stream-pair?`

#### Signatures:
```scheme
(stream-pair? <obj>)
```

#### Description:
```
Returns whether <obj> is a stream-pair created by <scons>.
```

-------------------------------------------------------------------------------
### `stream-slice`

#### Signatures:
```scheme
(stream-slice <index>)
(stream-slice <index> <length>)
(stream-slice <index> <continue?-callable>)
```

#### Description:
```
Slices a subset of the items in <stream> starting from <start-index>.
If no other args are given, returns the rest of the items from <start-index>.
If <length> is given, returns at most <length> items.
Given <continue?-callable>, slices while values satisfy <continue?-callable>.

Note that this may run infinitely if given an infinite stream where every
value satisfies <continue?-callable>
```

-------------------------------------------------------------------------------
### `stream-take`

#### Signatures:
```scheme
(stream-take <stream> <length>)
```

#### Description:
```
Returns the first <length> items in <stream> as another stream.
```

-------------------------------------------------------------------------------
### `stream-take-while`

#### Signatures:
```scheme
(stream-take-while <continue?-callable> <stream>)
```

#### Description:
```
Returns a stream with the first items in <stream> while they satisfy
<continue?-callable>.
```

-------------------------------------------------------------------------------
### `stream-unfold`

#### Signatures:
```scheme
(stream-unfold <break?-callable> <mapper-callable> <update-callable> <seed>)
```

#### Description:
```
Unfolds a stream from left to right, starting with <seed>. <break?-callable>
determines when unfolding stops, <mapper-callable> maps the <seed> to a value
in the unfolded stream, and <update-callable> increments <seed> for the
next round of unfolding.

See <stream-unfolds> for an alternative without <break?-callable>.
```

-------------------------------------------------------------------------------
### `stream-unfolds`

#### Signatures:
```scheme
(stream-unfolds <mapper-callable> <update-callable> <seed>)
```

#### Description:
```
Unfolds a stream from left to right, starting with <seed>. <mapper-callable>
maps the <seed> to a value in the unfolded stream, and <update-callable>
increments <seed> for the next round of unfolding.

See <stream-unfold> for an alternative with a <break?-callable>.
```

-------------------------------------------------------------------------------
### `stream-val`

#### Signatures:
```scheme
(stream-val <stream> <index>)
```

#### Description:
```
Return the <index>th item in <stream>.
```

-------------------------------------------------------------------------------
### `stream?`

#### Signatures:
```scheme
(stream? <obj>)
```

#### Description:
```
Returns whether <obj> is a stream. Equivalent to:
  (or (stream-pair? <obj>) (null? <obj>))
```

-------------------------------------------------------------------------------
## Synchronization


-------------------------------------------------------------------------------
### `dosync`

#### Signatures:
```scheme
(dosync <expr> ...)
```

#### Description:
```
Execute "<expr> ..." with a mutex lock around it as a critical section.
Locks/Unlocks the lock via <dynamic-wind> to ensure that random
continuations don't prevent us from unlocking the lock.

  NOTE: Locks the *dosync-lock* parameter mutex variable (same state across
        modules).
```

-------------------------------------------------------------------------------
### `dosync-module`

#### Signatures:
```scheme
(dosync-module <expr> ...)
```

#### Description:
```
Execute "<expr> ..." with a mutex lock around it as a critical section.
Locks/Unlocks the lock via <dynamic-wind> to ensure that random continuations
don't prevent us from unlocking the lock.

  NOTE: Locks the *dosync-module-lock* global mutex variable (unique state
        per module).
```

-------------------------------------------------------------------------------
### `dosync-with`

#### Signatures:
```scheme
(dosync-with <mutex> <expr> ...)
```

#### Description:
```
Like <dosync>, but with a lock of your choosing rather than *dosync-lock*.
```

-------------------------------------------------------------------------------
### `thread-define`

#### Signatures:
```scheme
(thread-define <symbol> <value>)
(thread-define <thread> <symbol> <value>)
```

#### Description:
```
Bind <variable-name> to <value> in <thread>'s (defaults to the "meta-thread")
dynamic environment (effectively a thread-local global environment).

Note that the "meta-thread" is a pseudo-thread accessable by all threads:
  Thread dynamic environments "inherit" value bindings from the
  "meta-thread" by caching a copy of them upon reference.
```

-------------------------------------------------------------------------------
### `thread-defined?`

#### Signatures:
```scheme
(thread-defined? <symbol>)
(thread-defined? <thread> <symbol>)
```

#### Description:
```
Return whether <variable-name> is defined in <thread>'s
(defaults to the "meta-thread") dynamic environment
(effectively a thread-local global environment).

Note that the "meta-thread" is a pseudo-thread accessable by all threads:
  Thread dynamic environments "inherit" value bindings from the
  "meta-thread" by caching a copy of them upon reference.
```

-------------------------------------------------------------------------------
### `thread-get`

#### Signatures:
```scheme
(thread-get <symbol>)
(thread-get <thread> <symbol>)
```

#### Description:
```
Get <variable-name>'s value in <thread>'s (defaults to the "meta-thread")
dynamic environment (effectively a thread-local global environment).

Note that the "meta-thread" is a pseudo-thread accessable by all threads:
  Thread dynamic environments "inherit" value bindings from the
  "meta-thread" by caching a copy of them upon reference.
```

-------------------------------------------------------------------------------
### `thread-set!`

#### Signatures:
```scheme
(thread-set! <symbol> <value>)
(thread-set! <thread> <symbol> <value>)
```

#### Description:
```
Set <variable-name> to <value> in <thread>'s (defaults to the "meta-thread")
dynamic environment (effectively a thread-local global environment).

Note that the "meta-thread" is a pseudo-thread accessable by all threads:
  Thread dynamic environments "inherit" value bindings from the
  "meta-thread" by caching a copy of them upon reference.
```

-------------------------------------------------------------------------------
# Topics


-------------------------------------------------------------------------------
## `command-line`

### Description:
```
Command-line flags may be used to modify EScheme's behavior:
  1. -v, --version                    | Print EScheme version information
  2. -h, --help                       | Print this information
  3. -q, --quiet                      | Launch the REPL without ASCII art
  4. -e, --eval <escheme code>        | Evaluate <escheme code> in a temporary file
  5. -l, --load <script> <arg1> ...   | Load <script> with <arg> ... as *argv* into the REPL
  6. -i, --import <module> <arg1> ... | Import <module> with <arg> ... as *argv* into the REPL
  7. <script> <arg1> ...              | Interpret <script> with <arg> ... as *argv*
  8. [no arguments]                   | Launch the REPL
```

-------------------------------------------------------------------------------
## `comments`

### Description:
```
Single-line comments start with ';'.
```

-------------------------------------------------------------------------------
## `concurrency`

### Description:
```
EScheme supports parallelism via true Java threads!

Its core literal data structures are immutable (including lists,
in a departure from standard Scheme), however, objects generated
from classes can be mutated (meaning one could create mutable
lists via the object system!).

Spawn threads via the <thread> procedure.
Get a reentrant lock via the <mutex> procedure.
Check out <help>'s 'Procedures > Concurrency' section for more!

On Threading & Continuations:
* Threads each have their own stack, and hence their own set of continuations.
* Continuations are delimited by the execution of the thread that created them.
* Hence a child thread's continuation will never 'continue' back into the parent
  thread, rather, it will terminate where the child thread terminated.

On Threading & Dynamic Environments:
* Each thread has a so-called 'dynamic environment', wherein a set of variable
  bindings is kept globally within the thread while being hidden from other threads.
* After querying for a variable that doesn't exist in a thread's dynamic environment,
  the 'meta thread''s dynamic environment is checked. If an entry is found in the
  meta thread's dynamic environment, a local copy is cached into the current thread's
  dynamic environment and the querying operation continues.
* This allows for us to have 'environment-global-thread-local' variables! State can
  be shared & operated upon by many procedures without having to lock, since each
  thread only ever operates on a local copy of the state!
  => This is used by <dynamic-wind> in order to maintain thread-local winds!
```

-------------------------------------------------------------------------------
## `continuation-passing-style`

### Description:
```
A style of programming which explicitly handles control flow via 'continuations',
where a 'continuation' represents the rest of the work to be done in a program.

Programming with and manipulating continuations can yield certain advantages,
most notably the ability to implement many control flow operations in terms
of continuations (including coroutines, try-catch, arbitrary returns, etc.)

Unfortunately, explicitly programming with continuations is rarely desirable
and hardly enjoyable. Fortunately, there are ways to convert any program into
CPS, and Scheme as a language has this transformation baked in by default.

The power of continuations in Scheme may be leveraged through the primitive
"call/cc" procedure: taking an unary procedure as its argument, "call/cc"
(or "call-with-current-continuation") passes the current continuation as a
unary-procedure argument to the function it received.

Note that, when developing a runtime system, you actually have two means by
which to go about implementing continuations. You can either transform the
Scheme into continuation passing style, or you could write the VM itself in
CPS such that the continuation management is entirely handled internally.

EScheme opts for the second approach. Note that it required a custom
trampolining library for this to work, since Java 21 still doesn't implement
tail-call optimizations, but oh well.

Our baked-in VM-level continuations give us some serious advantages:

  1. The bytecode is continuation-agnostic, since 'call/cc' is just like any
     other procedure (this makes writing inlined bytecode MUCH simpler)
  2. All function calls are 'call-optimized': since I both (a) implemented
     trampolining for the Java tail-calls themselves, and (b) wrote the Java
     code in CPS (thereby making every function call a tail-call).
  3. Compiled procedure body bytecodes are MUCH shorter, since they don't
     need to reflect the continuation-management logic already done by the VM.
```

-------------------------------------------------------------------------------
## `macros`

### Description:
```
EScheme's macros expand at compile time, and are bound at runtime.

A set of symbolic names and procedures tracked by the compiler, macros
are defined in the global environment, and hence are shared across threads.
They are not, however, shared across modules!

Created by 'define-syntax', being bound to procedures means that macros can
use the full power of EScheme at compile time. Furthermore, they may have
multiple arities, support optional arguments, and have variadic arguments
just like any other EScheme procedure!

Macros are so important in fact, their expansion is one of the only 4 jobs
that EScheme's Java compiler performs:

  1. Reflect inlined bytecode from 'bytecode'
  2. Compile vector/hashmap literals
  3. Identify and expand macros
  4. Compile function applications

That's it! Every single other special form besides 'bytecode' is implemented
as a macro!
```

-------------------------------------------------------------------------------
## `module-system`

### Description:
```
# Modules in EScheme

### Describes EScheme's optional module system!

## Motivation and Overview

The value created by an `import` expression.

Modules present an alternative to Scheme's usual inter-file semantics.

The `load` function has always served as a means by which to execute the code
of another Scheme file in the calling file's global environment. This simplicity
is a double edged sword though: while making `load` easy to implement, it leaves
much to be desired with respect to enabling encapsulation of code across
coordinated Scheme files.

As such, in addition to `load`, EScheme offers a minimalistic module system
that strives to enable file-specific encapsulation of EScheme code. Files
processed via the `import` macro are defined as `module` objects within the
enclosing enivironment. See the `import` details below for more information on
how EScheme evaluates modules.

Each module has its own isolated global environment, and has automatic access
to EScheme's standard library. Note that this means that operations that depend
on global variables (e.g. `load-once`) are hence only able to operate on
a module-relative basis.

- Note that `dosync` notably works across modules, since its internal lock was
  created via `define-parameter`. Use `dosync-module` for module-relative locking
  behavior.

Both variables and macros can be accessed from a module by using EScheme's
dot-notation: for example, to access variable `PersonClass` from module `Mod`,
we write `Mod.PersonClass`.

Further note that imported modules are cached! Importing the same module
multiple times does not cause multiple evaluations of that module. Use the
`reload` macro if you'd like to forcefully re-evaluate the module in question.

Additionally, the `from` macro may be used to load specific variables within
a given module, without defining that module itself as a local variable.

Modules may be introspected upon by 2 primitives:

1. `module-path`: yield the absolute file path to the module (this is what
   distinguishes one module from another under the hood).
2. `module-bindings`: yield a list of the symbols defined in a module (beware:
   every module redefines the entire EScheme standard library!).

Note that the 'meta-thread's environment (see `thread-define`) is module
independant!

Use the `*import*` variable to determine if the current file was `import`ed.
Can combine with `unless` to mimic Python's `if __name__=='__main__':` pattern:

```scheme
  (unless *import*
    <execute-main-escheme-code-here> ...)
```

Lastly, note that the concept of 'parameter' variables exist in order to
have global state shared across modules. See the `define-parameter` and
`parameter?` 'help' entries for more details.

---

## Accessing variable `<var>` from module `<module>`

```scheme
<module>.<var>
```

---

## Regular Imports

```scheme
(import <module-path-symbol>)
(import <module-path-symbol> :as <module-alias-symbol>)
(import <filepath-string> <module-path-symbol>)
(import <filepath-string> <module-path-symbol> :as <module-alias-symbol>)
```

Import `<module-path-symbol>` as a module variable in the current environment,
where `<module-path-symbol>` represents an Escheme (or `serialize`d!) file.
Seeks the module from `<filepath-string>`, if provided.

Module variables are named `<module-path-symbol>` by default, though there are
2 caveats:

1.  `<module-alias-symbol>` is provided: this overrides the default name and
    will be what the module is loaded into the environment as.

2.  `<module-path-symbol>` is a dotted list of symbols: this is how we denote
    access to a module that is in a different folder than the current
    directory. For example, suppose we have the following directory layout:

    ```
    Root
    |____ Folder1
    |     |_______ Module.scm
    |
    |____ Folder2
          |_______ Main.scm
    ```

    For `Main.scm` to import `Module.scm`, it would contain:

    ```scheme
    (import Folder1.Module) ; within 'Main.scm'. '(import Root.Folder1.Module)' also works
    ```

    In this example, the module variable would be named `Module`. Note that
    the file extension of the target module file is left out from the `import`
    expression.

With regards to locating the file pointed to by `<module-path-symbol>`, EScheme
will first attempt to find it along the file path tree from the location that
invoked the `import` expression. Note that this makes executing `(import #path Module)`
redundant, in contrast to the `load` function.

Imported modules are also cached across modules, so every instance of `(import Module)`
will reference the same `Module` module object. Note that you can force a module
to be reloaded via the `reload` special form.

See the `from` details below for an alternative to `import` that extracts specific
fields from the `<module-path-symbol>` module, without adding the module itself to
your current environment's namespace.

---

## Reloading Modules

```scheme
(reload <module-alias-symbol>)
```

Forcefully re-evaluate `<module-alias-symbol>` (note that `import` caches
modules by default).

---

## Loading Module Variables

```scheme
(from <module-path-symbol> :import <obj1-symbol> <obj2-symbol> ...)
(from <module-path-symbol> :import <obj1-symbol> <obj2-symbol> ... :as <alias1-symbol> <alias2-symbol> ...)
(from <filepath-string> <module-path-symbol> :import <obj1-symbol> <obj2-symbol> ...)
(from <filepath-string> <module-path-symbol> :import <obj1-symbol> <obj2-symbol> ... :as <alias1-symbol> <alias2-symbol> ...)
```

Import `<obj1-symbol> <obj2-symbol> ...` from `<module-path-symbol>`, without
exposing the module itself as a variable within the current environment.

Equivalent to:

```scheme
(begin
  (import <module-path-symbol> :as <hidden-name>)
  (define <obj1-symbol> <hidden-name>.<obj1-symbol>)
  (define <obj2-symbol> <hidden-name>.<obj2-symbol>)
  ...)
```

Or, if `<alias1-symbol> ...` is provided:

```scheme
(begin
  (import <module-path-symbol> :as <hidden-name>)
  (define <alias1-symbol> <hidden-name>.<obj1-symbol>)
  (define <alias2-symbol> <hidden-name>.<obj2-symbol>)
  ...)
```

If given `<filepath-string>`, `from` simply adds it to the above `import`
statement.

---

## Module Introspection Primitives

### Module Predicate

```scheme
(module? <obj>)
```

### Module Absolute Path Source Location

```scheme
(module-path <module>)
```

The absolute file path string of the module file's location, what
distinguishes one module from another under the hood.

### Module Variable Name Bindings List

```scheme
(module-bindings <module>)
```

A list of variable symbols defined in `<module>`. Beware that all modules load
all of EScheme's standard library by default!
```

-------------------------------------------------------------------------------
## `object-system`

### Description:
```
# Objects in EScheme

### Describes EScheme's optional object system!

## Object System Overview

EScheme has a totally optional object system, including support for:

- Single inheritance for classes, multiple inheritance for interfaces
- Static support:
  - `:static` fields & methods for class/interface-local properties
    - implied when `define`ing a new class/interface property!
- `self` semantics:
  - instance methods have `self` dynamically bound to the calling object
  - static methods have `self` bound to the class datum they belong to
- `super` semantics:
  - instance methods have `super` statically bound to the super object
  - static methods have `super` bound to the super class
- Referring to static props in instance methods:
  - `<classname>.<static-prop>`
  - `self.class.<static-prop>`
- Special object properties:
  - `new` pseudo-method constructor syntax
    - Doesn't correlate to an actual method named `new`
  - `class` field to access the class of the current object
  - `->procedure` method to overload application for objects
    - Applicable objects with this method are called "functor"s
- Special class and interface property:
  - `name`: the symbolic name of the class/interface
    - Only present if the class/interface is NOT anonymous!
- All methods have the following variables automatically defined:
  - `self` ; the polymorphic calling object
  - `super` ; the super object if exists, else #f
- Bytecode-level support for `(define obj.prop)` `(set! obj.prop)` syntax

---

## Named Class Syntax:

### Also generates a `(<class-name>? <obj>)` predicate procedure!

### May use `defclass` to alias `define-class`!

```scheme
(define-class <class-name>
  (:extends <class>) (:implements <interface> ...) ; both are optional
  <optional-docstring>
  (:static <name> <value>)
  (:static (<method-name> <param> ...) <body> ...)
  (<name> <value>)
  ((<method-name> <param> ...) <body> ...))
```

---

## Anonymous Class Syntax:

```scheme
(class (:extends <class>) (:implements <interface> ...) ; both are optional
  <optional-docstring>
  (:static <name> <value>)
  (:static (<method-name> <param> ...) <body> ...)
  (<name> <value>)
  ((<method-name> <param> ...) <body> ...))
```

---

## Named Interface Syntax:

### Also generates an `(<interface-name>? <obj>)` predicate procedure!

### May use `definterface` to alias `define-interface`!

```scheme
(define-interface <interface-name>
  (:extends <interface> ...) ; ":extends" is optional
  <optional-docstring>
  (:static <name> <value>)
  (:static (<method-name> <param> ...) <body> ...)
  <name>) ; required property name(s) for a class to have
```

---

## Anonymous Interface Syntax:

```scheme
(interface (:extends <interface> ...) ; ":extends" is optional
  <optional-docstring>
  (:static <name> <value>)
  (:static (<method-name> <param> ...) <body> ...)
  <name>) ; required property name(s) for a class to have
```

---

## Initializing Super Object Constructors

The `(super! <param> ...)` macro may be used within object constructors to
initialize an object's super class with a set of parameters.

- Super objects with a "nullary" constructor are automatically constructed.
- `super!` must be used immediately in constructors to avoid undefined behavior!
- Use `(apply-super! <param-list>)` to initialize the super object with a list.
```

-------------------------------------------------------------------------------
## `serialization`

### Description:
```
Serialize EScheme code with the <serialize> primitive.
Load serialized EScheme code with the <load> primitive.

Serialization works by storing the Java bytecode of the assembled instruction
set object holding <escm-file-path>'s EScheme bytecode in <serialized-file-path>.
The contents must first be evaluated in order to expand macros properly during
compilation.

Totally optional to use, this facility is provided in order to serve as a portable 
obfuscation tactic to generate a rough equivalent to "EScheme binaries". 

However, it is important to note that - strangely enough - serialized files tend to 
load _SLOWER_ than regular EScheme files. Despite allowing us to circumvent the 
reading, compilation, and assembly of EScheme code, it turns out that EScheme's 
facilities to do so are all actually so fast as to beat out Java's native 
deserialization functions!

Hence, only serialize your EScheme code if you'd like to make the file unreadable
for humans.

All EScheme objects serialize just as you'd imagine, save for Threads and Ports.
By default, Java doesn't enable these items to be serialized since they rely on
system-dependant components that don't translate to a different machine's processes.
However, EScheme allows such to occur using the following rules:

  1. Threads
     * Threads serialize to their default, "pre-run" state. This means that
       serialized threads save their name and runnable-callable. It does _NOT_
       save any information about the executing sub-process if serialized mid-run.
  2. Output-Ports
     * Output-Ports serialize their absolute file path, and always deserialize
       without appending (as if created via <open-output-file>).
       - This is because deserialization re-loads the entire script containing
         the port, and hence any writing operations conducted by the script ought
         to reoccur during the script's loading process.
     * Hence Output-Ports _MUST_ be serialized & deserialized on a machine with the
       same directory layout, to avoid errors with the stored absolute-path!
  3. Input-Ports
     * Input-Ports serialize their absolute file path, and always deserialize with
       their current line & column both set to 1 (reading from the start of the file).
     * Hence Input-Ports _MUST_ be serialized & deserialized on a machine with the
       same directory layout, to avoid errors with the stored absolute-path!

OF NOTE: The semantics of thread/port serialization won't be an issue for 99.99999%
*******  of serialized programs that use macros in a remotely sane way.

         Only by intentionally returning a value that contains an initialized thread
         or port datum from a macro can one tease out this behavior.

         Given that most macros only return code as data structures (rather than
         data values), these semantics are only something worth thinking about
         if you're intentionally employing some genuinely tricky (& almost
         certainly questionable) EScheme meta-programming techniques.
```

-------------------------------------------------------------------------------
## `type-system`

### Description:
```
# Types in EScheme

### Describes EScheme's optional type system!

## Overview

EScheme denotes types with keywords, and "union types" via `|` syntax.

- EX: `:str|num` represents either a string or a number.

EScheme types are typically either a "primitive" or "collection" type.
If a type is neither a primitive nor a collection, it is presumed to
represent some class, interface, or type-alias: if the type doesn't
resolve to a valid class, interface, or type-alias during a runtime
type-check, an error is thrown.

- Note that EScheme supports referencing classes/interfaces/aliases
  in modules! Hence `:Module.ClassName` is a valid type.

EScheme types are parsed and converted to Java functional interface
predicates internally during compilation, with the predicates being
applied at runtime. Types are supported for function parameters and
return values.

### EScheme Primitive Types

Primitive types represent an intrinsic atomic EScheme type. Their
type-checks are typically as fast as a single `instanceof` check,
with a few exceptions like `:int` requiring slightly more work.

EScheme's primitive types include:

```scheme
:any

:num ; aliased by ":complex"
:int ; matches floats without fractionals too
:flo
:real
:exact
:inexact

:str
:char
:key ; keyword
:bool
:sym ; symbol
:void

:thread
:mutex

:nil
:atom

:fn ; all callables
:procedure
:syntax

:metaobj ; includes objects, classes, and interfaces
:object
:class
:interface

:dottable ; includes objects, classes, interfaces, and modules
:module

:port
:inport
:outport

:type-alias
```

### EScheme Collection Types

Collection types represent an intrinsic EScheme collection type. By
default, collections are just checked to match whatever type of
collection the keyword stands for (without regard for the types of
its contents). However, collections may be parameterized by adding
the `<type>` suffix in order to type-check its contents as well.

For example, `:list<str|sym>` is a list where each element is either
a string or symbol.

- For either a list that only has strings OR a list that only has
  symbols, use `:list<str>|list<sym>`.
- Furthermore, `:pair` and `:map` may also be parameterized with the
  `<type,type>` suffix in order to type-check their keys and values.

EScheme's collection types include:

```scheme
:vec ; vector
:map ; hashmap

:pair
:list

:ac ; associative-collection
:oc ; ordered-collection
```

---

## Type Syntax

Notes on optional and variadic parameters:

- Optional parameters only type-check user args, _not_ their default values
  - Hence `(:int a "hello")` is a valid optional parameter clause
- Variadic values cannot be typed (they're implicitly `:list<any>`)

### `fn` and `defn`

- `defn` uses the same type syntax as `fn`

```scheme
(fn
  ; Typed <:int> return and <:list>/<:char> parameters
  (:int (:list a :char b . rest-args)
    (length (cons b (cons a rest-args))))

  ; Typeless return, required <:flo> parameter and optional <:int> parameter
  ((:flo a (:int b 42))
    (+ a b)))


(defn function-name
  ; Typed <:int> return and <:list>/<:char> parameters
  (:int (:list a :char b . rest-args)
    (length (cons b (cons a rest-args))))

  ; Typeless return, required <:flo> and optional <:int> parameters
  ((:flo a (:int b 42))
    (+ a b)))
```

### `lambda`

```scheme
; Typed <:int> return and <:list>/<:char> parameters
(lambda :int (:list a :char b . rest-args)
  (length (cons b (cons a rest-args))))

; Typeless return, required <:flo> and optional <:int> parameters
(lambda (:flo a (:int b 42))
  (+ a b))
```

### `define`

```scheme
; Typed <:int> return and <:list>/<:char> parameters
(define :int (function-name :list a :char b . rest-args)
  (length (cons b (cons a rest-args))))

; Typeless return, required <:flo> and optional <:int> parameters
(define (function-name :flo a (:int b 42))
  (+ a b))
```

### `define-generator`

- Only supports typed parameters, not typed returns, to account for
  `*generator-complete*` being returned from finite generators.

```scheme
; Required <:flo> and optional <:int> parameters
(define-generator (generator-factory-name :flo a (:int b 42))
  (let loop ((i b))
    (yield (+ i a))
    (loop (+ i 1))))
```

### `curry`

- Only type-checks the return value once all parameters have been applied.

```scheme
; Typed <:int> return and <:list>/<:char> parameters
(curry :int (:list a :char b)
  (length (cons b a)))
```

### `class`/`define-class` and `interface`/`define-interface`

- `class` supports types on instance and static methods
- `interface` only supports types on static methods
- `define-class` uses the same type syntax as `class`
  - as `define-interface` does with `interface`

```scheme
(define-class ClassName
  ; Instance: typed <:int> return and <:list>/<:char> parameters
  (:int (method-name-1 :list a :char b . rest-args)
    (length (cons b (cons a rest-args))))

  ; Instance: typeless return, required <:flo> and optional <:int> parameters
  ((method-name-2 :flo a (:int b 42))
    (+ a b))

  ; Static: typed <:int> return and <:list>/<:char> parameters
  (:static :int (method-name-1 :list a :char b . rest-args)
    (length (cons b (cons a rest-args))))

  ; Static: typeless return, required <:flo> and optional <:int> parameters
  (:static (method-name-2 :flo a (:int b 42))
    (+ a b)))


(define-interface InterfaceName
  ; Static: typed <:int> return and <:list>/<:char> parameters
  (:static :int (method-name-1 :list a :char b . rest-args)
    (length (cons b (cons a rest-args))))

  ; Static: typeless return, required <:flo> and optional <:int> parameters
  (:static (method-name-2 :flo a (:int b 42))
    (+ a b)))


; Mandating that <function-name> returns either <ClassName> or <InterfaceName>
(define :ClassName|InterfaceName (function-name)
  (ClassName))
```

---

## Type Aliases

Type aliases reference a preexisting keyword type, typically to
mask type complexity. For example, when implementing a `UserProfile`
class, it might be nicer to define a `:phone-number` type instead of
always using `:str|list<int>`.

Type aliases can be created by using `define-type` (aliased by `deftype`)
which is simply a convenience wrapper around `define` and `type-alias`.

- `(type-alias <type-keyword>)` creates a type alias value
- `(type-alias? <obj>)` returns whether `<obj>` is a type alias
- `(type-alias-source <type-alias>)` returns the original keyword type
  that `<type-alias>` references

### Example

```scheme
; Create a type-alias and dispatch on it
(define-type phone-number :str|list<int>)

(defn function-name
  ((:phone-number x) #t)
  ((:any x) #f))

(function-name "555-555-5555") ; #t
(function-name '(555 555 5555)) ; #t
(function-name 5555555555) ; #f
```

---

## Type Primitive

Use `(type-is? <obj> <type-keyword>)` to determine if `<obj>` is a `<type-keyword>`.
```

-------------------------------------------------------------------------------
# Variables


-------------------------------------------------------------------------------
## `#path`

### Description:
```
Reader literal that expands to the current file's parent path as a string.
```

-------------------------------------------------------------------------------
## `*argv*`

### Description:
```
A list of the command-line arguments as strings.
```

-------------------------------------------------------------------------------
## `*dosync-lock*`

### Description:
```
Mutex lock used by <dosync>. Defined as a parameter, hence maintains the same
state across modules.
```

-------------------------------------------------------------------------------
## `*dosync-module-lock*`

### Description:
```
Mutex lock used by <dosync-module>. Defined as a global variable, hence has
unique state per module.
```

-------------------------------------------------------------------------------
## `*escm-execution-command*`

### Description:
```
Execution command to run EScheme on the host machine. For example,
the below would run "script.scm" in a seperate process:
  (system (append *escm-execution-command* "script.scm"))
```

-------------------------------------------------------------------------------
## `*escm-path*`

### Description:
```
Path string to the hosting EScheme implementation's directory.
```

-------------------------------------------------------------------------------
## `*escm-version*`

### Description:
```
String representing the current EScheme version number.
```

-------------------------------------------------------------------------------
## `*file-separator*`

### Description:
```
The file seperator string used by the host operating system.
```

-------------------------------------------------------------------------------
## `*generator-complete*`

### Description:
```
Variable that resolves to the value returned by generators
(see <define-generator>) upon completion.
```

-------------------------------------------------------------------------------
## `*import*`

### Description:
```
Variable denoting whether the current file was loaded via <import> or not.
Can combine with <unless> to mimic Python's "if __name__=='__main__':" pattern:

  (unless *import*
    <execute-main-escheme-code-here> ...)
```

-------------------------------------------------------------------------------
## `*load-once-files*`

### Description:
```
Hashmap that stores the set of files already loaded by the current module.
Used by <load-once>.
```

-------------------------------------------------------------------------------
## `*max-priority*`

### Description:
```
Variable denoting the largest thread-priority supported by
<thread-set-priority!>. Also check out <*min-priority*>!
```

-------------------------------------------------------------------------------
## `*max-radix*`

### Description:
```
Variable denoting the largest radix supported by
<string->number> and <number->string>. Generally 36.
Also check out <*min-radix*>!
```

-------------------------------------------------------------------------------
## `*min-priority*`

### Description:
```
Variable denoting the smallest thread-priority supported by
<thread-set-priority!>. Also check out <*max-priority*>!
```

-------------------------------------------------------------------------------
## `*min-radix*`

### Description:
```
Variable denoting the smallest radix supported by
<string->number> and <number->string>. Generally 2.
Also check out <*max-radix*>!
```

-------------------------------------------------------------------------------
## `*os-architecture*`

### Description:
```
String of the host operating system's architecture.
#f if lacking security permission to access such.
```

-------------------------------------------------------------------------------
## `*os-name*`

### Description:
```
String of the host operating system's name.
#f if lacking security permission to access such.
```

-------------------------------------------------------------------------------
## `*os-version*`

### Description:
```
String of the host operating system's version.
#f if lacking security permission to access such.
```

-------------------------------------------------------------------------------
## `*path-separator*`

### Description:
```
The path seperator (think environment variables) string used
by the host operating system.
```

-------------------------------------------------------------------------------
## `*user-home*`

### Description:
```
String of the system's user home directory.
#f if lacking security permission to access such.
```

-------------------------------------------------------------------------------
## `*user-name*`

### Description:
```
String of the system's active user name.
#f if lacking security permission to access such.
```
