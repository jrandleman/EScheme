;; Author: Jordan Randleman -- string.scm
;; => Tests for EScheme's primitive functions.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load #path (append ".." *file-separator* ".." *file-separator* "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(ut (string) "")
(ut (string "abc") "abc")
(ut (string #\space 3 '() "abc" :e) " 3()abc:e")

(ut (string-java-length "abc") 3)
(ut (string-java-length "\U0001F525") 2) ; java strings are processed by Java char's
(ut (length "\U0001F525") 1)             ; escm strings are processed by codepoint

(ut (string-upcase "") "")
(ut (string-upcase "aBc") "ABC")

(ut (string-downcase "") "")
(ut (string-downcase "AbC") "abc")

(ut (string-escape "\n\t\U0001f525") "\\n\\t\\U0001f525")         ; escm's escape supports 32-bit unicode literals
(ut (string-java-escape "\n\t\U0001f525") "\\n\\t\\ud83d\\udd25") ; java's escape supports 16-bit unicode literals (processes 32-bits as 16-bit pairs)

(ut (string-unescape (string-escape "\n\t\U0001f525")) "\n\t\U0001f525")
(ut (string-unescape (string-java-escape "\n\t\U0001f525")) "\n\t\U0001f525")

(ut (string-replace "Hi! My email is random@gmail.com :)" "@[^\\.]+.[^ \\.]{3}" "@scu.edu") "Hi! My email is random@scu.edu :)")

(ut (string-trim "abc") "abc")
(ut (string-trim " abc") "abc")
(ut (string-trim "abc ") "abc")
(ut (string-trim " abc ") "abc")

(ut (string-contains "hello hello" "abc") #f)
(ut (string-contains "hello hello" "he") 0)
(ut (string-contains "hello hello" "lo") 3)
(ut (string-contains "hello hello" "ell") 1)

(ut (string-contains-right "hello hello" "abc") #f)
(ut (string-contains-right "hello hello" "he") 6)
(ut (string-contains-right "hello hello" "lo") 9)
(ut (string-contains-right "hello hello" "ell") 7)

(ut (string-join '()) "")
(ut (string-join '() "--") "")
(ut (string-join '("hey")) "hey")
(ut (string-join '("hey") "--") "hey")
(ut (string-join '("hey" "how" "are" "you")) "heyhowareyou")
(ut (string-join '("hey" "how" "are" "you") "--") "hey--how--are--you")

(ut (string-split "") '(""))
(ut (string-split "\U0001f525") '("\U0001f525"))
(ut (string-split "hello") '("h" "e" "l" "l" "o"))
(ut (string-split "\U0001f525hello") '("\U0001f525" "h" "e" "l" "l" "o"))
(ut (string-split "hello\U0001f525") '("h" "e" "l" "l" "o" "\U0001f525"))
(ut (string-split "hello\U0001f525\U0001f525") '("h" "e" "l" "l" "o" "\U0001f525" "\U0001f525"))
(ut (string-split "hello\U0001f525\U0001f5252") '("h" "e" "l" "l" "o" "\U0001f525" "\U0001f525" "2"))
(ut (string-split "hello there friend" " ") '("hello" "there" "friend"))
(ut (string-split "hello there friend " " ") '("hello" "there" "friend")) ; behavior inherited from java
(ut (string-split " hello there friend" " ") '("" "hello" "there" "friend")) ; behavior inherited from java
(ut (string-split " hello there friend " " ") '("" "hello" "there" "friend")) ; behavior inherited from java

(define (char-math op num ch) (char-for-digit (op num (char-digit ch))))
(ut (string-unfold (bind char<? #\4) (bind char-math * 2) (bind char-math + 1) #\0) "02468")
(ut (string-unfold (bind char<? #\4) (bind char-math * 2) (bind char-math + 1) #\5) "")

(ut (string-unfold-right (bind char<? #\4) (bind char-math * 2) (bind char-math + 1) #\0) "86420")
(ut (string-unfold-right (bind char<? #\4) (bind char-math * 2) (bind char-math + 1) #\5) "")

(ut (string=? "a") #t)
(ut (string=? "a" "a" "a") #t)
(ut (string=? "a" "A" "a") #f)
(ut (string=? "b" "a" "a") #f)
(ut (string=? "a" "b" "a") #f)
(ut (string=? "a" "a" "b") #f)
(ut (string=? "a" "B" "c") #f)
(ut (string=? "c" "B" "a") #f)

(ut (string<? "a") #t)
(ut (string<? "a" "a" "a") #f)
(ut (string<? "a" "A" "a") #f)
(ut (string<? "a" "b" "c") #t)
(ut (string<? "b" "a" "c") #f)
(ut (string<? "b" "c" "a") #f)
(ut (string<? "a" "B" "c") #f)
(ut (string<? "c" "B" "a") #f)

(ut (string<=? "a") #t)
(ut (string<=? "a" "a" "a") #t)
(ut (string<=? "a" "A" "a") #f)
(ut (string<=? "a" "b" "c") #t)
(ut (string<=? "b" "a" "c") #f)
(ut (string<=? "b" "c" "a") #f)
(ut (string<=? "a" "B" "c") #f)
(ut (string<=? "c" "B" "a") #f)

(ut (string>? "a") #t)
(ut (string>? "a" "a" "a") #f)
(ut (string>? "a" "A" "a") #f)
(ut (string>? "c" "b" "a") #t)
(ut (string>? "c" "a" "b") #f)
(ut (string>? "a" "c" "b") #f)
(ut (string>? "a" "B" "c") #f)
(ut (string>? "c" "B" "a") #f)

(ut (string>=? "a") #t)
(ut (string>=? "a" "a" "a") #t)
(ut (string>=? "a" "A" "a") #f)
(ut (string>=? "c" "b" "a") #t)
(ut (string>=? "c" "a" "b") #f)
(ut (string>=? "a" "c" "b") #f)
(ut (string>=? "a" "B" "c") #f)
(ut (string>=? "c" "B" "a") #f)

(ut (string-ci=? "a") #t)
(ut (string-ci=? "a" "a" "a") #t)
(ut (string-ci=? "a" "A" "a") #t)
(ut (string-ci=? "b" "a" "a") #f)
(ut (string-ci=? "a" "b" "a") #f)
(ut (string-ci=? "a" "a" "b") #f)
(ut (string-ci=? "a" "B" "c") #f)
(ut (string-ci=? "c" "B" "a") #f)

(ut (string-ci<? "a") #t)
(ut (string-ci<? "a" "a" "a") #f)
(ut (string-ci<? "a" "A" "a") #f)
(ut (string-ci<? "a" "b" "c") #t)
(ut (string-ci<? "b" "a" "c") #f)
(ut (string-ci<? "b" "c" "a") #f)
(ut (string-ci<? "a" "B" "c") #t)
(ut (string-ci<? "c" "B" "a") #f)

(ut (string-ci<=? "a") #t)
(ut (string-ci<=? "a" "a" "a") #t)
(ut (string-ci<=? "a" "A" "a") #t)
(ut (string-ci<=? "a" "b" "c") #t)
(ut (string-ci<=? "b" "a" "c") #f)
(ut (string-ci<=? "b" "c" "a") #f)
(ut (string-ci<=? "a" "B" "c") #t)
(ut (string-ci<=? "c" "B" "a") #f)

(ut (string-ci>? "a") #t)
(ut (string-ci>? "a" "a" "a") #f)
(ut (string-ci>? "a" "A" "a") #f)
(ut (string-ci>? "c" "b" "a") #t)
(ut (string-ci>? "c" "a" "b") #f)
(ut (string-ci>? "a" "c" "b") #f)
(ut (string-ci>? "a" "B" "c") #f)
(ut (string-ci>? "c" "B" "a") #t)

(ut (string-ci>=? "a") #t)
(ut (string-ci>=? "a" "a" "a") #t)
(ut (string-ci>=? "a" "A" "a") #t)
(ut (string-ci>=? "c" "b" "a") #t)
(ut (string-ci>=? "c" "a" "b") #f)
(ut (string-ci>=? "a" "c" "b") #f)
(ut (string-ci>=? "a" "B" "c") #f)
(ut (string-ci>=? "c" "B" "a") #t)

(ut (string? "") #t)
(ut (string? "abc") #t)
(ut (string? 0) #f)
(ut (string? []) #f)
