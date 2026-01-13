;; Author: Jordan Randleman -- json.scm
;; => Tests for EScheme's primitive functions.
;;    Invoked by ../../main.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS
(load (path (path-parent #path 2) "lib.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(ut (json->data "null") '())
(ut (json->data "true") #t)
(ut (json->data "false") #f)
(ut (json->data "-1.0") -1.0)
(ut (json->data "0.0") 0.0)
(ut (json->data "1.0") 1.0)
(ut (json->data "\"abc\"") "abc")
(ut (json->data "[1.0,2.0,3.0]") [1.0 2.0 3.0])
(ut (json->data "[[1.0,2.0,3.0],[4.0,5.0,6.0],[7.0,8.0,9.0]]") [[1.0 2.0 3.0] [4.0 5.0 6.0] [7.0 8.0 9.0]])
(ut (json->data "{\"a\":1.0,\"b\":2.0,\"c\":3.0}") {"a" 1.0 "b" 2.0 "c" 3.0})
(ut (json->data "{\"a\":{\"1\":true},\"b\":{\"2\":false},\"c\":{\"3\":true}}") {"a" {"1" #t} "b" {"2" #f} "c" {"3" #t}})

(ut (data->json '()) "null")
(ut (data->json #t) "true")
(ut (data->json #f) "false")
(ut (data->json -1.0) "-1.0")
(ut (data->json 0.0) "0.0")
(ut (data->json 1.0) "1.0")
(ut (data->json "abc") "\"abc\"")
(ut (data->json [1.0 2.0 3.0]) "[1.0,2.0,3.0]")
(ut (data->json [[1.0 2.0 3.0] [4.0 5.0 6.0] [7.0 8.0 9.0]]) "[[1.0,2.0,3.0],[4.0,5.0,6.0],[7.0,8.0,9.0]]")
(ut (data->json {"a" 1.0 "b" 2.0 "c" 3.0}) "{\"a\":1.0,\"b\":2.0,\"c\":3.0}")
(ut (data->json {"a" {"1" #t} "b" {"2" #f} "c" {"3" #t}}) "{\"a\":{\"1\":true},\"b\":{\"2\":false},\"c\":{\"3\":true}}")
(ut 
  (data->json {"a" {"1" #t} "b" {"2" #f} "c" {"3" #t}} 2)
  "{\n  \"a\": {\n    \"1\": true\n  },\n  \"b\": {\n    \"2\": false\n  },\n  \"c\": {\n    \"3\": true\n  }\n}")

(ut (json-datum? '()) #t)
(ut (json-datum? #t) #t)
(ut (json-datum? #f) #t)
(ut (json-datum? -1.0) #t)
(ut (json-datum? 0.0) #t)
(ut (json-datum? 1.0) #t)
(ut (json-datum? "abc") #t)
(ut (json-datum? [1.0 2.0 3.0]) #t)
(ut (json-datum? [[1.0 2.0 3.0] [4.0 5.0 6.0] [7.0 8.0 9.0]]) #t)
(ut (json-datum? {"a" 1.0 "b" 2.0 "c" 3.0}) #t)
(ut (json-datum? {"a" {"1" #t} "b" {"2" #f} "c" {"3" #t}}) #t)
(ut (json-datum? 'a) #f)
(ut (json-datum? :a) #f)
(ut (json-datum? '(a)) #f)
(ut (json-datum? ['a 'b 'c]) #f)
(ut (json-datum? {'a 1 'b 2 'c 3}) #f)
(ut (json-datum? {"a" 'a "b" 'b "c" 'c}) #f)

(ut (json-string?  "null") #t)
(ut (json-string? "true") #t)
(ut (json-string? "false") #t)
(ut (json-string? "-1.0") #t)
(ut (json-string? "0.0") #t)
(ut (json-string? "1.0") #t)
(ut (json-string? "\"abc\"") #t)
(ut (json-string? "[1.0,2.0,3.0]") #t)
(ut (json-string? "[[1.0,2.0,3.0],[4.0,5.0,6.0],[7.0,8.0,9.0]]") #t)
(ut (json-string? "{\"a\":1.0,\"b\":2.0,\"c\":3.0}") #t)
(ut (json-string? "{\"a\":{\"1\":true},\"b\":{\"2\":false},\"c\":{\"3\":true}}") #t)
(ut 
  (json-string? "{\n  \"a\": {\n    \"1\": true\n  },\n  \"b\": {\n    \"2\": false\n  },\n  \"c\": {\n    \"3\": true\n  }\n}")
  #t)
(ut (json-string?  "") #f)
(ut (json-string?  "abc") #f)
(ut (json-string?  "[1,2") #f)
(ut (json-string?  "[1 2]") #f)
(ut (json-string? "{true:1.0,false:2.0,true:3.0}") #f)
