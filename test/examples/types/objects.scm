(define-interface IPerson
  name
  age)

(define-class Person (:implements IPerson)
  (name "")
  (age 0)
  ((new :str name :int age)
    (define self.name name)
    (define self.age age)))

(define-type personalias :Person)
