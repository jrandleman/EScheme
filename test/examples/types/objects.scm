(define-interface IPerson
  name
  age)

(define-class Person (:implements IPerson)
  (name "")
  (age 0)
  ((new :string name :int age)
    (define self.name name)
    (define self.age age)))