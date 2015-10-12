#lang racket

(struct student (name id# dorm))

(define joe (student 'Joe 1234 'Steeb))
(define ryan (student 'Ryan 1234 'Smith))
(define ellie (student 'Ellie 1234 'Steeb))
(define reid (student 'Reid 1234 'Smith))
(define anne (student 'Anne 1234 'Steeb))
(define in-class (list joe ryan ellie reid anne))
(student-name (third in-class))

(define freshman1 (student 'Joe 1234 'NewHall))

(struct student-body (freshmen sophomores juniors seniors))
(define all-students
  (student-body (list freshman1 (student 'Mary 1010 `OldHall))
                (list (student 'Jeff 5671 'OldHall))
                (list (student 'Bob 4321 'Apartment))
                empty))

  (student-name (first (student-body-freshmen all-students)))


(struct example (x y z) #:transparent)
(define ex1 (example 1 2 3))

(if (= (+ 1 2) 3)
    'yup
    'nope)


(define (my-length a-list)
  (if (empty? a-list)
      0
      (add1 (my-length (rest a-list)))))

(my-length '(list with four symbols))