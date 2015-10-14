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

(define x 5)
(define y 7)
(define z 9)

(and (odd? x) (odd? y) (odd? z))

(define is-it-even #f)

(or (odd? x) (set! is-it-even #t))

(and (even? x) (set! is-it-even #t))

(member 4 (list 1 3 4 5 4))

(define tasks '(1 clean 3 homework 4 party))
(member 3 tasks)

(struct point (x y) #:transparent)

(define p (point 10 100))

(define (distance-to-origin p)
  (sqrt (+ (sqr (point-x p)) (sqr (point-y p)))))

(distance-to-origin (point 3 4))

(define pt1 (point -1 2))
(define pt2 (point -1 2))

(equal? pt1 pt2)
(eq? pt1 pt1)
(eq? pt1 pt2)

(define pt3 pt1)
(eq? pt1 pt3)

(define (eq-first-items list1 list2)
  (eq? (first list1) (first list2)))

(eq-first-items (cons pt1 empty) (cons pt3 empty))

