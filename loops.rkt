#lang racket

(values 42)

(values 'this 'and-this 'and-that)

(define-values (one two three) (values 'one 'two 'three))

(define-values (x y)
  (if (= 1 2)
      (values 10 20)
      (values 42 55)))

(for/fold ([sqrs 0]
           [count 0])
          ([i '(1 2 3 4 5 6 7 8 9 10)])
  (values (+ (sqr i) sqrs)
          (if (> (sqr i) 50)
              (add1 count)
              count)))


(for/list ([i '(1 2 3 4 5)]
           #:when (odd? i))
  i)

(for/fold ([sum 0])
          ([i '( 1 2 3 4 5)]
           #:when (even? i))
  (+ sum 1))

(for/list ([i '(1 2 3 4 5)]
           [j '(1 2 3 4)]
           [k '(5 4 3 2 1)])
  (list i j k))

(for/list ([i '(1 2 3 4 5)]
           [s '("a" "b" "c" "d" "e")]
           #:when (and (even? i) (string=? s "d")))
  i)

(for* ([i '(1 2 3)]
       [j '(1 2 3)]
       [k '(1 2 3)])
  (displayln (list i j k)))




(for*/list ([k '((1 2) (3 4) (5 6) (7 8))]
            [n k])
  n)

(for* ([i (in-range 10)]
       [j (in-range 15 20)]
       [k (in-range 0 10 2)])
  (displayln (list i j k)))

(for*/list ([i '(1 2 3)]
            [j '(4 5 6)])
  (+ i j))

(for/list ([i '(1 2 3)]
           [j '(4 5 6)])
  (+ i j))



(for/product ([i '(1 2)])
     i)



