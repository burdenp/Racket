#lang pl 02

(: sequence : (All (A) (-> Any Any)) Any Any -> (Listof Any))
;;takes in a function and two values. Then outputs a list of values
;;that contains each value from the first value to the last value
;;with each value in the list being the last value with the function
;;applied to it
(define (sequence f first last)
  (cond
    [(equal? first last) '(last)]
    [else (cons first (sequence f (f first) last))]))

(test (sequence add1 1 1) => (list 1))
(test (sequence add1 1 5) => (list 1 2 3 4 5))
(test (sequence sub1 5 1) => (list 5 4 3 2 1))
(test (sequence sqrt 65536 2) => (list 65536 256 16 4 2))
(test (sequence not #f #t) => (list #f #t))
(test (sequence not #t #f) => (list #t #f))

(define-type INTSET
  [Num   Integer]
  [Range INTSET INTSET] 
  [2Sets INTSET INTSET])

(: intset-min/max : (Listof INTSET) (All (A) (-> Integer Integer)) -> Integer)
;;takes in a function either < or > and finds either the largest number
;;or the smallest number in the set
(define (intset-min/max set func)
  (cond
    [(null? set) true]
    [(null? (rest set)) true]
    [(equal? func >) (if (< (first set) (first (rest set)))
                         (intset-min/max (rest set) func) #f)]
    [(equal? func <) (if (> (first set) (first (rest set)))
                         (intset-min/max  (rest set) func) #f)]
    [else "error"]))

(: intset-min : INTSET -> Integer)
;; Finds the minimal member of the given set.
(define (intset-min set) (intset-min/max set <))

(: intset-max : INTSET -> Integer)
;; Finds the maximal member of the given set.
(define (intset-max set) (intset-min/max set >))

#|
  <pages>::= <int>
           |{ <int> }
           |{ <int>, <pages>}
           |{ <int> - <pages>}
|#

(define minutes-spent 60)