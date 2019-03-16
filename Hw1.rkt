;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Hw1) (read-case-sensitive #t) (teachpacks ((lib "guess.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "guess.rkt" "teachpack" "htdp")))))
;;bin4-to-num: (binary num)(binary num)(binary num)(binary num) -> num
;;takes 4 binary numbers from least significant digit to most and
;;converts it to a decimal number
(define (bin4-to-num a b c d)
  (+ a (* b 2) (* c 4) (* d 8)))

(equal? 13 (bin4-to-num 1 0 1 1))
(equal? 0 (bin4-to-num 0 0 0 0))
(equal? 15 (bin4-to-num 1 1 1 1))

;;gcd2: non-negative integer non-negative integer -> num
;;takes in two non-negative integers and returns their greatest common
;;divisor.
(define (gcd2 a b)
  (cond
    [(= a 0) b]
    [(= b 0) a]
    [(even? a) (cond
                 [(even? b) (* 2 (gcd2 (/ a 2) (/ b 2)))]
                 [else (gcd2 (/ a 2) b)])]
    [else (cond
            [(even? b) (gcd2 a (/ b 2))]
            [(>= a b) (gcd2 (/ (- a b) 2) b)]
            [else (gcd2 (/ (- b a) 2) a)])]))

(equal? 2 (gcd2 0 2))
(equal? 2 (gcd2 2 0))
(equal? 18 (gcd2 378 144))
(equal? 36 (gcd2 216 612))
(equal? 3 (gcd2 12 3))
(equal? 3 (gcd2 15 12))
(equal? 3 (gcd2 33 15))
(equal? 3 (gcd2 15 33))


;;all-even?: list of integers -> boolean
;;takes in a list of integers and returns true if all are even
;;else returns false
(define (all-even? list1)
  (cond
    [(empty? list1) true]
    [(even? (first list1)) (all-even? (rest list1))]
    [else false]))

(all-even? (list 2 4 6 8))
(not (all-even? (list 1 3 5 7)))
(all-even? '())

;;merge-lists: list of num list of num -> list of num
;;takes in two lists that are sorted in ascending order and merges them
;;into one list in ascending order
(define (merge-lists list1 list2)
  (cond
    [(and (empty? list1) (empty? list2)) empty]
    [(empty? list1) list2]
    [(empty? list2) list1]
    [(<= (first list1) (first list2))
     (cons (first list1) (merge-lists (rest list1) list2))]
    [else (cons (first list2) (merge-lists list1 (rest list2)))]))

(equal? '() (merge-lists '() '()))
(equal? '(a) (merge-lists '(a) '()))
(equal? '(b) (merge-lists '() '(b)))
(equal? '(1 2 3 4) (merge-lists '(1 2) '(3 4)))
(equal? '(1 2 3 4) (merge-lists '(3 4) '(1 2)))
(equal? '(1 2 3) (merge-lists '(1 3) '(2)))
(equal? '(1 2 3) (merge-lists '(2) '(1 3)))


(define my-picture 81)
(define my-other-pictures '())
(define minutes-spent 60)
