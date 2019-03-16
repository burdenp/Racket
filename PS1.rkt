;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname PS1) (read-case-sensitive #t) (teachpacks ((lib "guess.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "guess.rkt" "teachpack" "htdp")))))
;;max-min: num a num b num c -> num
;;helper that calls max and min of abc and subtracts them from each other
(define (max-min a b c)
  (- (max a b c) (min a b c)))
;;near?: num a num b num c -> boolean
;;sees if the ints are within 2 of each other.
(define (near? a b c)
 (cond [(> (max-min a b c) 2) false]
 [else true]))
(equal? true (near? 1 -1 1))
(equal? false (near? 4 -1 1))

;;count-xs: List -> num
;;returns the number of x's in the list
(define (count-xs list)
  (cond [(empty? list) 0]
        [(equal? (quote x) (first list)) (+ 1 (count-xs (rest list)))]
        [else (count-xs (rest list))]))
(equal? 1 (count-xs (cons (quote x) '())))
(equal? 2 (count-xs (cons (quote x) (cons (quote x) '()))))
(equal? 0 (count-xs (cons (quote a) '())))

;;ascending-helper: num num -> boolean
;;returns true if the list is in ascending order
(define (ascending-helper a b)
  (cond [(or(< a b) (= a b)) true]
        [else false]))
;;ascending?: List of nums -> boolean
;;returns true if the list is in ascending order
(define (ascending? list)
  (cond [(or(empty? list) (empty? (rest list))
        (ascending-helper (first list) (first (rest list)))) true]
        [else false]))
(equal? true (ascending? (cons 1 (cons 2 (cons 3 '())))))
(equal? false (ascending? (cons 2 (cons 1 '()))))

;;combiner: a b -> list
;;helper for zip2 combines 2 givens into a list
(define (combiner a b)
  (list a b))  
;;zip2: List List -> List
;;takes in 2 lists of equal size and returns a list with
;;elements from each list paired together in a list within a list
;;i.e.(zip2 (cons a '()) (cons b '())) becomes (cons (cons a (cons b '())) '())
(define (zip2 list1 list2)
  (cond [(empty? list1) '()]
        [else (cons (combiner (first list1) (first list2))
                    (zip2 (rest list1) (rest list2)))]))
(equal? (list (list 1 'a) (list 2 'b) (list 3 'c))
        (zip2 (list 1 2 3) (list 'a 'b 'c)))
(equal? '() (zip2 '() '()))

;;There was no picture of me in your list, I'm sorry I've been sick
;;So I have missed your class, I'd like to meet with you sometime
;;to go over anything that I may have missed if possible
(define my-picture 0)

;;total minutes spent over 2 days, most of the time was reconfiguring racket
;;and figuring out how to reuse this language
;;I expect to spend about 120 minutes on each other assignment
(define minutes-spent 360)