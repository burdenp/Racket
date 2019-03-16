;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |fundies spring hw 10|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;------------------------------------------------------------------------------
;;Problem 1
;;------------------------------------------------------------------------------

;; sort : Listof[Number] -> Listof[Number]
;; to construct a list with all items from alon in increasing order
(define (sort-a alon)
  (local ((define (insert an alon)
            (cond
              [(empty? alon) (list an)]
              [else (cond
                      [(< an (first alon)) (cons an alon)]
                      [else (cons (first alon)
                                  (insert an (rest alon)))])])))
    (cond
      [(empty? alon) empty]
      [else (insert (first alon) (sort-a (rest alon)))])))

(check-expect (sort-a empty) empty)
(check-expect (sort-a '(1 2 3 4 5))(list 1 2 3 4 5))
(check-expect (sort-a '(1 2 8 3 7 4 6 5)) (list 1 2 3 4 5 6 7 8))
(check-expect (sort-a '(10 9 8 7 6 5)) (list 5 6 7 8 9 10))

(define (sort-b alon op)
  (local ((define (extreme x y)
            (cond [(empty? y) x]
                  [(op y x) y]
                  [else x]))
          (define (remove x list)
            (cond [(empty? list) empty]
                  [(= x (first list)) (rest list)]
                  [else (cons (first list)(remove x (rest list)))])))
    (cond [(empty? alon) empty]
          [else
           (cons (foldr extreme empty alon)(sort-b (remove(foldr extreme empty alon) alon) op))])))
(check-expect (sort-b empty <) empty)
(check-expect (sort-b '(1 2 3 4 5) <)(list 1 2 3 4 5))
(check-expect (sort-b '(1 2 8 3 7 4 6 5) <) (list 1 2 3 4 5 6 7 8))
(check-expect (sort-b '(10 9 8 7 6 5) <) (list 5 6 7 8 9 10))

(define (sort-ascending LoN)
  (sort-b LoN <))
(check-expect (sort-ascending empty) empty)
(check-expect (sort-ascending '(1 2 3 4 5))(list 1 2 3 4 5))
(check-expect (sort-ascending '(1 2 8 3 7 4 6 5)) (list 1 2 3 4 5 6 7 8))
(check-expect (sort-ascending '(10 9 8 7 6 5)) (list 5 6 7 8 9 10))

(define (sort-descending LoN)
  (sort-b LoN >))
(check-expect (sort-descending empty) empty)
(check-expect (sort-descending '(1 2 3 4 5))(list 5 4 3 2 1))
(check-expect (sort-descending '(1 2 8 3 7 4 6 5)) (list 8 7 6 5 4 3 2 1))
(check-expect (sort-descending '(10 9 8 7 6 5)) (list 10 9 8 7 6 5))

;;------------------------------------------------------------------------------
;;Problem 2
;;------------------------------------------------------------------------------

;; A Grade is: (make-grade Symbol Number)
(define-struct grade (letter num))

;; The Symbol in a Grade represents
;; 'A >= 90
;; 'B >= 80
;; 'C >= 70
;; 'D >= 60
;; 'F < 60

;; A [Listof Grades] ...
(define grades
  (list (make-grade 'D 62) (make-grade 'C 79) (make-grade 'A 93)
        (make-grade 'B 84) (make-grade 'F 57) (make-grade 'F 38)
        (make-grade 'A 90) (make-grade 'A 95) (make-grade 'C 76)
        (make-grade 'A 90) (make-grade 'F 55) (make-grade 'C 74)
        (make-grade 'A 92) (make-grade 'B 86) (make-grade 'F 43)
        (make-grade 'C 73)))

;;log->los: {listof Grades} -> {Listof Symbols}
;;converts a [listof Grade] into a [Listof Symbol] that contains 
;;just the letter grade.
(define (log->los log)
  (map (λ (g) (grade-letter g)) log))

(check-expect (log->los empty) empty)
(check-expect (log->los  (list (make-grade 'D 62) (make-grade 'C 79) 
                               (make-grade 'A 93) (make-grade 'B 84) 
                               (make-grade 'F 57) (make-grade 'F 38))) 
              (list 'D 'C 'A 'B 'F 'F))
(check-expect (log->los  (list (make-grade 'D 62) (make-grade 'C 79) 
                               (make-grade 'A 93) (make-grade 'B 84) 
                               (make-grade 'F 57) (make-grade 'F 38)
                               (make-grade 'A 90) (make-grade 'A 95) (make-grade 'C 76)
                               (make-grade 'A 90) (make-grade 'F 55) (make-grade 'C 74)
                               (make-grade 'A 92) (make-grade 'B 86) (make-grade 'F 43)
                               (make-grade 'C 73))) (list 'D 'C 'A 'B 'F 'F 'A 'A 'C 'A 'F 'C 
                                                          'A 'B 'F 'C))

;;average-grade: {listof Grades} -> Number
;;that finds the average (number) Grade in a [Listof Grade]
#|
(define (average-grade log)
  (/ (foldr (lambda (x result) (+ (grade-num x) result)) 0 log) (length log)))

(define (total-sum log)
  (cond [(empty? log) 0]
        [else (+ (grade-num (first log)) (total-sum (rest log)))]))

(check-expect (average-grade empty) 0)
(check-expect (average-grade  (list (make-grade 'D 62) (make-grade 'C 79) 
                                    (make-grade 'A 93) (make-grade 'B 84) 
                                    (make-grade 'F 57) (make-grade 'F 38))) 68.833) 

(check-expect (average-grade  (list (make-grade 'D 62) (make-grade 'C 79) 
                                    (make-grade 'A 93) (make-grade 'B 84) 
                                    (make-grade 'F 57) (make-grade 'F 38)
                                    (make-grade 'A 90) (make-grade 'A 95) (make-grade 'C 76)
                                    (make-grade 'A 90) (make-grade 'F 55) (make-grade 'C 74)
                                    (make-grade 'A 92) (make-grade 'B 86) (make-grade 'F 43)
                                    (make-grade 'C 73))) 74.1875)
|#
;;all-above-79: {listof Grades} -> {listof Grades} 
;;returns a list of only the grades that are above 79.
(define (all-above-79 log)
  (filter (λ (x) (> (grade-num x) 79)) log))

(check-expect (all-above-79 empty) empty)
(check-expect (all-above-79  (list (make-grade 'D 62) (make-grade 'C 79) 
                                   (make-grade 'A 93) (make-grade 'B 84) 
                                   (make-grade 'F 57) (make-grade 'F 38))) 
              (list (make-grade 'A 93) (make-grade 'B 84)))         
(check-expect (all-above-79  (list (make-grade 'D 62) (make-grade 'C 79) 
                                   (make-grade 'A 93) (make-grade 'B 84) 
                                   (make-grade 'F 57) (make-grade 'F 38)
                                   (make-grade 'A 90) (make-grade 'A 95)
                                   (make-grade 'C 76) (make-grade 'A 90) 
                                   (make-grade 'F 55) (make-grade 'C 74)
                                   (make-grade 'A 92) (make-grade 'B 86) 
                                   (make-grade 'F 43) (make-grade 'C 73)))
              (list (make-grade 'A 93) (make-grade 'B 84)                    
                    (make-grade 'A 90) (make-grade 'A 95)
                    (make-grade 'A 90) 
                    (make-grade 'A 92) (make-grade 'B 86))) 

;;all-pass? {Listof Grades} -> Boolean
;;checks to see if all the Grades in a given list are not 'F.
(define (all-pass? log)
  (andmap (λ (x) (not (symbol=? 'F (grade-letter x)))) log))

(check-expect (all-pass? empty) true)
(check-expect (all-pass?  (list (make-grade 'D 62) (make-grade 'C 79) 
                                (make-grade 'A 93) (make-grade 'B 84))) true)                            
(check-expect (all-pass?  (list (make-grade 'D 62) (make-grade 'C 79) 
                                (make-grade 'A 93) (make-grade 'B 84) 
                                (make-grade 'F 57) (make-grade 'F 38)
                                (make-grade 'A 90) (make-grade 'A 95)
                                (make-grade 'C 76) (make-grade 'A 90) 
                                (make-grade 'F 55) (make-grade 'C 74)
                                (make-grade 'A 92) (make-grade 'B 86) 
                                (make-grade 'F 43) (make-grade 'C 73)))
              false)

;;bonus: {Listof Grades} -> {Listof Grades} 
;;adds 5 to all of the Grades in a givenlist,
;;and updates the letter portion of the Grade if it changes.
(define (bonus log)
  (map change-grade 
       (map (λ (x) (make-grade (grade-letter x) (+ 5 (grade-num x)))) log)))
;; change-grade : Grade -> Grade
;; takes a grade, and gives it the proper letter grade
(define (change-grade g)
  (local ((define num (grade-num g)))
    (cond [(>= num 90) (make-grade 'A num)]
          [(>= num 80) (make-grade 'B num)]
          [(>= num 70) (make-grade 'C num)]
          [(>= num 60) (make-grade 'D num)]
          [else
           (make-grade 'F num)])))

(check-expect (bonus empty) empty)
(check-expect (bonus  (list (make-grade 'D 62) (make-grade 'C 79) 
                            (make-grade 'A 93) (make-grade 'B 84)))
              (list (make-grade 'D 67) (make-grade 'B 84) (make-grade 'A 98) 
                    (make-grade 'B 89)))
(check-expect (bonus  (list (make-grade 'D 62) (make-grade 'C 79) 
                            (make-grade 'A 93) (make-grade 'B 84) 
                            (make-grade 'F 57) (make-grade 'F 38)
                            (make-grade 'A 90) (make-grade 'A 95)
                            (make-grade 'C 76) (make-grade 'A 90) 
                            (make-grade 'F 55) (make-grade 'C 74)
                            (make-grade 'A 92) (make-grade 'B 86) 
                            (make-grade 'F 43) (make-grade 'C 73)))
              (list (make-grade 'D 67) (make-grade 'B 84) 
                    (make-grade 'A 98) (make-grade 'B 89) 
                    (make-grade 'D 62) (make-grade 'F 43)
                    (make-grade 'A 95) (make-grade 'A 100)
                    (make-grade 'B 81) (make-grade 'A 95) 
                    (make-grade 'D 60) (make-grade 'C 79)
                    (make-grade 'A 97) (make-grade 'A 91) 
                    (make-grade 'F 48) (make-grade 'C 78)))
;;------------------------------------------------------------------------------
;;Problem 26.1.1
;;------------------------------------------------------------------------------

;;tabulate-div: Number>=1 -> Listof Numbers
;; tabulates the list of all of its divisors, starting with 1 and ending in n
#|
(define (tabulate-div num)
  (cond [(= 1 num) (list 1)]
        [(= num (+ 1 
        [(= (remainder num (- num 1)) 0) (list 1  
  |#

