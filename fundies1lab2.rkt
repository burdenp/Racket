;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname fundies1lab2) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
(require 2htdp/image)
(require 2htdp/universe)

;;Exercise 1
;;unary: num -> string
;;turns a number into unary represented by I
(define (unary num)
  (string-append (replicate num "I") " (" (number->string num) ")")) 
(check-expect(unary 2) "II (2)") 
(check-expect(unary 3) "III (3)")    
(check-expect(unary 4) "IIII (4)") 

;;Exercise 2
;;unary-add: num num -> string
;;turns 2 numbers into a string representing addition in unary
(define (unary-add num1 num2)
  (string-append (unary num1) " + "(unary num2) "= " (unary (+ num1 num2))))
(check-expect(unary-add 1 1) "I (1) + I (1)= II (2)") 
(check-expect(unary-add 1 2) "I (1) + II (2)= III (3)")    
(check-expect(unary-add 2 2) "II (2) + II (2)= IIII (4)")

;;Exercise 3
;;Stepper
  
;;Exercise 4
;;More stepper
#|(+ (sqr 2)
   (* (round 3.2)
      (- (sqrt 4)
         (/ 42 6))))
(place-image (circle 10 "solid" "black") 70 70
             (place-image (circle 10 "solid" "black") 30 70
                          (place-image (rectangle 80 40 "solid" "red") 50 50
                                       (empty-scene 100 100))))
|#
;;very little they are both primative data types one renders an image to the screen and the other doesn't

;;Exercise 5
;;rental-fee: num -> num
;;takes in a number and returns how much the rental fees are. Up to and 
;;including the 3rd day there are no fees after that for 1 week (the 10th day)
;;the fee increases by $3 for each day over the orignal 3 then after that day
;;it costs $20 flat. 
(define (rental-fee num)
  (cond [(<= num 3) 0]
        [(< num 10) (* 3 (- num 3))]
        [else 20]))
(check-expect(rental-fee 1) 0)
(check-expect(rental-fee 3) 0) 
(check-expect(rental-fee 4) 3)  
(check-expect(rental-fee 5) 6) 
(check-expect(rental-fee 10) 20)

;;Exercise 6
;;if it returns false then it doesn't look into the possible answer
;;and moves down otherwise it returns the answer

;;Exercise 7
;;sales-tax: num num -> num
;;returns the final price of an item with sales tax
(define (sales-tax price tax)
  (+ price (* price tax)))
(check-expect(sales-tax 20 .05) 21)
(check-expect(sales-tax 10 .05) 10.5)

;;Exercise 8
;;cond-tax: num num -> num
;;returns the final price of an item with sales tax but tax only exists
;;when the number is over 100.
(define (cond-tax price tax)
  (cond [(< price 100) price]
        [else (sales-tax price tax)]))
(check-expect(cond-tax 20 .05) 20)
(check-expect(cond-tax 100 .05) 105)

;;Exercise 9
;;Stepper. it calculates the unconditional sales tax if and only if 
;;the price is over 100 and it does it after it checks if it is over 100

;;Exercise 10
;;place-circle: posn -> image
;;consumes a posn then creats an red circle with radius 10
;;at the position of the posn on a scene of 300 300
(define (place-circle posn)
  (place-image (circle 10 "solid" "red") (posn-x posn) (posn-y posn)
               (empty-scene 300 300)))

;;Exercise 11
;;mouse-click: world num num me -> world
;;a world is a posn
(define (mouse-click posn x y me)
  (cond[(mouse=? me "button-down") (make-posn x y)]
        [else posn]))

;;Exercise 12
(big-bang (make-posn 0 0)
          (to-draw place-circle)
          (on-mouse mouse-click))
  
  
  