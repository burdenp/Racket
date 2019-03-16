;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab8) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
;; A(n) LoN is one of
;; - empty
;; - (cons Number LoN)
;; A(n) LoS is one of
;; - empty
;; - (cons Symbol LoS

;;Exercise 1
;; list : Number ... Number -> LoN
;; list : Symbol ... Symbol -> LoS

;;cons: X list -> list
;;a cons is either cons(first rest)
;; where first is any thing and rest is a list

;;append: list list -> list
;;combines two lists

;;length: list -> number
;;returns the number of elements in a list

;;Exercise 2
;;list of X: X....X -> LoX
;;creates a list of X

;;Exercise 3
;;;;cons: X list -> list
;;a cons is either cons(first rest)
;; where first is any thing and rest is a list

;;append: list list -> list
;;combines two lists

;;length: list -> number
;;returns the number of elements in a list

;;Exercise 4 5 6 7
;;is-in?: number LoN -> boolean
;;checks to see if the number is in the list of numbers
(define (is-in? number list)
  (local ((define (check-number? x)
            (= number x)))
    (ormap check-number? list)))
(check-expect(is-in? 0 (list 0 1 2 3)) true)
(check-expect(is-in? 1 (list 0 1 2 3)) true)
(check-expect(is-in? 5 (list 0 1 2 3)) false)



;;Exercise 8
;;remove-symbols?: [Lo any] -> [list with out symbols]
;;removes the symbols from the list
(define (remove-symbols? l)
  (local ((define (helper x)
            (not(symbol? x))))
    (filter helper l)))
(check-expect(remove-symbols? (list 'a 'b 'c "a" "b")) (list "a" "b"))
(check-expect(remove-symbols? (list  "a" "b")) (list "a" "b"))
(check-expect(remove-symbols? (list 'a 'b 'c )) empty)

;;Exercise 9
;;remove-numbers?: : [Lo any] -> [list with out numbers]
;;removes the numbers from the list
(define (remove-numbers? l)
  (local ((define (helper x)
            (not(number? x))))
    (filter helper l)))
(check-expect(remove-numbers? (list 1 2 3 "a" "b")) (list "a" "b"))
(check-expect(remove-numbers? (list  "a" "b")) (list "a" "b"))
(check-expect(remove-numbers? (list 1 2 3 )) empty)

;;Exercise 10
;;filter-out:  [Lo any] [any -> boolean] -> [list of any]
;;removes a type of element from the list
(define (filter-out l op)
  (local ((define (helper x)
            (not (op x))))
    (filter helper l)))
(check-expect(filter-out (list 1 2 3 "a" "b") number?) (list "a" "b"))
(check-expect(filter-out (list  "a" "b") number?) (list "a" "b"))
(check-expect(filter-out (list 1 2 3 ) number?) empty)

;;Exercise 11
;;all-odd?: LoN -> boolean
;;returns true if the whole list is odd
(define (all-odd? LoN)
  (andmap odd? LoN))
(check-expect(all-odd? (list 1 2 3)) false)
(check-expect(all-odd? (list 3)) true)

;;Exercise 12
;;all-odd2?: LoN -> boolean
;;returns true if the whole list is odd
(define (all-odd2? LoN)
  (not (ormap even? LoN)))
(check-expect(all-odd2? (list 1 2 3)) false)
(check-expect(all-odd2? (list 3)) true)

;;Exercise 13
;;range: number number -> list of numbers
;;returns a list with the range between the 2 numbers
(define (range n m)
  (build-list (abs (- n m)) (lambda (num) (+ n num))))
(check-expect (range 0 2) (list 0 1))
(check-expect (range 0 3) (list 0 1 2))
(check-expect (range 1 3) (list 1 2))

;;Exercise 14
;;evens: number number -> LoN
;;returns the range except only the even numbers
(define (evens n m)
  (filter even? (build-list (abs (- n m)) (lambda (num) (+ n num)))))
(check-expect (evens 0 2) (list 0))
(check-expect (evens 0 3) (list 0 2))
(check-expect (evens 1 3) (list 2))

;;Exercise 15
;;sum: LoN -> number
;;sums all the numbers in a list
(define (sum l)
  (foldr + 0 l))
(check-expect (sum (list 1 2 3)) 6)
(check-expect (sum (list 2 3)) 5)
(check-expect (sum (list 1 2 3 4)) 10)

;;Exercise 16
;; minus-all : Number [listof Number] -> Number
;; Subtract all the numbers in the list from the given one
(define (minus-all n lon)
  (local ((define (temp x y)
            (+ y (- 0 x))))
    (foldr temp 20 lon)))
(check-expect (minus-all 20 empty) 20)
(check-expect (minus-all 20 (list 5 2)) 13)
(check-expect (minus-all 20 (list 5 4 3 2 1)) 5)

(require 2htdp/universe)
(require 2htdp/image)
;; Scene Width and Height...
(define WIDTH 400)
(define HEIGHT 400)
;; A Planet is:;; (make-planet Number Number Number Number String)
(define-struct planet (x y vx vy color))
;; x,y represent the planet's current location, and vx,vy represent
;; its velocity (speed/direction)
;; Number of colors
(define NUM-COLORS 8)
;; color-for : Number -> String
;; Return a color for the given number
(define (color-for n)
  (cond [(= n 0) "red"]
        [(= n 1) "green"]
        [(= n 2) "pink"]
        [(= n 3) "grey"]
        [(= n 4) "orange"]
        [(= n 5) "vermillion"]
        [(= n 6) "yellow"]
        [(= n 7) "purple"]
        [else "blue"]))         

;;Exercise 17
;;move-all: LoP -> LoP
;;moves all the planets in the list of planets
(define (move-all LoP)
  (local ((define (single planet)
            (make-planet (+ (planet-x planet) (planet-vx planet))
                         (+ (planet-y planet) (planet-vy planet))
                         (planet-vx planet) (planet-vy planet)
                         (planet-color planet))))
    (map single LoP)))
(define planet1 (make-planet 1 1 1 1 "green"))
(define planet2 (make-planet 2 2 1 1 "green"))
(check-expect(move-all (list planet1 planet1 planet1))
             (list planet2 planet2 planet2))

;;Exercise 18
;;draw-lop: LoP-> image
;;draws the list of planets on the empty scene
(define (draw-lop LoP)
  (local ((define (draw planet x)
            (place-image (circle (random 20) "solid" (planet-color planet))
                         (planet-x planet) (planet-y planet)
                         x)))
    (foldr draw (empty-scene 1985 1000) LoP)))
(draw-lop (list planet1 planet2))

;; distance : Planet Planet -> Number
;; Calculate the distance between the Planets
(define (distance p1 p2)
  (sqrt (+ (sqr (- (planet-x p1) (planet-x p2)))
           (sqr (- (planet-y p1) (planet-y p2))))))
;; apply-gravity : Planet Planet -> Planet
;; Apply the gravitational effects of the other Planet to the
;; second Planet (note the order of the arguments...)
(define (apply-gravity p-other p)
  (local [(define dist (distance p p-other))
          (define dx(-(planet-x p)
                      (planet-x p-other)))
          (define dy(-(planet-y p)
                      (planet-y p-other)))]
    (cond [(< dist 1) p]
          [else (make-planet (planet-x p) (planet-y p)
                             (- (planet-vx p) (/ dx dist))
                             (- (planet-vy p) (/ dy dist))
                             (planet-color p))])))

;;Exercise 19
;;gravity-one: Planet LoP -> Planet
;;applies gravity to one planet
(define (gravity-one planet LoP)
  (foldr apply-gravity planet LoP))

;;Exercise 20
;;gravity-all: LoP -> LoP
;;applies gravity to all the planets
(define (gravity-all LoP)
  (local ((define (helper x)
            (gravity-one x (rest LoP))))
    (map helper LoP)))
;; tick : [Listof Planet] -> [Listof Planet]
;; Apply gravity, then move all the Planets
(define (tick lop)
  (move-all (gravity-all lop)))
;; mouse : [Listof Planet] Number Number String -> [Listof Planet]
;; Add a new planet where the mouse was clicked
(define (mouse lop x y me)
  (cond [(string=? me "button-down")
         (cons (make-planet x y 0 0 (color-for (random NUM-COLORS)))
               lop)]
        [else lop]))
;; Start with no planets...
(define last (big-bang empty
                       (on-mouse mouse)
                       (to-draw draw-lop)
                       (on-tick tick 1/20)))
