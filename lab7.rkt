;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname lab7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; A Nat (natural number) is one of:
  ;  - 0
  ;  - (add1 Nat)
  ;
  ; 0 predicate: zero?
  ;
  ; (add1 n) predicate: positive?
  ; (add1 n) accessor:  sub1

;; Nat template
(define (Nat-temp N)
  (cond [(zero? N) 0]
        [else (...(Nat-temp (sub1 N)) ...)]))

;; nat-even? : N -> boolean
;; returns true if the given Nat is even
(define (nat-even? N)
  (cond [(zero? N) true]
        [(not (positive? N)) false]
        (else (nat-even? (sub1(sub1 N))))))
(check-expect(nat-even? 2) true)
(check-expect(nat-even? 3) false)

;; double : Nat -> Nat
;; doubles the given Nat
(define (double N)
  (cond [(zero? N) 0]
        [else (add1 (add1 (double (sub1 N))))]))
(check-expect(double 2) 4)
(check-expect(double 0) 0)
(check-expect(double 1) 2)

;; down-from : Nat -> list
;; returns the list of Nats counting down from n
(define (down-from N)
  (cond [(zero? N) (list 0)]
        [else (append (list N) (down-from (sub1 N)))]))
(check-expect(down-from 3) (list 3 2 1 0))
(check-expect(down-from 0) (list 0))

;; repeat : Nat string -> list
;; lists the given string n times
(define (repeat N s)
  (cond [(zero? N) empty]
        [else (cons s (repeat (sub1 N) s))]))
(check-expect(repeat 2 "hey") (list "hey" "hey"))
(check-expect(repeat 4 "hey") (list "hey" "hey" "hey" "hey"))

;; nat+: nat nat -> nat
;; returns the two numbers added together
(define (nat+ val1 val2)
  (cond [(zero? val2) val1]
        [else (add1 (nat+ val1 (sub1 val2)))]))
(check-expect(nat+ 2 2) 4)
(check-expect(nat+ 4 2) 6)

;; nat*: nat nat -> nat
;; returns the two numbers multiplied together
(define (nat* val1 val2)
  (cond [(zero? val2) 0]
        [else (nat+ val1 (nat* val1 (sub1 val2)))]))
(check-expect(nat* 2 3) 6)
(check-expect(nat* 0 3) 0)

;; sqware: nat nat -> nat
;; returns the number squared
(define (sqware N)
  (cond [(= 0 N) 0]
        [else (nat+ (sqware (sub1 N)) (sub1 (double N)))]))
         
(check-expect(sqware 5) 25)
(check-expect(sqware 6) 36)

(require 2htdp/image)
(require 2htdp/universe)

(define width 400)
(define height 400)
;; (make-ring number posn)
(define-struct ring (size center))
  
;; LoR is a list of Rings
;; a list of rings is one of
;; -empty
;; cons(ring LoR)

;; grow-ring: ring -> ring
;; makes the ring size one larger with the same center
(define (grow-ring ring)
  (make-ring (add1(ring-size ring)) (ring-center ring)))

;; draw-ring: nat LoR -> image
;; creates the image for the ring and overlays it on the other rings
(define (draw-ring N r)
  (cond [(zero? N) (circle 0 "solid" "black")]
        [else (overlay (draw-ring (sub1 N) r) 
                       (circle N "solid" (cond [(nat-even? N) 'orange]
                                               [else 'black])))]))
;;place-ring: ring scene -> image
;;takes a ring and gives it a position in the scene
(define (place-ring ring scene)
  (place-image (draw-ring (ring-size ring) (ring-size ring)) 
               (posn-x (ring-center ring))
               (posn-y (ring-center ring))
               scene))
;;draw: list of Rings
;;calls place-ring on all the rings in the list and places it on the scene
(define (draw lor)
  (cond [(empty? lor) (empty-scene width height)]
        [else (place-ring (first lor) (draw (rest lor)))]))

;;mouse: LoR number number mouse-event
;; it takes the posn of the mouse click 
;;and creates a ring with that posn as its center
(define (mouse lor x y me)
  (cond [(empty? me) lor]
        [(mouse=? me "button-down") (cons (make-ring 0 (make-posn x y))
                                          lor)]
        [else lor]))

;;tick: LoR -> LoR
;; takes an LoR and adds another LoR onto it through grow-ring
(define (tick lor)
  (cond [(empty? lor) empty]
        [else (cons (grow-ring (first lor))
                    (tick (rest lor)))]))

(big-bang empty
            (on-tick tick .1)
            (on-draw draw)
            (on-mouse mouse))
