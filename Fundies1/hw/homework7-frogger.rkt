;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname homework7-frogger) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Frogger game
(require 2htdp/image)
(require 2htdp/universe)

(define BOARD-WIDTH 400)
(define BOARD-HEIGHT 1000)
(define BACKGROUND (empty-scene BOARD-WIDTH
                                BOARD-HEIGHT))
(define PLAYER(overlay (triangle 14 "solid" "red")(circle 15 "solid" "lime")))
(define VEHICLE(rectangle 35 30 "solid" "red"))
(define PLANK(rectangle 35 30 "solid" "brown"))
(define TURTLE(rectangle 35 30 "solid" "green"))
;;; Board coordinate system is in
;;; x & y increasing to the right & down direction, respectively.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions
;; A Player is a (make-player Number Number String)
;;where the string is a direction
;;a direction is either up down left right
(define-struct player (x y dir))
;; A Vehicle is a (make-vehicle Number Number String boolean)
;;where the string is a direction
;;a direction is either up down left right
(define player1(make-player (/ BOARD-WIDTH 2) BOARD-HEIGHT "up")) 
(define-struct vehicle (x y dir safe))
(define vehicle1(make-vehicle 100 850 "left" false))
(define vehicle2(make-vehicle 200 850 "left" false))
(define vehicle3(make-vehicle 300 850 "left"false))
(define vehicle4(make-vehicle 400 850 "left" false))
(define vehicle5(make-vehicle 100 750 "right" false))
(define vehicle6(make-vehicle 200 750 "right" false))
(define vehicle7(make-vehicle 300 750 "right" false))
(define vehicle8(make-vehicle 400 750 "right" false))
(define vehicle9(make-vehicle 100 650 "left" false))
(define vehicle10(make-vehicle 200 650 "left" false))
(define vehicle11(make-vehicle 300 650 "left" false))
(define vehicle12(make-vehicle 400 650 "left" false))
(define vehicle13(make-vehicle 100 550 "right" false))
(define vehicle14(make-vehicle 200 550 "right" false))
(define vehicle15(make-vehicle 300 550 "right" false))
(define vehicle16(make-vehicle 400 550 "right" false))
(define vehicle17(make-vehicle 100 950 "right" false))
(define vehicle18(make-vehicle 200 950 "right" false))
(define vehicle19(make-vehicle 300 950 "right" false))
(define vehicle20(make-vehicle 400 950 "right" false))
(define vehicle21(make-vehicle 100 450 "left" true))
(define vehicle22(make-vehicle 200 450 "left" true))
(define vehicle23(make-vehicle 300 450 "left" true))
(define vehicle24(make-vehicle 400 450 "left" true))
(define vehicle25(make-vehicle 100 400 "right" true))
(define vehicle26(make-vehicle 200 400 "right" true))
(define vehicle27(make-vehicle 300 400 "right" true))
(define vehicle28(make-vehicle 400 400 "right" true))
(define vehicle29(make-vehicle 100 350 "left" true))
(define vehicle30(make-vehicle 200 350 "left" true))
(define vehicle31(make-vehicle 300 350 "left" true))
(define vehicle32(make-vehicle 400 350 "left" true))
(define vehicle33(make-vehicle 100 300 "right" true))
(define vehicle34(make-vehicle 200 300 "right" true))
(define vehicle35(make-vehicle 300 300 "right" true))
(define vehicle36(make-vehicle 400 300 "right" true))
(define vehicle37(make-vehicle 100 250 "left" true))
(define vehicle38(make-vehicle 200 250 "left" true))
(define vehicle39(make-vehicle 300 250 "left" true))
(define vehicle40(make-vehicle 400 250 "left" true))

;; A Set of Vehicles (VSet) is one of:
;; - empty
;; - (cons Vehicle VSet)
;; A World is a (make-world Player VSet)
;;The VSet represents the set of vehicles moving across the screen
(define-struct world (player vehicles))
(define world1(make-world player1 
                          (list vehicle1 vehicle2 vehicle3 vehicle4
                                vehicle5 vehicle6 vehicle7 vehicle8
                                vehicle9 vehicle10 vehicle11 vehicle12
                                vehicle13 vehicle14 vehicle15 vehicle16
                                vehicle17 vehicle18 vehicle19 vehicle20
                                vehicle21 vehicle22 vehicle23 vehicle24
                                vehicle25 vehicle26 vehicle27 vehicle28
                                vehicle29 vehicle30 vehicle31 vehicle32
                                vehicle33 vehicle34 vehicle35 vehicle36
                                vehicle37 vehicle38 vehicle39 vehicle40)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rendering functions ;;;;;;;;;;;;;;;;;;;;;
;;draw-vset: vset -> image
;;takes a vset and puts the vset on the background
(define (draw-vset vset)
  (cond [(empty? vset) BACKGROUND]
        [else (cond [(and(>= 450 (vehicle-y(first vset)))
                         (= 0(remainder(vehicle-y(first vset)) 100)))
                     (place-image PLANK(vehicle-x(first vset))
                                  (vehicle-y(first vset)) 
                                  (draw-vset (rest vset)))]
                    [(and(>= 450 (vehicle-y(first vset)))
                         (= 50 (remainder(vehicle-y(first vset)) 100)))
                     (place-image TURTLE(vehicle-x(first vset))
                                  (vehicle-y(first vset)) 
                                  (draw-vset (rest vset)))]
                    [else (place-image VEHICLE
                                       (vehicle-x(first vset))
                                       (vehicle-y(first vset)) 
                                       (draw-vset (rest vset)))])]))

;;world->image: world -> image
;;takes in a world and uses draw-vset on its vset and draws the world
;;with the player on top
(define (world->image w)
  (cond[(string=? (player-dir(world-player w)) "up")
        (place-image PLAYER (player-x (world-player w)) 
               (player-y (world-player w))
               (draw-vset (world-vehicles w)))]
       [(string=? (player-dir(world-player w)) "left")
        (place-image (rotate 90 PLAYER) (player-x (world-player w)) 
               (player-y (world-player w))
               (draw-vset (world-vehicles w)))]
       [(string=? (player-dir(world-player w)) "right")
        (place-image (rotate 270 PLAYER) (player-x (world-player w)) 
               (player-y (world-player w))
               (draw-vset (world-vehicles w)))]
       [(string=? (player-dir(world-player w)) "down")
        (place-image (rotate 180 PLAYER) (player-x (world-player w)) 
               (player-y (world-player w))
               (draw-vset (world-vehicles w)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Collisions and moving;;;;;;;;;;;;;;;;;;;;;
;;onscreen?: vehicle -> boolean
;;sees if the vehicle is on the game screen
(define (onscreen? car)
  (and (<= (vehicle-x car) BOARD-WIDTH) (>= (vehicle-x car) 0)))
;;car-move: vehicle -> vehicle
;;takes a vehicle and moves it in the direction of its dir
(define (car-move car)
  (cond [(and (string=? (vehicle-dir car) "left") (onscreen? car))
         (make-vehicle (- (vehicle-x car) 5)
                       (vehicle-y car)
                       (vehicle-dir car)(vehicle-safe car))]
        [(and (string=? (vehicle-dir car) "right") (onscreen? car))
         (make-vehicle (+ 5 (vehicle-x car))
                       (vehicle-y car) 
                       (vehicle-dir car) (vehicle-safe car))]
        [else (cond
                [(string=? (vehicle-dir car) "right")
                 (make-vehicle 1 (vehicle-y car) 
                               (vehicle-dir car) (vehicle-safe car))]
                [else
                 (make-vehicle BOARD-WIDTH 
                               (vehicle-y car) 
                               "left" (vehicle-safe car))])]))

;;car-list-move: vset -> vset
;;takes in a vset and calls car-move on each car in the vset
(define (car-list-move vset)
  (cond [(empty? vset) empty]
        [else (cons (car-move (first vset)) (car-list-move (rest vset)))]))

;; in-range?: Number, Number, Number -> Boolean
;; is n1 within range of n2?
(define (in-range? n1 n2 range)
  (and (< n1 (+ n2 range))
       (> n1 (- n2 range))))

;; hit?: Player, Vehicle -> Boolean
;; was the player hit by the vehicle?
(define (hit? player vehicle)
  (and (= (player-y player)
          (vehicle-y vehicle))
       (in-range? (player-x player)
                  (vehicle-x vehicle)
                  (+ (/ (image-width PLAYER) 2)
                     (/ (image-width VEHICLE) 2)))))
;;hit-any?: Player, Vehicle -> Boolean
;; was the player hit by any vehicle?
(define (hit-any? player vset)
  (cond [(empty? vset) false]
        [(hit? player (first vset)) true]
        [else (hit-any? player (rest vset))]))
;;river? player -> boolean
;;sees if the player is in the river area
(define (river? player)
  (<= (player-y player) 450))
;;on-log? player vset -> boolean
;;sees if the player is on a log or a turtle in the river
(define (on-log? player vset)
  (hit-any?  player vset))
;;hit-car? player vset -> boolean
;;sees if the player hit a car and thus would lose
(define (hit-car? player vset)
  (and (> (player-y player) 450) (hit-any? player vset)))
;;endgame: world -> boolean
;;helper to hit? it passes a world into hit? and recurses
(define (endgame w)
  (cond[(empty? (world-vehicles w)) false]
       [(and (not (on-log? (world-player w) (world-vehicles w))) (river? (world-player w))) true]
       [(hit-car? (world-player w) (world-vehicles w)) true]
       [(> 200 (player-y (world-player w))) true]
       [else (endgame (make-world
                       (world-player w)
                       (rest (world-vehicles w))))]))
;;helper functions that take a player and then move the player in a direction
(define (player-shift-left player)
  (make-player (- (player-x player) 25) (player-y player) "left"))
(define (player-shift-right player)
  (make-player (+ 25 (player-x player)) (player-y player) "right"))
(define (player-shift-up player)
  (make-player (player-x player) (- (player-y player) 50) "up"))
(define (player-shift-down player)
  (make-player (player-x player) (+ 50 (player-y player)) "down"))

;;; handle-key: world key-event -> world
;;; Handle key-presses in the game.
(define (handle-key w ke)
  (cond [(and (key=? ke "left") (> (player-x(world-player w)) 0))
         (make-world (player-shift-left (world-player w))
                     (world-vehicles w))]
        [(and (key=? ke "right") (< (player-x(world-player w)) BOARD-WIDTH))
         (make-world (player-shift-right (world-player w))
                     (world-vehicles w))]
        [(key=? ke "up")
         (make-world (player-shift-up (world-player w))
                     (world-vehicles w))]
        [(and (key=? ke "down") (< (player-y(world-player w)) BOARD-HEIGHT))
         (make-world (player-shift-down (world-player w))
                     (world-vehicles w))] 
        [else w]))
;;world->world: world-> world
;;Takes in a world and produces the next world
(define (world->world w)
  (make-world (world-player w) (car-list-move (world-vehicles w)))) 

;;last-image: world -> image
;;renders the world with game over on top of it
(define (last-image w)
  (cond[(< (player-y(world-player w)) 150) 
        (place-image (text "You Win" 32 'red)
                     (/ BOARD-WIDTH 2) 
                     (/ BOARD-HEIGHT 2)
                     (empty-scene BOARD-WIDTH BOARD-HEIGHT))]
       [else (place-image (text "You Lose" 32 'black) (/ BOARD-WIDTH 2)
                          (/ BOARD-HEIGHT 2)
                          (empty-scene BOARD-WIDTH BOARD-HEIGHT))]))

(big-bang world1
          (to-draw   world->image)
          (on-tick   world->world .3)
          (on-key    handle-key)
          (stop-when endgame last-image))