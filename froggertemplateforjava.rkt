;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname froggertemplateforjava) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
;;hit?: player vehicle -> boolean
;;sees if the player hit a vehicle
(define(hit? player vehicle)
  (local ((define (in-range? n1 n2 range)
            (and (< n1 (+ n2 range))
                 (> n1 (- n2 range)))))
    (and (= (player-y player)
            (vehicle-y vehicle))
         (in-range? (player-x player)
                    (vehicle-x vehicle)
                    (+ (/ (image-width PLAYER) 2)
                       (/ (image-width VEHICLE) 2))))))
(
;;hit-any?: Player, Vehicle -> Boolean
;; was the player hit by any vehicle
(define (hit-any? player vset)
  (cond [(empty? vset) false]
        [(hit? player (first vset)) true]
        [else (hit-any? player (rest vset))]))


;;river? player -> boolean
;;sees if the player is in the river area
(define (river? player)
  (<= (player-y player) 450))


;;hit-car? player vset -> boolean
;;sees if the player hit a car and thus would lose
(define (hit-car? player vset)
  (and (> (player-y player) 450) (hit-any? player vset)))

;;endgame: world -> boolean
;;helper to hit? it passes a world into hit? and recurses
(define (endgame w)
  (cond[(empty? (world-vehicles w)) false]
       [(and (hit-any? (world-player w) (world-vehicles w))
             (not (hit-car? (world-player w) (world-vehicles w))))  false]
       [(and (not (hit-any? (world-player w) (world-vehicles w)))
             (river? (world-player w))) true]
       [(hit-car? (world-player w) (world-vehicles w)) true]
       [(< (player-y (world-player w)) 200) true]
       [else (endgame (make-world
                       (world-player w)
                       (rest (world-vehicles w))))]))

;;world->world: world-> world
;;Takes in a world and produces the next world
(define (world->world w)
  (make-world (log-move (world-player w) (world-vehicles w))
              (car-list-move (world-vehicles w))))
;; tests
(check-expect (world->world worldTEST2)
              (make-world (world-player worldTEST2)
                          (car-list-move (world-vehicles worldTEST2))))

;;last-image: world -> image
;;renders the world with game over on top of it
(define (last-image w)
  (cond[(< (player-y(world-player w)) 250)
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