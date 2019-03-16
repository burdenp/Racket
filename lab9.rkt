;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab9) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
;;Exercise 1
;;(Dir) is a Symbol, one of: 'left, 'right, 'up, or 'down.
;;rotate-dir: dir -> dir
;;rotates a dir 90 degress counter-clockwise
(define (rotate-dir dir)
  (cond [(symbol=? 'left dir) 'down]
        [(symbol=? 'up dir) 'left]
        [(symbol=? 'right dir) 'up]
        [(symbol=? 'down dir) 'right]))
(check-expect(rotate-dir 'right) 'up)
(check-expect(rotate-dir 'left) 'down)
;;Exercise 2
;;rotate-dirs: LoD -> LoD
;;aplies rotate-dir to a list of dirs
(define (rotate-dirs Lod)
  (map rotate-dir Lod))
(check-expect(rotate-dirs (list 'left 'right)) (list 'down 'up))
(check-expect(rotate-dirs (list 'up 'down)) (list 'left 'right))
;;Exercise 3
;;move-posn: number number symbol number -> posn
;;creates a posn that is moved in the given direction by the amount given
(define (move-posn x y dir amt)
  (cond [(symbol=? 'left dir) (make-posn (- x amt) y)]
        [(symbol=? 'up dir) (make-posn  x (- y amt))]
        [(symbol=? 'right dir) (make-posn (+ x amt) y)]
        [(symbol=? 'down dir) (make-posn x (+ y amt))]))
(check-expect (move-posn 5 5 'up 5) (make-posn 5 0))
(check-expect (move-posn 5 5 'down 2) (make-posn 5 7))
;;Exercise 4
;; draw-dirs : [Listof Dir] Number Number Color Scene -> Scene
;; Draw lines of given color, following the given directions starting at (x,y)
;; into the given Scene.
(define (random-color num)
  (cond [(even? num) "red"]
        [else "blue"]))
(define (draw-dirs LoD x y color scene)
  (cond [(empty? LoD) scene]
        [else (draw-dirs (rest LoD)
                         (posn-x(move-posn x y (first LoD) 5))
                         (posn-y (move-posn x y (first LoD) 5))
                         (random-color (random 6)) (add-line 
                                scene x y
                                (posn-x(move-posn x y
                                                  (first LoD) 5))
                                (posn-y(move-posn x y
                                                  (first LoD) 5))
                                (random-color (random 6))))]))


;; Screen Size...
(define W 400)
(define H 400)
;; Draw wrapper
(define (draw w)
  (local ((define lst (reverse w)))
    (draw-dirs lst (/ W 2) (/ H 2) "red" (empty-scene W H))))
;; Key Handler
(define (key w ke)
  (cond
    [(key=? ke "up") (cons 'up w)]
    [(key=? ke "down") (cons 'down w)]
    [(key=? ke "left") (cons 'left w)]
    [(key=? ke "right") (cons 'right w)]
    [(key=? ke "r") (rotate-dirs w)]
    [else w]))
(big-bang '()
          (to-draw draw)
          (on-key key))

;;Exercise 5
;; jurassic: [Listof Dir] Number -> [Listof Dir]
;; Compute the next iteration of the Jurassic Fractal, given a [Listof Dir]
;; and the number of iterations left.
(define (jurassic LoD num)
  (cond [(= 0 num) LoD]
        [else (jurassic  (append LoD (reverse(rotate-dirs LoD))) (sub1 num))]))
(check-expect (jurassic (list 'up) 2) (list 'up 'left 'down 'left))

(draw-dirs (jurassic (list 'down) 10) 200 200 "red" (empty-scene 400 400))
