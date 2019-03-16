;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |lab 5|) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
(define-struct empty-lon())
(define-struct cons-lon (first rest))
#|
(define (theySeeMeRollinTemplate HRLoN)
  (cond
    [(empty-lon? HRLoN)...]
    [else ...(first HRLoN)...(rest HRLoN)...]))
|#

;;a LoS is one of
;;-empty
;;(cons string LoS)

;;a LoI is one of
;;-empty
;;(cons images LoI)

;;a LoLoN is one of
;; -empty
;;(cons LoN LoLoN)

(define (sum LoN)
  (cond
    [(empty? LoN) 0]
    [else (+ (first LoN) (sum (rest LoN)))]))
(check-expect
     (sum (cons 2 (cons 3 (cons 4 empty))))
     9)
(define (product LoN)
  (cond
    [(empty? LoN) 1]
    [else (* (first LoN) (product (rest LoN)))]))
(check-expect
     (product (cons 2 (cons 3 (cons 4 empty))))
     24)
(define (join-los LoS)
  (cond
    [(empty? LoS) ""]
    [else (string-append (first LoS) (join-los (rest LoS)))]))
(check-expect
     (join-los (cons "An" (cons "Lo" (cons "S" empty))))
     "AnLoS")

;;a LoB is one of
;; empty
;; or (cons ball LoB
(define-struct ball (x y vx vy))
(define WIDTH 400)
(define HEIGHT 400)
(define GRAV-ACC -3)
(define SIZE 10)
#|
(define (BallsUpInHere ball)
  (...(ball-x ball)...(ball-y ball)...(ball-vx ball)...(ball-vy ball)...))
(define (LoBUpInHere LoB)
  (cond
    [(empty? LoB)...]
    [else ...(ball-x (first LoB))...(ball-y (first LoB))...(ball-vx (first LoB))...
             (ball-vy (first LoB))...(LoBUpInHere (rest LoB))]))
|#

(define (off-screen? ball)
    (or (< (ball-x ball) 0) (> (ball-x ball) WIDTH) (< (ball-y ball) 0) (> (ball-y ball) HEIGHT)))

(define (gravity ball)
  (make-ball (ball-x ball) (ball-y ball) (ball-vx ball) (* (ball-vy ball) GRAV-ACC))) 
(define (move ball)
  (make-ball (+(ball-x ball)(ball-vx ball)) (+(ball-y ball) (ball-vy ball)) (ball-vx ball) (ball-vy ball))) 
(define (draw-lob LoB)
  (cond
    [(empty? LoB) (empty-scene WIDTH HEIGHT)]
    [else (place-image (circle SIZE "solid" "pink") (ball-x (first LoB)) (ball-y (first LoB))
                       (draw-lob (rest LoB)))]))

;(draw-lob (cons ball1(cons ball2(cons ball3(cons ball4(cons ball5 empty))))))

;; Exercise 12
(define (on-screen LoB)
  (cond
    [(empty? LoB) empty]
    [else (cond [(off-screen? (first LoB)) (on-screen (rest LoB))]
                [else
                 (cons (first LoB) (rest LoB))])]))
;(on-screen (cons ball1(cons ball2(cons ball3(cons ball4(cons ball5 empty))))))

;; Exercise 13
(define (gravity-all LoB)
  (cond [(empty? LoB) empty]
        [else (cons (gravity (first LoB)) (gravity-all (rest LoB)))]))

;; Exercise 14
(define (move-all LoB)
  (cond [(empty? LoB) empty]
        [else (cons (move (first LoB)) (move-all (rest LoB)))]))

 ;; mouse : LoB Number Number MouseEvent -> LOP
      ;; Add a new random ball
      (define (mouse LoB x y me)
        (cond [(string=? me "drag")
               (cons (make-ball x y
                                (- (random 9) 4)
                                (- (+ (random 10) 10)))
                     LoB)]
              [else LoB]))

      ;; tick : LoB -> LoB
      ;; Move, gravitize, then filter out all 
      ;;   the off-screen Balls
      (define (tick LoB)
        (on-screen (move-all (gravity-all LoB))))

      (define last (big-bang empty
                             (on-mouse mouse)
                             (to-draw draw-lob)
                             (on-tick tick 1)))

  











