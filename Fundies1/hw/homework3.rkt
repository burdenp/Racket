;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname homework3) (read-case-sensitive #t) (teachpacks ((lib "universe.ss" "teachpack" "2htdp") (lib "image.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.ss" "teachpack" "2htdp") (lib "image.ss" "teachpack" "2htdp")))))
;;Exercise 6.4.2
;;(make-time num num num)
(define-struct time(hour minute second))
;;Exercise 6.4.3
;;(make-word letter letter letter)
;;where a letter is a symbol that is between 'a and 'z
(define-struct word(letter1 letter2 letter3))
;;Exercise 6.5.2
;;time->seconds: time-> number
;;consumes a time structure and returns the number of seconds since midnight
;;example: (time->seconds(make-time 12 30 2)) expects 45002
;; so it is 60 seconds in every minute and 60 minutes in every hour
(define (time->seconds time)
  (+ (* 3600 (time-hour time)) 
     (* 60 (time-minute time)) 
     (time-second time)))
(check-expect(time->seconds(make-time 12 30 2)) 45002)

;;Part 2
(define-struct Tlight (radius color)) 
;; Tlight = (make-tlight Number String) 
;; Constraints:  
;; The string is either “red” “yellow” or “green

;;change: Tlight-> Tlight
;;it changes the Tlight from red to green to yellow to red again
(define (change Tlight)
  (cond[(string=? "red" (Tlight-color Tlight))
        (make-Tlight (Tlight-radius Tlight) "green")]
       [(string=? "green" (Tlight-color Tlight))
        (make-Tlight (Tlight-radius Tlight) "yellow")]
       [else (make-Tlight (Tlight-radius Tlight) "red")]))
(check-expect(change(make-Tlight 2 "red")) (make-Tlight 2 "green"))
(check-expect(change(make-Tlight 2 "yellow")) (make-Tlight 2 "red"))

;;draw-light: Tlight-> image
;;takes in a Tlight and draws a circle
(define (draw-light Tlight)
  (circle (Tlight-radius Tlight) "solid" (Tlight-color Tlight)))

;;Part 3
;;(make-ball  posn direction)
;;a direction is either the string "up" "left" "down" or "right"
(define-struct ball (position direction))

(define a-ball (circle 5 "solid" "red"))
(define (ball-image b)
  (place-image a-ball
               (posn-x (ball-position b)) 
(posn-y (ball-position b))
               (empty-scene 300 300)))

;;ball-next: ball-> ball
;;consumes a ball and returns the next ball
(define (ball-next b)
  (cond[(string=? (ball-direction b) "up") 
        (make-ball (make-posn (posn-x (ball-position b)) 
                              (- (posn-y (ball-position b)) 10)) 
                   (ball-direction b))]
       [(string=? (ball-direction b) "down") 
        (make-ball (make-posn (posn-x (ball-position b)) 
                              (+ (posn-y (ball-position b)) 10))
                   (ball-direction b))]
       [(string=? (ball-direction b) "left") 
        (make-ball (make-posn (- (posn-x (ball-position b)) 10) 
                              (posn-y (ball-position b)))
                   (ball-direction b))]
       [(string=? (ball-direction b) "right") 
        (make-ball (make-posn (+ (posn-x (ball-position b)) 10) 
                              (posn-y (ball-position b)))
                   (ball-direction b))]))

(define (ball-change b ke)
  (cond [(key=? ke "left")
         (make-ball (ball-position b) "left")]    
        [(key=? ke "right")
         (make-ball (ball-position b) "right")]
        [(key=? ke "up")
         (make-ball (ball-position b) "up")]
        [(key=? ke "down")
         (make-ball (ball-position b) "down")]
        [else b]))





