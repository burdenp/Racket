;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname fundies1lab4) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
;; EXERCISE 2
(define (compute a b)
  (/ (sqr (+ a b)) (sqr (- a b))))
(check-expect (compute 2 3) 25)
(check-expect (compute 5 4) 81)
#| 
Stepper: 
(compute 2 3)

(/ (sqr (+ 2 3)) (sqr (- 2 3)))

(/ (sqr 5) (sqr (- 2 3)))

(/ 25 (sqr (- 2 3)))

(/ 25 (sqr -1))

(/ 25 1)

25
|#


;; EXERCISE 3

;; a band is one of:
;; -rockband
;; -jazzband
;; -popband

(define-struct rockband (name singer guitarist bassist drummer))
;; a rockband is a struct:
;; (define-struct rockband (String String String String String))
;; (make-rockband name singer guitarist bassist drummer)

(define-struct jazzband (name trumpeter bassist drummer))
;; a jazzband is a struct:
;; (define-struct jazzband (String String String String))
;; (make-jazzband name trumpeter bassist drummer)

(define-struct popband (name singer synth1 synth2))
;; a popband is a struct:
;; (define-struct popband (String String String String))
;; (make-popband name singer synth1 synth2)

;; Eamples:
(make-rockband "Good Charlotte" "Joel" "Jack" "Dan" "Nathan")
(make-jazzband "Jazzer" "Carlos" "Deema" "Marina")
(make-popband "Backstreet Boys" "Josh" "Jimmy" "Taylor")

#|(define (band-temp b)
  (cond [(rockband? b) ... (rockband-name b)... 
                       ... (rockband-singer b)...
                       ... (rockband-guitarist b)...
                       ... (rockband-bassist b)...
                       ... (rockband-drummer b)]
        [(jazzband? b) ... (jazzband-name b) ... 
                       ... (jazzband-trumpeter b)... 
                       ... (jazzband-bassist b) ... 
                       ... (jazzband-drummer b)]
        [(popband? b) ... (popband-name b)... 
                      ... (popband-synth1 b) ...
                      ... (popband-synth2 b)]))
|#
;; EXERCISE 4

;; a music-album is one of: 
;; -live 
;; -studio

(define-struct studio (name yop))
;; (make-studio String Number) 

(define-struct live (name yor yop))
;; (make-live String Number Number)

;; Examples: 
(make-studio "Cool" 1992)
(make-live "Sick Shizzz" 2005 2006)
#|
(define (album-temp a)
  (cond [(studio? a) ... (studio-name a) ...
                     ... (studio-yop a)]
        [(live? a) ... (live-name a) ...
                   ... (live-yor a) ...
                   ... (live-yop a)]))
|#
;; EXERCISE 5

;; unpublished-album is one of:
;; -complete
;; -incomplete

(define-struct complete (sn name release))
;; (make-complete Number String Number)

(define-struct incomplete (sn))
;; (make-incomplete Number)

;; Examples:
(make-complete 5848 "Funk" 2012)
(make-incomplete 0000)
#|
(define (ua-temp a)
  (cond [(complete? a) ... (complete-sn a) ...
                       ... (complete-name a) ...
                       ... (complete-release a)]
        [(incomplete? a) ... (incomplete-sn a)]))
|#
;; EXERCISE 6

(define (checker a sn)
  (cond [(or (= sn (incomplete-sn a)) (= sn (complete-sn a))) true]
        [else false]))

;; EXERCISE 7

(define (finished a date name)
  (cond [(complete? a) a]
        [(incomplete? a) (make-complete (incomplete-sn a) name date)]))


;;Exercise 8
(define-struct world (posn1 posn2))

(define (mouse w x y me)
  (cond [(empty? me) w]
        [(mouse=? me "button-down") (make-world (world-posn1 w) (make-posn x y))]
        [else w]))

;;Exercise 9
(define (tick-tock w)
  (make-world (make-posn (mover(posn-x (world-posn1 w))(posn-x (world-posn2 w)))
                         (mover(posn-y (world-posn1 w))(posn-y (world-posn2 w))))
              (world-posn2 w)))

(define (mover n1 n2)
  (cond [(< n1 n2) (+ 1 n1)]
        [(> n1 n2) (- n1 1)]
        [else n1]))

;;Exercise 10
(define (world-draw world)
  (place-image (circle 10 "solid" "red") (posn-x (world-posn1 world)) (posn-y (world-posn1 world)) 
               (place-image (circle 15 "solid" "blue") (posn-x (world-posn2 world)) (posn-y (world-posn2 world))
               (empty-scene 300 300))))

;;Exercise 11
(big-bang (make-world (make-posn 150 150) (make-posn 0 0))
            (on-tick tick-tock .1)
            (on-draw world-draw)
            (on-mouse mouse))
