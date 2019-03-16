;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |lab 4|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
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

(define (band-temp b)
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

(define (album-temp a)
  (cond [(studio? a) ... (studio-name a) ...
                     ... (studio-yop a)]
        [(live? a) ... (live-name a) ...
                   ... (live-yor a) ...
                   ... (live-yop a)]))

;; EXERCISE 5

;; unpublished-album is one of:
;; -complete
;; -incomplete

(define-struct complete (sn name release))
;; (make-complete Number String Number)

(define-struct incomplte (sn))
;; (make-incomplete Number)

;; Examples:
(make-complete (5848 "Funk" 2012))
(make-incomplete (0000))

(define (ua-temp a)
  (cond [(complete? a) ... (complete-sn a) ...
                       ... (complete-name a) ...
                       ... (complete-release a)]
        [(incomplete? a) ... (incomplete-sn a)]))

;; EXERCISE 6

(define (checker a sn)
  (cond [(or (= sn (incomplete-sn a)) (= sn (complete-sn a))) true]
        [else false]))

;; EXERCISE 7

(define (complete a date name)
  (cond [(complete? a) a]
        [(incomplete? a) (make-complete (incomplete-sn a) name date)]))

