;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname fundies2lab1) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
;; a list of a few inexact numbers
(define NUM-LIST
  (list #i+9e23
        #i+8e23
        #i-1e23
        #i+6e23))

(define (sum-right alist)
  (foldr + 0 alist))

(define (sum-left alist)
  (foldl + 0 alist))

;; one way to add all numbers
(sum-right NUM-LIST)

;; another way to add all numbers
(sum-left NUM-LIST)
;; adding the large numbers first
(+ (+ #i+9e23  #i+8e23) #i+6e23)

;; then subtracting the small one => correct result
(+ (+ (+ #i+9e23  #i+8e23) #i+6e23) #i-1e23)

;; one way to add all numbers
(sum-right (list 1 2 3 4 5 6))

;; another way to add all numbers
(sum-left (list 1 2 3 4 5 6))

(define (sum-recipe LoN)
  (cond [(empty? LoN) 0]
        [else (+ (first LoN) (sum-recipe (rest LoN)))]))
(check-expect(sum-recipe (list 1 2 3)) 6)
(define (sum-accumlator LoN)
  (local[(define (accum LoN n)
           (cond [(empty? LoN) n]
                 [else (accum (rest LoN) (+ n (first LoN)))]))]
  (accum LoN 0)))
(check-expect(sum-accumlator (list 1 2 3)) 6)


