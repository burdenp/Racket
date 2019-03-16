;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname fundies1lab6) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
(require 2htdp/image)
;;Exercise 1
;;;; A Time is (make-time Number Number)
(define-struct Time (hours minutes))
;; Time -> Image
;; Produce an image of the given time, as it appears on a digital clock.
(define(time->text a-time)
  (text (append-time a-time) 30'red))
;;append-time: Time -> string
;;Helper for time-> Image
(define (append-time a-time)
  (string-append
   (number->string
    (Time-hours a-time))":"
                        (cond[
                              (<(Time-minutes a-time)10)"0"]
                             [else ""])
                        (number->string(Time-minutes a-time))))
(check-expect(time->text (make-Time 1 1)) (text "1:01" 30 'red))
(check-expect(time->text (make-Time 2 1)) (text "2:01" 30 'red))
(check-expect(time->text (make-Time 3 1)) (text "3:01" 30 'red))
;; LOS -> Number
;; Determine how many symbols are in a-los
(define (count a-los)
  (cond
    [(empty? a-los) 0]
    [(cons? a-los)
     (+ 1 (count (rest a-los)))]))

;;Exercise 2
;;string-of: number string-> string
;;takes a number and a string and repeats the string that many times with spaces
(define (string-of num string)
  (string-append (replicate (- num 1) (string-append string " ")) string))
(check-expect(string-of 2 "no") "no no")
(check-expect(string-of 2 "hey") "hey hey")

;;Exercise 3
;;reducing: number string -> list of strings
;;returns a list of srings that are the string repeated
;;num times gettting smaller each time
(define (reducing num string)
  (cond [(= 0 num) empty]
        [else (cons (string-of num string) (reducing (- num 1) string))]))
(check-expect(reducing 3 "a") (list "a a a" "a a" "a"))
(check-expect(reducing 2 "b") (list "b b" "b"))
(check-expect(reducing 1 "c") (list  "c"))

;;Exercise 4
;;lookup: LoS num -> symbol
;;returns the nth element of the list
(define (lookup LoS num)
  (cond[(empty? LoS) "List is too short"]
       [(= num 0) (first LoS)]
       [else (lookup (rest LoS) (- num 1))]))
(check-expect(lookup (list 'a 'b 'c 'd) 0) 'a)
(check-expect(lookup (list 'a 'b 'c 'd) 2) 'c)

;;Exercise 5
;;replace: LoS symbol number -> LoS
;;replaces the nth element in the LoS with the symbol
(define (replace LoS sym num)
  (cond [(empty? LoS) "List is too short"]
        [(= num 0) (cons sym (rest LoS))]
        [else (cons (first LoS) (replace (rest LoS) sym (- num 1)))]))
(check-expect(replace (list 'a 'b 'c 'd) 'new 2)(list 'a 'b 'new 'd))
(check-expect(replace (list 'a 'b 'c 'd) 'yay 0)(list 'yay 'b 'c 'd))
(check-expect(replace (list 'a 'b 'c 'd) 'end 3)(list 'a 'b 'c 'end))

;;Exercise 6
;;all-comb: LoS LoS -> LoS
;;makes a list of strings with all possible combinations of the elements in the two lists
(define (all-comb LoS1 LoS2)
  (cond[(empty? LoS1) empty]
       [else (append (all-comb-help (first LoS1) LoS2) (all-comb (rest LoS1) LoS2))]))
;;all-comb-help: String LoS -> list
;;makes strings with string appended onto each element in the LoS
(define (all-comb-help string LoS)
  (cond [(empty? LoS) empty]
        [else (cons(string-append string (first LoS)) (all-comb-help string (rest LoS)))]))
(check-expect(all-comb (list "Student: " "Faculty: ")
                       (list "Mr." "Ms." "Mrs."))
             (list "Student: Mr." "Student: Ms." "Student: Mrs."
                   "Faculty: Mr." "Faculty: Ms." "Faculty: Mrs."))
(check-expect(all-comb-help "A" (list "B" "C" "D"))(list "AB" "AC" "AD"))

;;Exercise 7

;; a Dog is (define-struct dog (name age breed)) where name is a
;; string, age is a number and breed is a symbol
(define-struct dog (name age breed))
;; dogs-older-than : list-of-Dogs number -> list-of-Dogs
;; to list all the dogs older than age
(define (dogs-older-than dogs age)
  (cond [(empty? dogs) empty]
        [(> (dog-age (first dogs)) age)
         (cons (first dogs)
               (dogs-older-than (rest dogs) age))]
        [else (dogs-older-than (rest dogs) age)]))
(check-expect(dogs-older-than (list (make-dog "h" 5 'he) (make-dog "f" 7 'ne)) 6) (list (make-dog "f" 7 'ne)))

;;Exercise 8
;; numbers-between : num num -> LoN
;; to list all the whole numbers between the low number and the high number
(define (numbers-between low high)
  (cond [(> low high) empty]
        [else (cons low
                    (numbers-between (add1 low)
                                     high))]))
(check-expect(numbers-between 2 4) (list 2 3 4))

;;Exercise 9
;; pairify : list-of-any -> list-of-list
;; to group the elements of the input list into a list of two-element lists
;;must be even
(define (pairify a-list)
  (cond [(or (empty? a-list)
             (empty? (rest a-list)))
         empty]
        [else (cons (list (first a-list)
                          (first (rest a-list)))
                    (pairify (rest (rest a-list))))]))