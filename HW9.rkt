#lang pl 09


;;tail call optimization makes all of this code work because
;;you call one function that returns more functions and thus
;;slowly work through the problem that needs to be solved
;;such as how main returns either done or step
;;and step always returns main


;; we represent labels (goto targets) as thunks, and registers (or
;; memory locations in general) as integer boxes.
(define-type Label    = (-> Integer))
(define-type Register = (Boxof Integer))


;; "X = Y"
;; assigns the contents of register Y to register X
(: mov : Register Register -> Void)
(define (mov X Y) (set-box! X (unbox Y)))

;; "X = N"
;; assigns the constant N (an "immediate" value)  to register X
(: movi : Register Integer -> Void)
(define (movi X N) (set-box! X N))

;; "X += Y"
;; increments register X by register Y
(: add : Register Register -> Void)
(define (add X Y) (set-box! X (+ (unbox X) (unbox Y))))

;; "X += N"
;; increments register X by a constant N
(: addi : Register Integer -> Void)
(define (addi X N) (set-box! X (+ (unbox X) N)))

;; "X -= Y"
;; deccrements register X by register Y
(: sub : Register Register -> Void)
(define (sub X Y) (set-box! X (- (unbox X) (unbox Y))))

;; "X -= N"
;; decrements register X by a constant N
(: subi : Register Integer -> Void)
(define (subi X N) (set-box! X (- (unbox X) N)))

;; "X &= Y"
;; sets X to the bitwise "and" of X and Y
(: and : Register Register -> Void)
(define (and X Y) (set-box! X (bitwise-and (unbox X) (unbox Y))))

;; "X &= N"
;; sets X to the bitwise "and" of X and a constant N
;;(: andi : Register Integer -> Void)
;;(define (andi X N) (set-box! X (bitwise-and (unbox X) N)))

;; "X >>= N"
;; shifts register X right by N bits
;;(: shri : Register Integer -> Void)
;;(define (shri X N) (set-box! X (arithmetic-shift (unbox X) (- N))))

;; "goto L"
;; (goto L) jumps to the label -- labels are represented as nullary
;; functions (also called "thunks")
(: goto : Label -> Integer)
(define (goto L) (L))

;; "ret X"
;; return the contents of register X
(: ret : Register -> Integer)
(define (ret X) (unbox X))

;; "ret N"
;; return the constant N
(: reti : Integer -> Integer)
(define (reti N) N)

;; "if X=0 goto L1 else goto L2"
;; if register X is zero, jump to L1, else jump to L2
(: if0 : Register Label Label -> Integer)
(define (if0 a l1 l2) (if (zero? (unbox a)) (goto l1) (goto l2)))

;; "if X>0 goto L1 else goto L2"
;; if register X is positive, jump to L1, else jump to L2
;;(: ifp : Register Label Label -> Integer)
;;(define (ifp a l1 l2) (if (positive? (unbox a)) (goto l1) (goto l2)))

(: fib : Integer -> Integer)
;; compute the nth fibonacci number using the assembly language
(define (fib n)
  (: A : Register) (define A (box 0))
  (: B : Register) (define B (box 1))
  (: C : Register) (define C (box 0))
  (: N : Register) (define N (box n))
  ;;
  (: main : Label)
  (: step : Label)
  (: done : Label)
  ;;
  (define (main) (if0  N done step))
  (define (step) (mov  C A)
                 (add  C B)
                 (mov  A B)
                 (mov  B C)
                 (subi N 1)
                 (goto main))
  (define (done) (ret  A))
  ;;
  (main))
;; test
(test (map fib '(0 1 2 3 4 5 6 7 8 9 10))
      => '(0 1 1 2 3 5 8 13 21 34 55))

(: more-ones? : Integer Integer -> Integer)
;; returns 1 if `a' has more 1s in its binary representation than `b'
(define (more-ones? a b)
  (: A : Register) (define A (box a))
  (: B : Register) (define B (box b))
  (: C : Register) (define C (box 0))
  (: switch : Register) (define switch (box 0))
  (: temp : Register)  (define temp (box 0))
  (: numA : Register) (define numA (box 0))
  (: numB : Register) (define numB (box 0))
  (: numC : Register) (define numC (box 0))
  (: temp2 : Register) (define temp2 (box 0))
 
  ;;
  (: main : Label)
  (: bitC : Label)
  (: setup : Label)
  (: setup2 : Label)
  (: setup3 : Label)
  (: setup-check : Label)
  (: checkA : Label)
  (: checkB : Label)
  (: re0 : Label)
  (: re1 : Label)
  (: decrement : Label)
  ;;
  (define (main) (mov C A)
                 (goto setup))
  (define (setup)(if0 C setup-check bitC))
  (define (bitC) (addi numC 1)
                 (mov temp C)
                 (movi temp2 1)
                 (sub temp temp2)
                 (and C temp)
                 (goto setup))
  (define (setup2) (addi switch 1)
                   (mov C B)
                   (mov numA numC)
                   (movi numC 0)
                   (if0 C setup3 bitC))
  (define (setup3) (mov numB numC)
                   (goto checkA))
  
  (define (checkA) (if0 numA re0 checkB))
  (define (checkB) (if0 numB re1 decrement))
  (define (decrement) (subi numA 1)
                      (subi numB 1)
                      (goto checkA))
  (define (setup-check)(if0 switch setup2 setup3))
  (define (re0) (reti 0))
  (define (re1) (reti 1))
  
    
  ;;
  (main))
;; tests
(test (more-ones? 0 0) => 0)
(test (more-ones? 1 0) => 1)
(test (more-ones? 1 2) => 0)
(test (more-ones? 2 0) => 1)
(test (more-ones? 0 1) => 0)
(test (more-ones? 0 2) => 0)
(test (more-ones? 2 1) => 0)
(test (more-ones? 2 2) => 0)
(test (more-ones? 3 1) => 1)
(test (more-ones? 7 0) => 1)
(test (more-ones? 3 7) => 0)


(define minutes-spent 120)