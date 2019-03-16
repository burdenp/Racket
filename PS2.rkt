#lang pl 02
#| BNF for the LE
  <LE> :: = <list>
          | { null }
          | { (cons <symbol> <LE> }
          | { (cons <num> <LE>}
|#

;; ---< The AE interpreter >---
;;changed to infix notation
#| BNF for the AE language:
   <AE> ::= <num>
          | { <AE> + <AE> }
          | { <AE> - <AE> }
          | { <AE> * <AE> }
          | { <AE> / <AE> }
|#

;; AE abstract syntax trees
(define-type AE
  [Num Number]
  [Add AE AE]
  [Sub AE AE]
  [Mul AE AE]
  [Div AE AE])

(: parse-sexpr : Sexpr -> AE)
;; to convert s-expressions into AEs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n) (Num n)]
    [(list lhs '+ rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list lhs '- rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list lhs '* rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list lhs '/ rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> AE)
;; parses a string containing an AE expression to an AE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

(: eval : AE -> Number)
;; consumes an AE and computes the corresponding number
(define (eval expr)
  (cases expr
    [(Num n)   n]
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
    [(Mul l r) (* (eval l) (eval r))]
    [(Div l r) (cond [(= 0 (eval r)) 999]
                 [else (/ (eval l) (eval r))])]))

(: run : String -> Number)
;; evaluate an AE program contained in a string
(define (run str)
  (eval (parse str)))

;; tests
(test (run "3") => 3)
(test (run "{3 + 4}") => 7)
(test (run "{{3 - 4} + 7}") => 6)
(test (run "{3 * 4}") => 12)
(test (run "{4 / 2}") => 2)
(test (run "{2 / 0}") => 999)
(test (run "{incorrect input}") =error> "bad syntax in (incorrect input)")
#|
<MAE> ::= <num>
        | { + <MAE> <MAE> }
        | { - <MAE> <MAE> }
        | { * <MAE> <MAE> }
        | { / <MAE> <MAE> }
        | { set <MAE> }
        | get
|#
#| {* {+ {set 1} {set 2}} get}
   The problem is that two separate values were set and only one of them
   was gotten
|#
#|
  <SAE> ::= <AE>
         | <MAE>
|#
#|
;;2
;;Num -> num
;;square the number
(define (square num)
  (* num num))
  
;;LIST of Num -> num
;;takes in a list of numbers and produces a number which
;;is the sum of the square of all the numbers of the lists
(define (sum-of-squares list)
  (foldl + 0 (map square list)))
  |#
;(test (sum-of-squares (list 2  3)) => 13)
;(test (sum-of-squares (list 0 1)) => 1)
#|
;;3
;;binary-tree is a
;; Leaf
;; Node
;;A leaf is 
;; a num
;;a Node is
;; binary-tree binary-tree
(define-type BinTree 
  [Leaf Number]
  [Node BinTree BinTree])
;;(num a -> num b) BinTree -> BinTree
(define (tree-map f tree)
  (cases tree
    [(Leaf n) (Leaf(f n))]
    [(Node l r) (Node (tree-map f l) (tree-map f r))]))
(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))))
      => (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))
;;tree-fold: (num a num b -> num c) (num a -> num b) BinTree -> List
;;takes a tree and a 2 argumment number function and a one argument number
;;function and a bintree and returns a list

4.
|#
(define (minutes-spent)
  240)