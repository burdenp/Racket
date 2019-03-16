#lang pl 4

#| BNF for the ALGAE language:
     <ALGAE> ::= <num>
               | True | False
               | { + <ALGAE> ... }
               | { * <ALGAE> ... }
               | { - <ALGAE> ... }
               | { / <ALGAE> ... }
               | { with { <id> <ALGAE> } <ALGAE> }
               | <id>
               | { < <ALGAE> <ALGAE> }
               | { = <ALGAE> <ALGAE> }
               | { <= <ALGAE> <ALGAE> }
               | { if <ALGAE> <ALGAE> <ALGAE> }
               | { not <ALGAE> }
               | { and <ALGAE> ... }
               | { or <ALGAE> ... }
|#

;; ALGAE abstract syntax trees
(define-type ALGAE
  [Num  Number]
  [Add  (Listof ALGAE)]
  [Mul  (Listof ALGAE)]
  [Sub  ALGAE (Listof ALGAE)]
  [Div  ALGAE (Listof ALGAE)]
  [Id   Symbol]
  [With Symbol ALGAE ALGAE]
  [Less ALGAE ALGAE]
  [Equal ALGAE ALGAE]
  [LessEq ALGAE ALGAE]
  [Bool Boolean]
  [If ALGAE ALGAE ALGAE])

(define-type PROGRAM
  [Funs  FUN])

(define-type FUN
  [Fun Symbol Symbol ALGAE])

(: Not : ALGAE -> ALGAE)
;; to negate the value of a boolean expression
(define (Not a)
  (If a (Bool #f) (Bool #t)))

(: And : (Listof ALGAE) -> ALGAE)
;; to perform conjunction on two boolean expressions
(define (And a)
  (cond
    [(null? a) (Bool #t)]
    [(null? (rest a)) (first a)]
    [else (If (first a) (And (rest a)) (Bool #f))]))

(: Or : (Listof ALGAE) -> ALGAE)
;; to perform disjunction on two boolean expressions
(define (Or a)
  (cond
    [(null? a) (Bool #f)]
    [(null? (rest a)) (first a)]
    [else (If (first a) (Bool #t) (Or (rest a)))]))
  

(: parse-sexpr : Sexpr -> ALGAE)
;; to convert s-expressions into ALGAEs
(define (parse-sexpr sexpr)
  ;; utility for parsing a list of expressions
  (: parse-sexprs : (Listof Sexpr) -> (Listof ALGAE))
  (define (parse-sexprs sexprs) (map parse-sexpr sexprs))
  (match sexpr
    [(number: n)    (Num n)]
    ['True (Bool #t)]
    ['False (Bool #f)]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(list '+ (sexpr: args) ...) (Add (parse-sexprs args))]
    [(list '* (sexpr: args) ...) (Mul (parse-sexprs args))]
    [(list '- fst (sexpr: args) ...)
     (Sub (parse-sexpr fst) (parse-sexprs args))]
    [(list '/ fst (sexpr: args) ...)
     (Div (parse-sexpr fst) (parse-sexprs args))]
    [(list '< left right)
     (Less (parse-sexpr left) (parse-sexpr right))]
    [(list '= left right)
     (Equal (parse-sexpr left) (parse-sexpr right))]
    [(list '<= left right)
     (LessEq (parse-sexpr left) (parse-sexpr right))]
    [(list 'if cond if-branch else-branch)
     (If (parse-sexpr cond) (parse-sexpr if-branch) (parse-sexpr else-branch))]
    [(list 'not a) (Not (parse-sexpr a))]
    [(list 'and (sexpr: a) ...) (And (parse-sexprs a))]
    [(list 'or (sexpr: a) ...) (Or (parse-sexprs a))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> ALGAE)
;; parses a string containing an ALGAE expression to an ALGAE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

#| Formal specs for `subst':
   (`N' is a <num>, `B' is a boolean, `E1', `E2', `E3' are <ALGAE>s,
   `x' is some <id>, `y' is a *different* <id>)
      N[v/x]                = N
      B[v/x]                = B
      {+ E ...}[v/x]        = {+ E[v/x] ...}
      {* E ...}[v/x]        = {* E[v/x] ...}
      {- E1 E ...}[v/x]     = {- E1[v/x] E[v/x] ...}
      {/ E1 E ...}[v/x]     = {/ E1[v/x] E[v/x] ...}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
      {< E1 E2}[v/x]        = {< E1[v/x] E2[v/x]}
      {= E1 E2}[v/x]        = {= E1[v/x] E2[v/x]}
      {<= E1 E2}[v/x]       = {<= E1[v/x] E2[v/x]}
      {if E1 E2 E3}[v/x]    = {if E1[v/x] E2[v/x] E3[v/x]}
|#

(: subst : ALGAE Symbol ALGAE -> ALGAE)
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (subst expr from to)
  ;; convenient helper -- no need to specify `from' and `to'
  (: subst* : ALGAE -> ALGAE)
  (define (subst* x) (subst x from to))
  ;; helper to substitute lists
  (: substs* : (Listof ALGAE) -> (Listof ALGAE))
  (define (substs* exprs) (map subst* exprs))
  (cases expr
    [(Num n)        expr]
    [(Bool b)       expr]
    [(Add args)     (Add (substs* args))]
    [(Mul args)     (Mul (substs* args))]
    [(Sub fst args) (Sub (subst* fst) (substs* args))]
    [(Div fst args) (Div (subst* fst) (substs* args))]
    [(Id name)      (if (eq? name from) to expr)]
    [(With bound-id named-expr bound-body)
     (With bound-id
           (subst* named-expr)
           (if (eq? bound-id from)
               bound-body
               (subst* bound-body)))]
    [(Less left right) (Less (subst* left) (subst* right))]
    [(Equal left right) (Equal (subst* left) (subst* right))]
    [(LessEq left right) (LessEq (subst* left) (subst* right))]
    [(If cond if-branch else-branch)
     (If (subst* cond) (subst* if-branch) (subst* else-branch))]))

#| Formal specs for `eval':
     eval(N)            = N
     eval(True)         = #t
     eval(False)        = #f
     eval({+ E ...})    = evalN(E) + ...
     eval({* E ...})    = evalN(E) * ...
     eval({- E})        = -evalN(E)
     eval({/ E})        = 1/evalN(E)
     eval({- E1 E ...}) = evalN(E1) - (evalN(E) + ...)
     eval({/ E1 E ...}) = evalN(E1) / (evalN(E) * ...)
     eval(id)           = error!
     eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
     evalN(E) = eval(E) if it is a number, error otherwise
     eval({< E1 E2})    = evalN(E1) < evalN(E2)
     eval({= E1 E2})    = evalN(E1) == evalN(E2)
     eval({<= E1 E2})   = evalN(E1) <= evalN(E2)
     eval({if E1 E2 E3})= eval(E2) if evalB(E1) is true
                          eval(E3) if evalB(E1) is false
                          error    otherwise
     eval({Not E})      = eval({if E False True})
     eval({And E1 E2})  = eval({if E1 E2 False})
     eval({Or E1 E2})   = eval({if E1 True E2})
|#

(: eval-number : ALGAE -> Number)
;; helper for `eval': verifies that the result is a number
(define (eval-number expr)
  (let ([result (eval expr)])
    (if (number? result)
        result
        (error 'eval-number "need a number when evaluating ~s, but got ~s"
               expr result))))

(: eval-boolean : ALGAE -> Boolean)
;; helper for `eval': verifies that the result is a boolean
(define (eval-boolean expr)
  (let ([result (eval expr)])
    (if (boolean? result)
        result
        (error 'eval-boolean "need a boolean when evaluating ~s, but got ~s"
               expr result))))

(: value->algae : (U Number Boolean) -> ALGAE)
;; converts a value to an ALGAE value (so it can be used with `subst')
(define (value->algae val)
  (cond [(number? val) (Num val)]
        [(boolean? val) (Bool val)]
        ;; Note: since we use Typed Racket, the type checker makes sure
        ;; that this function is never called with something that is not
        ;; in its type, so there's no need for an `else' branch.
        ;; (Strictly speaking, there's no need for the last predicate
        ;; (which is the only one here until you extend this), but it's
        ;; left in for clarity)
        ;; [else (error 'value->algae "unexpected value: ~s" val)]
        ))
;; The following test is also not needed.  In the untyped version, it
;; was needed because the error could not be achieved through `eval' --
;; which is exactly why the above type works.
;; ;; test for an otherwise unreachable error:
;; (test (value->algae null) =error> "unexpected value")

(: eval : ALGAE -> (U Number Boolean))
;; evaluates ALGAE expressions by reducing them to numbers or booleans
(define (eval expr)
  (cases expr
    [(Num n) n]
    [(Bool b) b]
    [(Add args) (foldl + 0 (map eval-number args))]
    [(Mul args) (foldl * 1 (map eval-number args))]
    [(Sub fst args) (if (equal? null args)
                        (- (eval-number fst))
                        (- (eval-number fst)
                           (foldl + 0 (map eval-number args))))]
    [(Div fst args) (if (equal? null args)
                        (/ (eval-number fst))
                        (let [(denominator (foldl * 1 (map eval-number args)))]
                          (if (equal? denominator 0)
                              (error '/ "division by zero")
                              (/ (eval-number fst) denominator))))]
    [(With bound-id named-expr bound-body)
     (eval (subst bound-body
                  bound-id
                  ;; see the above `value->algae' helper
                  (value->algae (eval named-expr))))]
    [(Id name) (error 'eval "free identifier: ~s" name)]
    [(Less left right) (< (eval-number left) (eval-number right))]
    [(Equal left right) (= (eval-number left) (eval-number right))]
    [(LessEq left right) (<= (eval-number left) (eval-number right))]
    [(If cond if-branch else-branch)
     (if (eval-boolean cond)
         (eval if-branch)
         (eval else-branch))]))

(: run : String -> (U Number Boolean))
;; evaluate an ALGAE program contained in a string
(define (run str)
  (eval (parse str)))

;; tests
(test (run "5") => 5)
(test (run "{+ 5 5}") => 10)
(test (run "{with {x {+ 5 5}} {+ x x}}") => 20)
(test (run "{with {x 5} {+ x x}}") => 10)
(test (run "{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}") => 14)
(test (run "{with {x 5} {with {y {- x 3}} {+ y y}}}") => 4)
(test (run "{with {x 5} {+ x {with {x 3} 10}}}") => 15)
(test (run "{with {x 5} {+ x {with {x 3} x}}}") => 8)
(test (run "{with {x 5} {+ x {with {y 3} x}}}") => 10)
(test (run "{with {x 5} {with {y x} y}}") => 5)
(test (run "{with {x 5} {with {x x} x}}") => 5)
(test (run "{with with}") =error>
      "parse-sexpr: bad `with' syntax in (with with)")
(test (run "{* 2 3}") => 6)
(test (run "{/ 5 1}") => 5)
(test (run "{/ }") =error> "parse-sexpr: bad syntax in (/)")
(test (run "{with {x 5} {* 5 x}}") => 25)
(test (run "{with {x 5} {/ 5 x}}") => 1)
(test (run "{/ *}") =error> "eval: free identifier: *")
(test (run "{+ 1 2 3 4 5}") => 15)
(test (run "{* 1 2 3 4 5}") => 120)
(test (run "{- 100 50 -50 100}") => 0)
(test (run "{/ 100 4 5 5}") => 1)
(test (run "{/ 100 4 5 5 0}") =error> "/: division by zero")
(test (run "{+}") => 0)
(test (run "{*}") => 1)
(test (run "{- 2}") => -2)
(test (run "{/ 3}") => 1/3)
(test (run "{< 4 5}") => #t)
(test (run "{<= 4 4}") => #t)
(test (run "{= 4 4}") => #t)
(test (run "{< 4 2}") => #f)
(test (run "{<= 4 3}") => #f)
(test (run "{= 4 3}") => #f)
(test (run "{= s 3}") =error> "eval: free identifier: s")
(test (run "{with {x 5} {< 4 x}}") => #t)
(test (run "{with {x 6} {<= 6 x}}") => #t)
(test (run "{with {x 6} {<= 7 x}}") => #f)
(test (run "{with {x 6} {<= 5 x}}") => #t)
(test (run "{with {x 5} {< 5 x}}") => #f)
(test (run "{with {x 5} {= 5 x}}") => #t)
(test (run "{with {x 5} {= 6 x}}") => #f)
(test (run "{if {< 4 5} {+ 5 6} {+ 4 6}}") => 11)
(test (run "{if True 5 6}") => 5)
(test (run "{if False 5 6}") => 6)
(test (run "{and True False}") => #f)
(test (run "{not True}") => #f)
(test (run "{or True False}") => #t)
(test (run "{with {x True} {if x 5 6}}") => 5)
(test (run "{or False False}") => #f)
(test (run "{or False True}") => #t)
(test (run "{or True}") => #t)
(test (run "{with {x True} {not x}}") => #f)
(test (run "{with {x False} {or x False}}") => #f)
(test (run "{with {x True} {and x True}}") => #t)
(test (run "{and }") => #t)
(test (run "{or }") => #f)
(test (run "{if 5 4 2}")
      =error> 
      "eval-boolean: need a boolean when evaluating (Num 5), but got 5")
(test (run "{+ 5 True}")
      =error>
      "eval-number: need a number when evaluating (Bool #t), but got #t")

(define minutes-spent 120)