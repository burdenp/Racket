#lang pl 04

#| BNF for the ALGAE language:
     <ALGAE> ::= <num>
               | { + <ALGAE> ... }
               | { * <ALGAE> ... }
               | { - <ALGAE> ... }
               | { / <ALGAE> ... }
               | { with { <id> <ALGAE> } <ALGAE> }
               | <id>
|#

;; ALGAE abstract syntax trees
(define-type ALGAE
  [Num  Number]
  [Add  (Listof ALGAE)]
  [Mul  (Listof ALGAE)]
  [Sub  ALGAE (Listof ALGAE)]
  [Div  ALGAE (Listof ALGAE)]
  [Less ALGAE ALGAE]
  [Equal ALGAE ALGAE]
  [LessEq ALGAE ALGAE]
  [Id   Symbol]
  [With Symbol ALGAE ALGAE]
  [Bool Boolean]
  [If Boolean ALGAE ALGAE]
  [Not ALGAE]
  [Or ALGAE ALGAE]
  [And ALGAE ALGAE])

(: parse-sexpr : Sexpr -> ALGAE)
;; to convert s-expressions into ALGAEs
(define (parse-sexpr sexpr)
  ;; utility for parsing a list of expressions
  (: parse-sexprs : (Listof Sexpr) -> (Listof ALGAE))
  (define (parse-sexprs sexprs) (map parse-sexpr sexprs))
  (match sexpr
    [(number: n)    (Num n)]
    [(symbol: name) (cond
                      [(eq? name 'True) (Bool #t)]
                      [(eq? name 'False) (Bool #f)]
                      [else (Id name)])]
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
    [(list 'if co th el)
     (If (parse-sexpr co) (parse-sexpr th) (parse-sexpr el))]
    [(list '< fst (sexpr: args)) 
     (Less (parse-sexpr fst) (parse-sexpr args))]
    [(list '= fst (sexpr: args)) 
     (Equal (parse-sexpr fst) (parse-sexpr args))]
    [(list '<= fst (sexpr: args)) 
     (LessEq (parse-sexpr fst) (parse-sexpr args))]
    [(list 'not fst)
     (Not (parse-sexpr fst))]
    [(list 'or fst snd)
     (Or (parse-sexpr fst) (parse-sexpr snd))]
    [(list 'and fst snd)
     (And (parse-sexpr fst) (parse-sexpr snd))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> ALGAE)
;; parses a string containing an ALGAE expression to an ALGAE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

#| Formal specs for `subst':
   (`N' is a <num>, `E1', `E2' are <ALGAE>s, `x' is some <id>, `y' is a
   *different* <id>)
      N[v/x]                = N
      {+ E ...}[v/x]        = {+ E[v/x] ...}
      {* E ...}[v/x]        = {* E[v/x] ...}
      {- E1 E ...}[v/x]     = {- E1[v/x] E[v/x] ...}
      {/ E1 E ...}[v/x]     = {/ E1[v/x] E[v/x] ...}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
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
    [(Add args)     (Add (substs* args))]
    [(Mul args)     (Mul (substs* args))]
    [(Sub fst args) (Sub (subst* fst) (substs* args))]
    [(Div fst args) (Div (subst* fst) (substs* args))]
    [(Less fst args) (Less (subst* fst) (subst* args))]
    [(Equal fst args) (Equal (subst* fst) (subst* args))]
    [(LessEq fst args) (LessEq (subst* fst) (subst* args))]
    [(Id name)      (if (eq? name from) to expr)]
    [(With bound-id named-expr bound-body)
     (With bound-id
           (subst* named-expr)
           (if (eq? bound-id from)
             bound-body
             (subst* bound-body)))]
    [(Bool b) expr]
    [(If co th el) (If (subst* co) (subst* th) (subst* el))]
    [(Not fst) (Not (subst* fst))]
    [(Or fst snd) (Or (subst* fst) (subst* snd))]
    [(And fst snd) (And (subst* fst) (subst* snd))]))

#| Formal specs for `eval':
     eval(N)            = N
     eval({+ E ...})    = evalN(E) + ...
     eval({* E ...})    = evalN(E) * ...
     eval({- E})        = -evalN(E)
     eval({/ E})        = 1/evalN(E)
     eval({- E1 E ...}) = evalN(E1) - (evalN(E) + ...)
     eval({/ E1 E ...}) = evalN(E1) / (evalN(E) * ...)
     eval(id)           = error!
     eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
     evalN(E) = eval(E) if it is a number, error otherwise
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

(: 
 : ALGAE -> (U Number Boolean))
;; evaluates ALGAE expressions by reducing them to numbers
(define (eval expr)
  (cases expr
    [(Num n) n]
    [(Add args) (foldl + 0 (map eval-number args))]
    [(Mul args) (foldl * 1 (map eval-number args))]
    [(Sub fst args) (- (eval-number fst) (foldl + 0 (map eval-number args)))]
    [(Div fst args) (/ (eval-number fst) (foldl * 1 (map eval-number args)))]
    [(Less fst args) (< (eval-number fst) (eval-number args))]
    [(Equal fst args) (= (eval-number fst) (eval-number args))]
    [(LessEq fst args) (or (< (eval-number fst) (eval-number args)) 
                           (= (eval-number fst) (eval-number args)))] 
    [(With bound-id named-expr bound-body)
     (eval (subst bound-body
                  bound-id
                  ;; see the above `value->algae' helper
                  (value->algae (eval named-expr))))]
    [(Id name) (error 'eval "free identifier: ~s" name)]
    [(Bool b) b]
    [(If co th el) (if (eval-boolean co)
                       (eval th)
                       (eval el))]
    [(Not fst) (not (eval fst))]
    [(Or fst snd) (or (eval fst)
                      (eval snd))]
    [(And fst snd) (and (eval fst)
                        (eval snd))]))

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
(test (run "{with with}") =error> "parse-sexpr: bad `with' syntax in (with with)")
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
(test (run "{with {x True} {not x}}") => #f)
(test (run "{with {x False} {or x False}}") => #f)
(test (run "{with {x True} {and x True}}") => #t)
(test (run "{if 5 4 2}") 
      =error> 
      "eval-boolean: need a boolean when evaluating (Num 5), but got 5")
(test (run "{+ 5 True}")
      =error>
      "eval-number: need a number when evaluating (Bool #t), but got #t")

