;; ---< The BRANG interpreter, using environments >---

#lang pl 06

#|
The grammar:
  <BRANG> ::= <num>
            | { + <BRANG> <BRANG> }
            | { - <BRANG> <BRANG> }
            | { * <BRANG> <BRANG> }
            | { / <BRANG> <BRANG> }
            | { with { <id> <BRANG> } <BRANG> }
            | <id>
            | { fun { <id> } <BRANG> }
            | { call <BRANG> <BRANG> }

Evaluation rules:
  eval(N,env)                = N
  eval({+ E1 E2},env)        = eval(E1,env) + eval(E2,env)
  eval({- E1 E2},env)        = eval(E1,env) - eval(E2,env)
  eval({* E1 E2},env)        = eval(E1,env) * eval(E2,env)
  eval({/ E1 E2},env)        = eval(E1,env) / eval(E2,env)
  eval(x,env)                = lookup(x,env)
  eval({with {x E1} E2},env) = eval(E2,extend(x,eval(E1,env),env))
  eval({fun {x} E},env)      = <{fun {x} E}, env>
  eval({call E1 E2},env1)
           = eval(Ef,extend(x,eval(E2,env1),env2))
                             if eval(E1,env1) = <{fun {x} Ef}, env2>
           = error!          otherwise
|#

(define-type BRANG
  [Num  Number]
  [Add  BRANG BRANG]
  [Sub  BRANG BRANG]
  [Mul  BRANG BRANG]
  [Div  BRANG BRANG]
  [Id   Symbol]
  [With Symbol BRANG BRANG]
  [Fun  Symbol BRANG]
  [Call BRANG BRANG])

(: parse-sexpr : Sexpr -> BRANG)
;; to convert s-expressions into BRANGs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)    (Num n)]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(cons 'fun more)
     (match sexpr
       [(list 'fun (list (symbol: name)) body)
        (Fun name (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'call fun arg) (Call (parse-sexpr fun) (parse-sexpr arg))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> BRANG)
;; parses a string containing a BRANG expression to a BRANG AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

;; Types for environments, values, and a lookup function

(define-type ENV = (Listof Val))

(define-type VAL
  [NumV Number]
  [FunV Symbol BRANG ENV])



(: NumV->number : VAL -> Number)
;; convert a BRANG runtime numeric value to a Racket one
(define (NumV->number v)
  (cases v
    [(NumV n) n]
    [else (error 'arith-op "expected a number, got: ~s" v)]))

(: arith-op : (Number Number -> Number) VAL VAL -> VAL)
;; gets a Racket numeric binary operator, and uses it within a NumV
;; wrapper
(define (arith-op op val1 val2)
  (NumV (op (NumV->number val1) (NumV->number val2))))

(: eval : BRANG ENV -> VAL)
;; evaluates BRANG expressions by reducing them to values
(define (eval expr env)
  (cases expr
    [(Num n) (NumV n)]
    [(Add l r) (arith-op + (eval l env) (eval r env))]
    [(Sub l r) (arith-op - (eval l env) (eval r env))]
    [(Mul l r) (arith-op * (eval l env) (eval r env))]
    [(Div l r) (arith-op / (eval l env) (eval r env))]
    [(With bound-id named-expr bound-body)
     (eval bound-body
           (Extend bound-id (eval named-expr env) env))]
    [(Id name) (lookup name env)]
    [(Fun bound-id bound-body)
     (FunV bound-id bound-body env)]
    [(Call fun-expr arg-expr)
     (let ([fval (eval fun-expr env)])
       (cases fval
         [(FunV bound-id bound-body f-env)
          (eval bound-body
                (Extend bound-id (eval arg-expr env) f-env))]
         [else (error 'eval "`call' expects a function, got: ~s"
                            fval)]))]))

(: run : String -> Number)
;; evaluate a BRANG program contained in a string
(define (run str)
  (let ([result (eval (parse str) (EmptyEnv))])
    (cases result
      [(NumV n) n]
      [else (error 'run
                   "evaluation returned a non-number: ~s" result)])))









#|
The grammar:
  <CORE> ::= <num>
            | { + <CORE> <CORE> }
            | { - <CORE> <CORE> }
            | { * <CORE> <CORE> }
            | { / <CORE> <CORE> }
            | { with { <id> <CORE> } <CORE> }
            | <id>
            | { fun { <id> } <CORE> }
            | { call <CORE> <CORE> }

Evaluation rules:
  ceval(N,env)                = N
  ceval({+ E1 E2},env)        = ceval(E1,env) + eval(E2,env)
  ceval({- E1 E2},env)        = ceval(E1,env) - eval(E2,env)
  ceval({* E1 E2},env)        = ceval(E1,env) * eval(E2,env)
  ceval({/ E1 E2},env)        = ceval(E1,env) / eval(E2,env)
  ceval(x,env)                = clookup(x,env)
  ceval({with {x E1} E2},env) = eval(E2,extend(x,eval(E1,env),env))
  ceval({fun {x} E},env)      = <{fun {x} E}, env>
  ceval({call E1 E2},env1)
           = eval(Ef,extend(x,eval(E2,env1),env2))
                             if eval(E1,env1) = <{fun {x} Ef}, env2>
           = error!          otherwise
|#

(define-type CORE
  [CNum  Natural]
  [CAdd  CORE CORE]
  [CSub  CORE CORE]
  [CMul  CORE CORE]
  [CDiv  CORE CORE]
  [CRef   Symbol]
  [CWith Symbol CORE CORE]
  [CFun  Symbol CORE]
  [CCall CORE CORE])

(: cparse-sexpr : Sexpr -> CORE)
;; to convert s-expressions into COREs
(define (cparse-sexpr sexpr)
  (match sexpr
    [(number: n)    (CNum n)]
    [(symbol: name) (CRef name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (CWith name (cparse-sexpr named) (cparse-sexpr body))]
       [else (error 'cparse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(cons 'fun more)
     (match sexpr
       [(list 'fun (list (symbol: name)) body)
        (CFun name (cparse-sexpr body))]
       [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (CAdd (cparse-sexpr lhs) (cparse-sexpr rhs))]
    [(list '- lhs rhs) (CSub (cparse-sexpr lhs) (cparse-sexpr rhs))]
    [(list '* lhs rhs) (CMul (cparse-sexpr lhs) (cparse-sexpr rhs))]
    [(list '/ lhs rhs) (CDiv (cparse-sexpr lhs) (cparse-sexpr rhs))]
    [(list 'call fun arg) (CCall (cparse-sexpr fun) (cparse-sexpr arg))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: cparse : String -> CORE)
;; parses a string containing a BRANG expression to a BRANG AST
(define (cparse str)
  (cparse-sexpr (string->sexpr str)))

;; Types for environments, values, and a lookup function

(define-type CENV
  [CEmptyEnv]
  [CExtend Symbol CVAL CENV])

(define-type CVAL
  [CNumV Number]
  [CFunV Symbol CORE CENV])

(: clookup : Symbol CENV -> CVAL)
(define (clookup name env)
  (cases env
    [(CEmptyEnv) (error 'clookup "no binding for ~s" name)]
    [(CExtend id cval rest-env)
     (if (eq? id name) cval (clookup name rest-env))]))

(: CNumV->number : CVAL -> Number)
;; convert a BRANG runtime numeric value to a Racket one
(define (CNumV->number v)
  (cases v
    [(CNumV n) n]
    [else (error 'arith-op "expected a number, got: ~s" v)]))

(: carith-op : (Number Number -> Number) CVAL CVAL -> CVAL)
;; gets a Racket numeric binary operator, and uses it within a CNumV
;; wrapper
(define (carith-op op val1 val2)
  (CNumV (op (CNumV->number val1) (CNumV->number val2))))

(: ceval : CORE CENV -> CVAL)
;; evaluates CORE expressions by reducing them to values
(define (ceval expr env)
  (cases expr
    [(CNum n) (CNumV n)]
    [(CAdd l r) (carith-op + (ceval l env) (ceval r env))]
    [(CSub l r) (carith-op - (ceval l env) (ceval r env))]
    [(CMul l r) (carith-op * (ceval l env) (ceval r env))]
    [(CDiv l r) (carith-op / (ceval l env) (ceval r env))]
    [(CWith bound-id named-expr bound-body)
     (ceval bound-body
           (CExtend bound-id (ceval named-expr env) env))]
    [(CRef name) (clookup name env)]
    [(CFun bound-id bound-body)
     (CFunV bound-id bound-body env)]
    [(CCall fun-expr arg-expr)
     (let ([fval (ceval fun-expr env)])
       (cases fval
         [(CFunV bound-id bound-body f-env)
          (ceval bound-body
                (CExtend bound-id (ceval arg-expr env) f-env))]
         [else (error 'ceval "`call' expects a function, got: ~s"
                            fval)]))]))

(: crun : String -> Number)
;; evaluate a BRANG program contained in a string
(define (crun str)
  (let ([result (ceval (cparse str) (CEmptyEnv))])
    (cases result
      [(CNumV n) n]
      [else (error 'run
                   "evaluation returned a non-number: ~s" result)])))






;; tests
(test (run "{call {fun {x} {+ x 1}} 4}")
      => 5)
(test (run "{with {add3 {fun {x} {+ x 3}}}
              {call add3 1}}")
      => 4)
(test (run "{with {add3 {fun {x} {+ x 3}}}
              {with {add1 {fun {x} {+ x 1}}}
                {with {x 3}
                  {call add1 {call add3 x}}}}}")
      => 7)
(test (run "{with {identity {fun {x} x}}
              {with {foo {fun {x} {+ x 1}}}
                {call {call identity foo} 123}}}")
      => 124)
(test (run "{with {x 3}
              {with {f {fun {y} {+ x y}}}
                {with {x 5}
                  {call f 4}}}}")
      => 7)
(test (run "{call {with {x 3}
                    {fun {y} {+ x y}}}
                  4}")
      => 7)
(test (run "{call {call {fun {x} {call x 1}}
                        {fun {x} {fun {y} {+ x y}}}}
                  123}")
      => 124)
(test (run "{+ 1 {- 1 {* 1 {/ 1 1}}}}") => 1)
(test (run "{call x}")
      =error> "bad syntax in (call x)")
(test (run "{with x}")
      =error> " bad `with' syntax in (with x)")
(test (run "{fun x}")
      =error> " bad `fun' syntax in (fun x)")