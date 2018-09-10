#lang plai-typed

(define-type ExprC
    [numC (n : number)]
    [idC (s : symbol)]
    ; function also is a expression
    [appC (fun : ExprC) (arg : ExprC)]
    [plusC (l : ExprC) (r : ExprC)]
    [multC (l : ExprC) (r : ExprC)]
    ; func decl
    [fdC (name : symbol) (arg : symbol) (body : ExprC)]
)

; return value which is not always a number
(define-type Value
    [numV (n : number)]
    [funV (name : symbol) (arg : symbol) (body : ExprC)]
)

; interp-higher-order-function
(define (interp [expr : ExprC] [env : Env]) : Value
    (type-case ExprC expr
      [numC (n) (numV n)]
      [idC (s) (lookup s env)]
      ; find the function body in env
      [appC (fun arg) 
      ; get the funV
      (local ([define fd (interp fun env)])
        (cond 
            [(funV? fd)
            ; get the funV return value
            (interp (funV-body fd)
                    ; The env only has its arg-symbol -> arg-expr-value
                    (extend-env 
                        (bind (funV-arg fd) (interp arg env)) mt-env)
            )]
            [else (error 'fd "is not a funtion")]
        ))]
      [plusC (l r) (num+ (interp l env) (interp r env))]
      [multC (l r) (num* (interp l env) (interp r env))]
      [fdC (name arg body) (funV name arg body)]
))

(define (num+ [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r)) (numV (+ (numV-n l) (numV-n r)))]
        [else (error 'num+ "one argument was not a number")])
)

(define (num* [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r)) (numV (* (numV-n l) (numV-n r)))]
        [else (error 'num* "one argument was not a number")])
)

(define-type Binding
    [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define (lookup [name : symbol] [env : Env]) : Value
    (cond 
        [(empty? env) (error 'lookup "name not found")]
        [(symbol=? name (bind-name (first env))) (bind-val (first env))]
        [else (lookup name (rest env))]
    ))

; (+ 10 ((lambda (_) 5) 10)) == 15
(test (interp (plusC (numC 10) (appC (fdC 'const5 '_ (numC 5)) (numC 10))) mt-env) (numV 15))
(test/exn (interp (appC (fdC 'f1 'x (appC (fdC 'f2 'y (plusC (idC 'x) (idC 'y))) (numC 4))) (numC 3)) mt-env) "name not found")