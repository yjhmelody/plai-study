#lang plai-typed

; core
(define-type ExprC
    [numC (n : number)]
    [idC (s : symbol)]
    [appC (fun : symbol) (arg : ExprC)]
    [plusC (l : ExprC) (r : ExprC)]
    [multC (l : ExprC) (r : ExprC)]
)

; function declaration
(define-type FunDefC
    [fdC (name : symbol) (arg : symbol) (body : ExprC)]
)

; get-fundef : symbol * (listof FunDefC) -> FunDefC
(define (get-fundef [s : symbol] [fds : (listof FunDefC)]) : FunDefC
    (cond 
        [(empty? fds) (error 'get-fundef "reference to undefined function")]
        [(cons? fds) (cond
                        [(equal? s (fdC-name (first fds))) (first fds)]
                        [else (get-fundef s (rest fds))])]         
))

; env model
; lexical scope (static scope)
(define (interp [e : ExprC] [env : Env] [fds : (listof FunDefC)]) : number
  (type-case ExprC e
    [numC (n) n]
    [idC (n) (lookup n env)]
    ; find the function body by `get-fundef`
    [appC (fun arg) (local ([define fd (get-fundef fun fds)])
                            (interp (fdC-body fd)
                                (extend-env 
                                    ; bind: arg -> function's return value
                                    (bind (fdC-arg fd) (interp arg env fds))
                                ; should not use old env
                                mt-env)
                            fds)
                    )]
    [plusC (l r) (+ (interp l env fds) (interp r env fds))]
    [multC (l r) (* (interp l env fds) (interp r env fds))]
))

(define-type Binding
    [bind (name : symbol) (val : number)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define (lookup [name : symbol] [env : Env]) : number
(cond 
    [(empty? env) (error 'lookup "name not found")]
    [(symbol=? name (bind-name (first env))) (bind-val (first env))]
    [else (lookup name (rest env))]
))

(test (interp (plusC (numC 10) (appC 'const5 (numC 10)))
    mt-env (list (fdC 'const5 '_ (numC 5)))) 
    15)

(test (interp (plusC (numC 10) (appC 'double (plusC (numC 1) (numC 2))))
    mt-env (list (fdC 'double 'x (plusC (idC 'x) (idC 'x))))) 
    16)

(test (interp (plusC (numC 10) (appC 'quadruple (plusC (numC 1) (numC 2))))
    mt-env (list (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x)))) 
        (fdC 'double 'x (plusC (idC 'x) (idC 'x))))) 
    22)