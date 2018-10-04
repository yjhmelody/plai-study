#lang plai-typed

(define-type ExprC
    [numC (n : number)]
    [idC (s : symbol)]
    ; function also is a expression
    [appC (fun : ExprC) (arg : ExprC)]
    [plusC (l : ExprC) (r : ExprC)]
    [multC (l : ExprC) (r : ExprC)]
    [lamC (arg : symbol) (body : ExprC)]
)

; return value which is not always a number
(define-type Value
    [numV (n : number)]
    ; implment the closure
    [closV (arg : symbol) (body : ExprC) (env : Env)]
)

; interp-higher-order-function
(define (interp [expr : ExprC] [env : Env]) : Value
    (type-case ExprC expr
      [numC (n) (numV n)]
      [idC (s) (lookup s env)]
      ; find the function body in env
      [appC (fun arg) 
      ; get the funV
      (local ([define fun-value (interp fun env)])
        (interp (closV-body fun-value)
            ; extend env with binding arg -> Value
            (extend-env (bind (closV-arg fun-value) 
                (interp arg env))
                env))
      )]
      [plusC (l r) (num+ (interp l env) (interp r env))]
      [multC (l r) (num* (interp l env) (interp r env))]
      ; record all env
      [lamC (arg body) (closV arg body env)]      
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


(test (interp (plusC (numC 10) (numC 5))
              mt-env)
      (numV 15))

(test (interp (appC (lamC 'x (appC (lamC 'y (plusC (idC 'x) (idC 'y)))
                                   (numC 4)))
                    (numC 3))
              mt-env)
      (numV 7))

(test (interp (appC (lamC 'x (appC (lamC 'y (multC (idC 'x) (idC 'y))) 
    (numC 2))) 
    (numC 3)) 
    mt-env) 
    (numV 6))


; 环境模型实际上实现了非捕获型替换
; 以下例子会报错
; (lambda (f)
; (lambda (x)
; (f 10)))    

; (lambda (y) (+ x y))

; (lambda (x)
; ((lambda (y) (+ x y)) 10))    
(test/exn
    (interp
        (appC
            (appC (lamC 'f (lamC 'x (appC (idC 'f) (numC 10))))
            (lamC 'y (plusC (idC 'x) (idC 'y))))
      (numC 5))
    mt-env)
"name not found")