#lang plai-typed

; mutable box
; set-box! return void
(let ([b0 (box 0)]
    [b1 (box 1)])
    (let ([l (list b0 b1)])
    (begin
    (set-box! (first l) 1)
    (set-box! (second l) 2)
    l)))
    

; counter
(define new-loc
    (let ([n (box 0)])
    (lambda ()
    (begin
    (set-box! n (add1 (unbox n)))
(unbox n)))))

(new-loc)
(new-loc)

; begin
(let ([b (box 0)])
    (begin (begin (set-box! b (+ 1 (unbox b)))
                  (set-box! b (+ 1 (unbox b))))
            (unbox b)
    )
)

; return 1 + (1+1) == 3
(let ([b (box 0)])
    (+ (begin (set-box! b (+ 1 (unbox b)))
        (unbox b))
        (begin (set-box! b (+ 1 (unbox b)))
    (unbox b))
    )
)


(define-type ExprC
    [numC (n : number)]
    [idC (s : symbol)]
    [appC (fun : ExprC) (arg : ExprC)]
    [plusC (l : ExprC) (r : ExprC)]
    [multC (l : ExprC) (r : ExprC)]
    [lamC (arg : symbol) (body : ExprC)]
    [boxC (arg : ExprC)]
    [unboxC (arg : ExprC)]
    ; [setboxC (b : ExprC) (v : ExprC)]
    ; begin syntax
    [seqC (b1 : ExprC) (b2 : ExprC)]
)

; interpreter's return value
(define-type Value
    ; number
    [numV (n : number)]
    ; closure
    [closV (arg : symbol) (body : ExprC) (env : Env)]
    ; box
    [boxV (v : Value)]
)

; interp-higher-order-function
(define (interp [expr : ExprC] [env : Env]) : Value
    (type-case ExprC expr
      [numC (n) (numV n)]
      [idC (s) (lookup s env)]
      ; find the function body in env
      [appC (fun arg) 
      ; get the funV
      (let ([fun-value (interp fun env)])
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
      ; right is a expr
      [boxC (arg) (boxV (interp arg env))]
      [unboxC (arg) (boxV-v (interp arg env))]
      [seqC (b1 b2) (let ([v (interp b1 env)])
                    (interp b2 env))]
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

; Env
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


; Store
(define-type-alias Location number) ; address
(define-type Storage
    [cell (location : Location) (val : Value)]
)
(define-type-alias Store (listof Storage))
(define mt-store empty)
(define override-store cons)


; test box unbox
(test
(interp (unboxC (boxC (plusC (numC 5) (numC 10)))) mt-env)
(numV 15))