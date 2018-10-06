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

;;;;;;;;;;;;;;;;;;;;;;;;

; Express ast
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
    [boxV (loc : Location)]
)

; interpreter's return value
(define-type Result 
    [v*s (v : Value) (s : Storage)])

; interp-higher-order-function
(define (interp [expr : ExprC] [env : Env] [store : Store]) : Result
    (type-case ExprC expr
      [numC (n) (v*s (numV n) store)]
      [idC (s) (v*s (fetch (lookup s env) store) store)]
      ; find the function body in env
      [appC (fun arg) 
      ; get the funV
      (let ([fun-value (interp fun env store)])
        (interp (closV-body fun-value)
            ; extend env with binding arg -> Value
            (extend-env (bind (closV-arg fun-value) 
                (interp arg env store))
                env 
                store))
      )]
      [plusC (l r) (num+ (interp l env store) (interp r env store))]
      [multC (l r) (num* (interp l env store) (interp r env store))]
      ; record all env
      [lamC (arg body) (v*s (closV arg body env) store)]
      ; right is a expr
      [boxC (arg) (boxV (interp arg env store))]
      [unboxC (arg) (fetch (boxV-loc (interp arg env store)))]
      ; when eval the b2, using b1's returned store
      [seqC (b1 b2) (type-case Result (interp b1 env store))])
                    (v*s (b1-val b1-store) (interp b2 env b1-store))]
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

; Bind name with location
(define-type Binding
    [bind (name : symbol) (loc : Location)])
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

; Store
(define-type-alias Location number) ; address
(define-type Storage
    [cell (loc : Location) (val : Value)]
)
(define-type-alias Store (listof Storage))
(define mt-store empty)
(define override-store cons) ; override store

; lookup name's location from env
(define (lookup [name : symbol] [env : Env]) : Location
    (cond 
        [(empty? env) (error 'lookup "name not found")]
        [(symbol=? name (bind-name (first env))) (bind-loc (first env))]
        [else (lookup name (rest env))]
    ))

; fetch value from some location of store
(define (fetch [loc : Location] [store : Store]) : Value 
    (cond
        [(empty? store) (error 'fetch "loc not found")]
        [(= loc (cell-loc (first store))) (cell-val (first store))]
        [else (fetch loc (rest store))] 
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;

; test box and unbox
(test
(interp (unboxC (boxC (plusC (numC 5) (numC 10)))) mt-env mt-store)
(numV 15))