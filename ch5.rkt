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

; subst : ExprC * symbol * ExprC -> ExprC
; replace `for` to `what` in 'in' 
(define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
    (type-case ExprC in
        ; the subst's return value is ExprC, so numC match should return in `which is (numC n)`
        [numC (n) in]
        [idC (s) (cond
                    ; `what` could be `eager eval`
                    [(symbol=? s for) what]
                    [else in])]
        [appC (fun arg) (appC fun (subst what for arg))]
        [plusC (l r) (plusC (subst what for l) (subst what for r))]
        [multC (l r) (multC (subst what for l) (subst what for r))]
))

; substitution model
(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC e
    [numC (n) n]
    ; forbid the free variable
    [idC (_) (error 'interp "should not get here")]
    [appC (fun arg) (local ([define fd (get-fundef fun fds)])
                        (interp (subst arg
                                (fdC-arg fd)
                                (fdC-body fd))
                                fds)
                    )]
    [plusC (l r) (+ (interp l fds) (interp r fds))]
    [multC (l r) (* (interp l fds) (interp r fds))]
))

(define fds (list
    (fdC 'double 'x (plusC (idC 'x) (idC 'x)))
    (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
    (fdC 'const5 'x (numC 5))
))

(test (interp (appC 'double (numC 5)) fds) 10)
(test (interp (appC 'const5 (numC 0)) fds) 5)
(test (interp (appC 'double (appC 'const5 (numC 0))) fds) 10)
(test (interp (appC 'quadruple (appC 'const5 (numC 0))) fds) 20)
