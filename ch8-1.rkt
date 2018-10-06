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
    [setboxC (b : ExprC) (val : ExprC)]
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
    [v*s (val : Value) (store : Store)])

; 贮存传递模式（store-passing style）
; 赋值引入了时间的概念；使用原先的贮存则允许我们回到过去，也就是赋值没有发生之前
; 贮存是线式传递的：所有的分支并不使用同一个贮存，
; 前一个分支产生的贮存后一个分支使用，最后一个分支的贮存就是总的返回贮存。
; 这种风格被称作贮存传递模式（store-passing style）。

; 现在，分支上可能会执行赋值操作从而因此影响到另一分支，
; 因此要使该语言的程序员能预测自己程序的行为，我们必须选择某种求值顺序
(define (interp [expr : ExprC] [env : Env] [store : Store]) : Result
    (type-case ExprC expr
      [numC (n) (v*s (numV n) store)]
      [idC (s) (v*s (fetch (lookup s env) store) store)]
      ; find the function body in env
      [appC (f arg) (type-case Result (interp f env store)
            [v*s (f-val f-store) (type-case Result (interp arg env f-store)
                [v*s (arg-val arg-store) (let ([where (new-loc)])
                    (interp 
                        (closV-body f-val) 
                        ; 添加参数到新地址的映射
                        (extend-env 
                            (bind (closV-arg f-val) where)
                            (closV-env f-val)
                        )
                        ; 添加新的贮存
                        (override-store (cell where arg-val) arg-store)
                    )
                )]
            )]
      )]
      ; we need to consider that left and right both produce box data
      [plusC (l r) (type-case Result (interp l env store)
                    [v*s (l-val l-store) (type-case Result (interp r env l-store)
                        [v*s (r-val r-store) (v*s (num+ l-val r-val) r-store)]    
                    )]
      )]
      [multC (l r) (type-case Result (interp l env store)
                    [v*s (l-val l-store) (type-case Result (interp r env l-store)
                        [v*s (r-val r-store) (v*s (num* l-val r-val) r-store)]    
                    )]
      )]
      ; record all env
      [lamC (arg body) (v*s (closV arg body env) store)]
      ; right is a expr
      ; box need to record the store address 
      [boxC (arg) (type-case Result (interp arg env store)
                    [v*s (arg-val arg-store)
                    (let ([where (new-loc)])
                        (v*s (boxV where)
                            (override-store (cell where arg-val)
                                arg-store)
                        )
                    )]
      )]
      [unboxC (arg) (type-case Result (interp arg env store)
            [v*s (arg-val arg-store) (v*s (fetch (boxV-loc arg-val) arg-store) arg-store)]
      )]
      [setboxC (b val) (type-case Result (interp b env store)
            [v*s (b-val b-store) (type-case Result (interp val env b-store)
                [v*s (val-val val-store)
                    (v*s val-val 
                        (override-store 
                            (cell (boxV-loc b-val) val-val)
                            val-store
                        )
                    ) 
                ]
            )]
      )]
      ; when eval the b2, using b1's returned store
      [seqC (b1 b2) (type-case Result (interp b1 env store)
                    (v*s (b1-val b1-store) (interp b2 env b1-store)))]
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

; test box and unbox and setbox
(test (v*s-val (interp 
        (unboxC (boxC (plusC 
                        (setboxC (boxC (numC 5)) (numC 10))
                        (numC 10)
                    )
                )
        )
        mt-env 
        mt-store))
(numV 20))

; test for seq
(test (v*s-val (interp
        (seqC 
            (plusC (numC 1) (numC 2))
            (multC (numC 2) (numC 5))
        )
        mt-env 
        mt-store))
(numV 10))

;;;;;;;; notes

; 想象一下，我们不直接改变贮存，而是引入日志的概念，表示贮存中意向中的更
; 新。日志的实现方式类似于贮存，线性传递。（语言中）添加创建新日志的指令；对于
; 查询操作，首先检查日志，仅当日志中找不到某个地址的绑定时，才在实际贮存中查
; 找。还要添加两个新指令：丢弃（discard）某个日志（用于进行时间回溯），以及 提交
; （commit）操作（将某个日志中的修改全部应用到贮存中）。
; 事实上这就是软件事务内存（Software Transactional Memory）的概念。（每条线程都
; 只能看到自己的日志和全局的贮存，看不到其他线程的日志，）其他线程在提交日志之
; 前所做的修改对本线程是透明的。这就是说，每个线程看到的世界都是一致的（能看到
; 自己所做的修改，因为它们都在日志中）。如果事务成功完成（提交），那么所有线程
; 都会看到更新后的全局贮存；如果事务中止（丢弃），被丢弃的日志也带走了其中所
; 有的修改，状态还原（其他线程做提交还是会生效）。