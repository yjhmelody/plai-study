#lang plai-typed

; ADT
(define-type MisspelledAnimal
  [caml (humps : number)]
  [yacc (height : number)])

(define a (caml 1))
(define b (yacc 2))

(define c : MisspelledAnimal (caml 1))
(define d : MisspelledAnimal (yacc 2))

; type match
(define (good? [ma : MisspelledAnimal]) : boolean
  (type-case MisspelledAnimal ma [caml (humps) (>= humps 2)] [yacc (height) (> height 2.1)]))

; 很少有需要用到类型判断函数(如caml?),不过可以用。
; 数据类型定义时还会生成字段提取函数,例如caml-humps。

(define (foo [a : number] [b : number]) : number (+ a b))

; tuple
(define tuple (values 1 2 3))
(define-values (t1 t2 t3) tuple)
(define-values ([t4 : number] [t5 : number] [t6 : number]) tuple)

(define curry2 (lambda (f) (lambda (a) (lambda (b) (f a b)))))
(define curry3 (lambda (f) (lambda (a) (lambda (b) (lambda (c) (f a b c))))))

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])
