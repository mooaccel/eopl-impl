#lang eopl

; 见EOPL p20
; S-list ::= ({ S-exp }^*) 
; S-exp  ::= Symbol | S-list

(define-datatype s-exp s-exp?
  (symbol-s-exp
    (sym symbol?))
  (s-list-s-exp
    (slst s-list?)))
; 在上面使用s-list?(还没真正调用), 但是slist?还没定义好, 底下估计只是个名字放那里当placeholder一样的东西? 或者是延迟绑定?

; 第二种方法定义s-list
; list-of是一个higer-order function
; val是一个{s-exp}的list
(define-datatype s-list s-list?
  (an-s-list
    (listof_exp (list-of s-exp?))))

(define (list-of pred)
  (lambda (val)
   (or (null? val)
       (and (pair? val)
            (pred (car val))
            ((list-of pred) (cdr val)))))
)

(define val1 (an-s-list (list (symbol-s-exp 'a) (symbol-s-exp 'b))))
(display val1)
(newline)
(display (equal? (s-exp? val1) #f))
(newline)
(display (equal? (s-list? val1) #t))
(newline)
; 生成一个slist
; 调用an-s-list, 给的参数必须每个都满足s-exp? pred
(define val2 (an-s-list (list (s-list-s-exp val1) (symbol-s-exp 'c))))
(eopl:pretty-print val2)
(newline)