; 怎么基于v1/v2这两个例子去写p21的subst和subst-in-s-exp例子???
; 重新实现下这个互递归例子?todo

#lang eopl

(define-datatype s-exp s-exp?
  (symbol-s-exp
    (sym symbol?))
  (s-list-s-exp
    (slst s-list?)))

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
; ===================
; copy from ./ch2/p48_s_slist/p48_s_slist_v2.scm, 因为还不会搞racket里的module. todo, 待重构


(define slist_01 (an-s-list (list (s-list-s-exp (an-s-list (list (symbol-s-exp 'b)
                                                                 (symbol-s-exp 'c))))
                                  (s-list-s-exp (an-s-list (list (symbol-s-exp 'b)
                                                                 (symbol-s-exp 'd))))
                                  (symbol-s-exp 'b))))
(eopl:pretty-print slist_01)


; 这么套几层好麻烦啊, 为了保持原先的表示...
(define (subst new old slist)
  (cases s-list slist
    (an-s-list (listof_exp)
      (an-s-list (map (lambda (sub_exp)   ; map也能处理null吧. 返回null, 如果是null, 待测试
                         (subst-in-s-exp new old sub_exp))
                      listof_exp)))))

(define (subst-in-s-exp new old sub_exp)
  (cases s-exp sub_exp
    (symbol-s-exp (sym)
      (if (eqv? sym old)
          (symbol-s-exp new)
          (symbol-s-exp sym)))
    (s-list-s-exp (slst)
      (s-list-s-exp (subst new old slst)))))

; 不改变内部表示...
(display "=====")
(newline)
(eopl:pretty-print (subst 'a 'b slist_01))

;(display (subst 'a 'b slist_01))
; 如果slist是null怎么办?
; 这个版本却去掉了表示, 有没有透明的办法? 可以不用关心表示的
(define (subst-v2 new old slist)
  (cases s-list slist
    (an-s-list (listof_exp)
      (map (lambda (sub_exp)   ; map也能处理null吧. 返回null, 如果是null, 待测试
             (subst-in-s-exp-v2 new old sub_exp))
           listof_exp))))

(define (subst-in-s-exp-v2 new old sub_exp)
  (cases s-exp sub_exp
    (symbol-s-exp (sym)
      (if (eqv? sym old)
          new
          sym))
    (s-list-s-exp (slst)
      (subst-v2 new old slst))))

; 貌似不能是(), todo, 这里需要处理一下, ()也当slist不就行了...
;'((b c) (b d) b)
(display "=====")
(newline)
(display "=====subst-v2")
(newline)
; 改变了内部表示
(eopl:pretty-print (subst-v2 'a 'b slist_01))


; 递归型数据结构, 自然很容易地可以由递归程序负责处理, 每一层procedure只处理一层的数据