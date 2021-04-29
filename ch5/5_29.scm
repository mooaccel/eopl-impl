#lang eopl

; 越来越像在写C/C++/甚至汇编了, 这么写的话, 本质是一样的

; A 0-argument tail call is the same as a jump.

(define cur_val 'uninitialized)
(define acc 'uninitialized)

(define fact-iter
  (lambda (n) 
    (set! cur_val n)
    (set! acc 1)
    (fact-iter-acc)))

(define fact-iter-acc
  (lambda () 
    (if (zero? cur_val)
        acc
        (begin 
          (set! acc (* cur_val acc))
          (set! cur_val (- cur_val 1))
          (fact-iter-acc)))))
          
(eopl:pretty-print (fact-iter 5))