; 这样定义方法, 确实很秀, 在SICP 2.1.3 What Is Meant by Data 第一次碰见, 当时就觉得很惊奇
; This may seem a curiosity now, but procedural representations of data will play a central role 
; in our programming repertoire. This style of programming is often called message passing, and 
; we will be using it as a basic tool in Chapter 3 when we address the issues of modeling and simulation. (在SICP第三章大量采用了message passing方式写代码)
(define report-no-binding-found
  (lambda (search-var) (error 'apply-env "No binding for: " search-var)))

(define empty-env
 (lambda ()
  (lambda (search-var)
   (report-no-binding-found search-var))))

(define extend-env
 (lambda (saved-var saved-val saved-env)
  (lambda (search-var)
   (if (eqv? search-var saved-var)
    saved-val
    (apply-env saved-env search-var)))))

(define apply-env
 (lambda (env search-var)
  (env search-var)))