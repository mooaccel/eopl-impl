(load "./ch2/2_05.scm")

; 这种写法apply-env也要改...
(define (extend-env* lst1 lst2 env)
  (cons (list lst1 lst2)
        env))

(define env_2_10_test_01 (extend-env 'd 
                      6 
                      (extend-env 'y 
                                  8 
                                  (extend-env 'x 
                                              7 
                                              (extend-env 'y 
                                                          14 
                                                          (empty-env))))))
(extend-env* '(h j k) '(100 200 300) env_2_10_test_01)

; 除非写成https://github.com/chenyukang/eopl/blob/master/ch2/10.scm这样, 一层套一层的方式