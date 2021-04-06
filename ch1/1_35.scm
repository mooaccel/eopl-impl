(load "./ch1/1_31.scm")

(define (number-leaves node)
  (let ((idx -1))
    (define (aux cur_node)
      (cond ((leaf? cur_node)
              (set! idx (+ idx 1))
              idx)
          (else (let ((new_left_branch (aux (lson cur_node))))
                  (let ((new_right_branch (aux (rson cur_node))))
                    (interior-node (contents-of cur_node)
                                   new_left_branch
                                   new_right_branch))
            ))))
                ;   (interior-node (contents-of cur_node)
                ;                (aux (lson cur_node))
                ;                (aux (rson cur_node))))))  
                ;          ; 依赖于求值顺序? 必须先进入lson... 貌似在mit-scheme上运行有问题...
    (aux node)))

(number-leaves
  (interior-node 'foo 
    (interior-node 'bar 
                   (leaf 26) 
                   (leaf 12)) 
    (interior-node 'baz 
                   (leaf 11) 
                   (interior-node 'quux (leaf 117) 
                                        (leaf 14)))))

