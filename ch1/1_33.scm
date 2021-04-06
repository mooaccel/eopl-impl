(load "./ch1/1_31.scm")

; 返回一棵新树, 带上red depth                                           
; 从root到leaf经过了几个red
(define (mark-leaves-with-red-depth node)
  (define (aux cur_node cur_depth)
    (if (leaf? cur_node)
        (leaf cur_depth)
        (let ((delta (if (eqv? 'red (car cur_node))
                         1
                         0)))
          (interior-node (car cur_node) 
                         (aux (lson cur_node) (+ cur_depth delta))
                         (aux (rson cur_node) (+ cur_depth delta))))))
  (aux node 0))

(mark-leaves-with-red-depth
 (interior-node 'red
  (interior-node 'bar (leaf 26) 
                      (leaf 12))
  (interior-node 'red (leaf 11) 
                      (interior-node 'quux (leaf 117) 
                                           (leaf 14)))))

(mark-leaves-with-red-depth
 (interior-node 'red
  (interior-node 'bar (leaf 26) 
                      (leaf 12))
  (interior-node 'red (leaf 11) 
                      (interior-node 'red (leaf 117) 
                                           (leaf 14)))))
