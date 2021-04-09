#lang eopl

(define-datatype bintree bintree?
  (leaf-node 
    (num integer?))
  (interior-node 
    (key symbol?) 
    (left bintree?) 
    (right bintree?)))

(define tree-1 (interior-node 'foo 
                              (leaf-node 2) 
                              (leaf-node 3)))

(define tree-2 (interior-node 'bar 
                              (leaf-node -1) 
                              tree-1))

(define tree-3 (interior-node 'baz 
                              tree-2 
                              (leaf-node 1)))

(define (max-interior bintree_node)
  (let ((pre_max_sum 'invalid_val)
        (pre_sym 'invalid_val))
    (define (sum-of-node-aux bintree_node)
      (cases bintree bintree_node
        (leaf-node (num)
          num)
        (interior-node (key left right)
          (let ((left_sum (sum-of-node-aux left))
                (right_sum (sum-of-node-aux right)))
            (let ((cur_sum (+ left_sum right_sum)))
              (if (or (eqv? pre_max_sum 'invalid_val)
                      (> cur_sum pre_max_sum))
                  (begin (set! pre_max_sum cur_sum)
                         (set! pre_sym key)
                         cur_sum)
                  cur_sum)
              )))))
    (begin 
      (sum-of-node-aux bintree_node)
      pre_sym
    )))

(display (equal? (max-interior tree-2) 'foo))
(display (equal? (max-interior tree-3) 'foo))
(newline)
; 如果改成
;                      (>= cur_sum pre_max_sum)
; 那么(max-interior tree-3)返回'baz