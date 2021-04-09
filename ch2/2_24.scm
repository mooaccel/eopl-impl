#lang eopl
; integer?和symbol?都是built-in的predicate
(define-datatype bintree bintree?
  (leaf-node 
    (num integer?))
  (interior-node 
    (key symbol?) 
    (left bintree?) 
    (right bintree?)))

(define (bintree-to-list bintree_node)
  (cases bintree bintree_node
         (leaf-node (num) 
            (list 'leaf-node 
                  num))
         (interior-node (key left right)
            (list 'interior-node
                  key
                  (bintree-to-list left)
                  (bintree-to-list right)))))

(eopl:pretty-print (bintree-to-list (interior-node 'a (leaf-node 3) (leaf-node 4))))

(define tree_02 (interior-node 'a
                               (interior-node 'b
                                              (leaf-node 10)
                                              (leaf-node 11))
                               (interior-node 'c
                                              (leaf-node 12)
                                              (leaf-node 13))))
(eopl:pretty-print tree_02)
(eopl:pretty-print (bintree-to-list tree_02))
; 输出:
; (interior-node
;  a
;  (interior-node b (leaf-node 10) (leaf-node 11))
;  (interior-node c (leaf-node 12) (leaf-node 13)))