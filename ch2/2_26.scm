#lang eopl

; Red-blue-tree ::= Red-blue-subtree 
; Red-blue-subtree ::= (red-node Red-blue-subtree Red-blue-subtree) 
;                  ::= (blue-node { Red-blue-subtree }^∗ ) 
;                  ::= (leaf-node Int)

(define-datatype red-blue-tree red-blue-tree?
  (a-red-blue-tree
    (rb_subtree red-blue-subtree?)))

(define-datatype red-blue-subtree red-blue-subtree?
  (red-node 
    (rb_subtree_1 red-blue-subtree?)
    (rb_subtree_2 red-blue-subtree?))
  (blue-node
    (listof_rb_subtree (list-of red-blue-subtree?)))
  (leaf-node
    (num integer?)))

; 不像2_25, 这里的内部节点没有存储用户的符号了
; 但是内部还是有符号的, 也就是tag
; 比如:
; #(struct:red-node #(struct:leaf-node 10) #(struct:leaf-node 20))
; 用来区分各种节点

(define (list-of pred)
  (define (aux listof_val)
    (or (null? listof_val)
        (and (pair? listof_val)
             (pred (car listof_val))
             (aux (cdr listof_val)))))
  aux)

(define sub_tree_01 (red-node (leaf-node 10)
                              (leaf-node 20)))
(eopl:pretty-print sub_tree_01)

(define (mark-leaves-with-red-depth-aux rb_subtree cur_depth)
  (cases red-blue-subtree rb_subtree
    (red-node (rb_subtree_1 rb_subtree_2)
      (red-node (mark-leaves-with-red-depth-aux rb_subtree_1 (+ cur_depth 1))
                (mark-leaves-with-red-depth-aux rb_subtree_2 (+ cur_depth 1))))
    (blue-node (listof_rb_subtree)
      (blue-node (map (lambda (rb_subtree_item) 
                        (mark-leaves-with-red-depth-aux rb_subtree_item cur_depth))
                      listof_rb_subtree)))
    (leaf-node (num)
      (leaf-node cur_depth))))

(define (mark-leaves-with-red-depth rb_tree)
  (cases red-blue-tree rb_tree
    (a-red-blue-tree (rb_subtree)
      (mark-leaves-with-red-depth-aux rb_subtree 0))))

(newline)
(eopl:pretty-print (mark-leaves-with-red-depth (a-red-blue-tree
                            (red-node
                             (blue-node (list (leaf-node 26) 
                                              (leaf-node 12)))
                             (red-node (leaf-node 11) 
                                       (blue-node (list (leaf-node 117) 
                                                        (leaf-node 14))))))))