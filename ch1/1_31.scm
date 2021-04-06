; Definition 1.1.7 (binary tree)
;   Bintree ::= Int | (Symbol Bintree Bintree)

(define interior-node
	(lambda (content lnode rnode)
		(list content lnode rnode)))

(define leaf
	(lambda (content)
		content))

(define leaf?
  (lambda (node) (not (list? node))))

; only work in interior node
(define lson
  (lambda (node) (cadr node)))
(define rson
  (lambda (node) (caddr node)))
(define (contents-of node)
  (if (leaf? node)
      node
      (car node)))

(define leaf_node_1 (leaf 2))
(define leaf_node_2 (leaf 4))
(define interior_node_1 (interior-node 'a leaf_node_1 leaf_node_2))
(leaf? leaf_node_1)
(leaf? leaf_node_2)
(leaf? interior_node_1)
(contents-of leaf_node_1)
(contents-of leaf_node_2)
(contents-of interior_node_1)
(lson interior_node_1)
(rson interior_node_1)