(define down
 (lambda (lst)
  (map (lambda (item) (list item))
       lst)))

(down '(1 2 3))
(down '((a) (fine) (idea)))
(down '(a (more (complicated)) object))