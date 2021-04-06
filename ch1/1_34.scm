(load "./ch1/1_31.scm")

; 覆盖之前的定义
(define (leaf? node)
  (and (null? (lson node))
       (null? (rson node))))

; todo 待统一...
(define (val-of-leaf node)
  (car node))
(define (val-of-interior-node interior-node)
  (car interior-node))

(define (path n bst)
  (define (aux node path_have_search)
    (if (leaf? node)
        (if (not (= (val-of-leaf node) n))
            #f
            path_have_search)  ; todo可能要反序
        (let ((cur_val (val-of-interior-node node)))
          (cond ((eqv? cur_val n) 
                  path_have_search)
                ((< n cur_val) 
                  (aux (lson node) (append path_have_search (list 'left))))
                ((> n cur_val)
                  (aux (rson node) (append path_have_search (list 'right))))))))
  (let ((res_aux (aux bst '())))
    (if (not res_aux)
        '()
        res_aux)))

(val-of-leaf '(7 () ()))

(equal? (path 31 '(14 (7 () (12 () ()))
                  (26 (20 (17 () ()) ()) 
                      (31 () ()))))
        '(right right))
(equal? (path 17 '(14 (7 () (12 () ()))
                  (26 (20 (17 () ()) ()) 
                      (31 () ()))))
        '(right left left))
(equal? (path 18 '(14 (7 () (12 () ()))
                  (26 (20 (17 () ()) ()) 
                      (31 () ()))))
        '())