; 第一个满足pred的索引.
(define (list-index pred lst) 
  (define (list-index-aux lst_aux idx)
    (if (null? lst_aux)
        #f
        (let ((car_lst (car lst_aux)))
            (if (pred car_lst)
                idx
                (list-index-aux (cdr lst_aux) (+ idx 1))))))
  (list-index-aux lst 0))
(equal? (list-index number? '(a 2 (1 3) b 7))
        1)
(list-index symbol? '(a (b c) 17 foo))
(list-index symbol? '(1 2 (a b) 3))