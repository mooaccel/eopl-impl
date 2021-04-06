; 任何一个满足, 就返回#t
(define (exists? pred lst)
  (define (exists-aux? pred_aux lst_aux)
    (if (null? lst_aux)
        #f
        (let ((car_lst (car lst_aux)))
            (if (pred_aux car_lst)
                #t
                (exists? pred_aux (cdr lst_aux))))))
  (if (null? lst)
      #f
      (exists-aux? pred lst)))
(exists? number? '())
(exists? number? '(a b c 3 e))
(exists? number? '(1 2 3 5 4))
(exists? number? '(a b c d e))