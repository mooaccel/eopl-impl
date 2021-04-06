; 全部都满足, 才返回#t
(define every?
  (lambda (pred lst)
    (if (null? lst)
        #t
        (let ((car_lst (car lst)))
            (if (not (pred car_lst))
                #f
                (every? pred (cdr lst)))))))
(every? number? '(a b c 3 e))
(every? number? '(1 2 3 5 4))