; 这不就是built-in的filter吗...
(define filter-in
  (lambda (pred lst)
    (if (null? lst)
        '()
        (let ((car_lst (car lst)))
          (if (pred car_lst)
              (cons car_lst (filter-in pred (cdr lst)))
              (filter-in pred (cdr lst)))))))
(filter-in number? '(a 2 (1 3) b 7))
(filter-in symbol? '(a (b c) 17 foo))