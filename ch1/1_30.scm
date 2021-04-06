(define (insert pred elem sorted_list)
  (if (null? sorted_list)
      (cons elem '())
      (let ((car_sorted_list (car sorted_list)))
        (if (pred elem car_sorted_list)
            (cons elem sorted_list)
            (cons car_sorted_list (insert pred elem (cdr sorted_list)))))))

(define (sort/predicate pred loi)
  (define (sort-aux sorted_list remaining)
    (if (null? remaining)
        sorted_list
        (sort-aux (insert pred (car remaining) sorted_list) (cdr remaining))))
  (sort-aux '() loi))

(sort/predicate < '(8 2 5 2 3))
(sort/predicate > '(8 2 5 2 3))