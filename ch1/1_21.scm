(define product 
  (lambda (sos1 sos2)
    (if (null? sos1)
        '()
        (append (helper (car sos1) sos2)
                (product (cdr sos1) sos2)))))
(define helper
  (lambda (s1 sos2)
    (map (lambda (s2) (list s1 s2))
         sos2)))
(product '(a b c) '(x y))
(product '(a b c) '(1 2 3))