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

(define (accumulate op initial sequence) 
  (if (null? sequence) 
      initial 
      (op (car sequence) 
          (accumulate op initial (cdr sequence)))))
(define (flatmap proc seq) 
  (accumulate append '() (map proc seq)))
(define product-v2
  (lambda (sos1 sos2)
    (flatmap (lambda (s1)
              (helper s1 sos2))
             sos1)))
(product-v2 '(a b c) '(x y))
(product-v2 '(a b c) '(1 2 3))