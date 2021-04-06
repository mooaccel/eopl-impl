  (define swapper
    (lambda (s1 s2 slist)
     (map (lambda (item) (swapper-helper s1 s2 item))
          slist)))

  (define swapper-helper
    (lambda (s1 s2 sexp)
      (if (symbol? sexp) 
        (cond ((eqv? sexp s1) s2)
              ((eqv? sexp s2) s1)
              (else sexp))
        (swapper s1 s2 sexp))))
(equal? (swapper 'a 'd '(a b c d)) '(d b c a))
(equal? (swapper 'a 'd '(a d () c d)) '(d a () c a))
(equal? (swapper 'x 'y '((x) y (z (x)))) '((y) x (z (y))))