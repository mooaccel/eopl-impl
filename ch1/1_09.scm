(define remove-all-s-in-los
 (lambda (s los)
  (if (null? los)
   '()
   (if (eqv? (car los) s)
    (remove-all-s-in-los s (cdr los))
    (cons (car los) (remove-all-s-in-los s (cdr los)))))))
(remove-all-s-in-los 'a '(a b a c a d))