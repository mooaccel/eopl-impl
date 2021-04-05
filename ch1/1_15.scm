; duple : NonnegInt × S-list → S-list
(define duple 
 (lambda (n x)
  (if (eqv? n 0)
      '()
      (cons x (duple (- n 1) x)))))
(duple 2 3)
(duple 4 3)
(duple 4 '(ha ha))
(duple 4 '(ha (a (b) ha)))
(duple 0 '(blah))