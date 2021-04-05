(define subst
 (lambda (new old slist)
  (map (lambda (item) 
        (subst-in-s-exp new old item)) 
       slist)))
(define subst-in-s-exp
 (lambda (new old sexp)
  (if (symbol? sexp)
   (if (eqv? sexp old)
    new
    sexp)
   (subst new old sexp))))
(subst 'a 'b '((b c) (b () d)))
(subst 'a
       'b
       '((b c) (b () mo (a c ((b b) b)))))