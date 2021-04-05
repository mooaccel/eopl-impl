(define subst
 (lambda (new old slist)
  (if (null? slist)
   '()
   (cons (subst-in-s-exp new old (car slist)) (subst new old (cdr slist))))))
(define subst-in-s-exp
 (lambda (new old sexp)
  (if (symbol? sexp)
   (if (eqv? sexp old)
    new
    sexp)
   (subst new old sexp))))
(subst 'a 'b '((b c) (b () d)))