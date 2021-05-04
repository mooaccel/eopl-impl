#lang eopl

;(define subst 
;  (lambda (new old slist) 
;    (if (null? slist) 
;        '() 
;        (cons (subst-in-s-exp new old (car slist)) 
;              (subst new old (cdr slist))))))
;
;(define subst-in-s-exp
;  (lambda (new old sexp) 
;    (if (symbol? sexp) 
;        (if (eqv? sexp old) 
;            new 
;            sexp)
;        (subst new old sexp))))

(define subst
  (lambda (new old slist)
    (subst/k new old slist (lambda (val)
                              (eopl:printf "End of computation.~%") 
                              (eopl:printf "This sentence should appear only once.~%") 
                              val))))

(define subst/k
  (lambda (new old slist cont)
    (if (null? slist)
        (cont '())
        (subst/k new
                 old
                 (cdr slist)
                 (lambda (maybe_modify_cdr_slist)
                    (subst-in-s-exp/k new
                                      old
                                      (car slist)
                                      (lambda (maybe_modify_car_slist)
                                         (cont (cons maybe_modify_car_slist
                                                     maybe_modify_cdr_slist)))))))))

(define subst-in-s-exp/k
  (lambda (new old sexp cont)
    (if (symbol? sexp)
        (cont (if (eqv? sexp old)
                  new
                  sexp))
        (subst/k new 
                 old 
                 sexp
                 (lambda (maybe_modify_sexp)
                    (cont maybe_modify_sexp))))))


(eopl:pretty-print (subst 'a 'b '((b c) (b () d))))
(eopl:pretty-print (subst 'dd 'bb '((b ((bb (bb))) c) (a bb) (b () d))))
(eopl:pretty-print (subst 'a 'b '()))