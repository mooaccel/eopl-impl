; version 1
;(define subst
; (lambda (new old slist)
;  (if (null? slist)
;   '()
;   (let ((car_slist (car slist)))
;    (let ((new_car_slist (if (symbol? car_slist)
;                             (if (eqv? car_slist old)
;                                 new
;                                 car_slist)
;                             (subst new old car_slist))))
;   (cons new_car_slist
;            (subst new old (cdr slist))))))))
; version 2
(define subst
 (lambda (new old slist)
  (if (null? slist)
   '()
   (cons (let ((car_slist (car slist)))
          (if (symbol? car_slist)
           (if (eqv? car_slist old)
            new
            car_slist)
           (subst new old car_slist)))
         (subst new old (cdr slist))))))
(subst 'a
       'b
       '((b c) (b () mo (a c ((b b) b)))))
