#lang eopl

(define-datatype expval expval?
  (num-val 
    (num number?)) 
;  (bool-val 
;    (bool boolean?))
  (cons-val
    (car_val expval?)
    (cdr_val expval?))
  (cons-emptylist-val)
)

(define (list-val expvals)
  (if (null? expvals)
      (cons-emptylist-val)
      (cons-val (car expvals)
                (list-val (cdr expvals)))))

(define expval->num
  (lambda (val) 
    (cases expval val 
      (num-val (num) 
        num) 
      (else 
        (eopl:error 'num "expval-num need num-val")))))

; (define expval->bool
;   (lambda (val) 
;     (cases expval val 
;       (bool-val (bool) 
;         bool) 
;       (else 
;         (eopl:error 'bool "expval-bool need bool-val")))))            

; 输出还是expval
(define expval_cons->car
  (lambda (val) 
    (cases expval val 
      (cons-val (car_val cdr_val) 
        car_val) 
      (else 
        (eopl:error 'bool "expval_cons->car need cons-val")))))            

(define expval_cons->cdr
  (lambda (val) 
    (cases expval val 
      (cons-val (car_val cdr_val) 
        cdr_val) 
      (else 
        (eopl:error 'bool "expval_cons->cdr need cons-val")))))            

(define expval_cons->nulllist?
  (lambda (val) 
    (cases expval val 
      (cons-emptylist-val ()
        (num-val 1))
      (else 
        (num-val 0)))))

(provide num-val)
;(provide bool-val)
(provide cons-val)
(provide cons-emptylist-val)

(provide expval->num)
; (provide expval->bool)
(provide expval_cons->car)
(provide expval_cons->cdr)

(provide expval_cons->nulllist?)

(provide list-val)


; ========= test
; (eopl:pretty-print (cons-emptylist-val))
; (eopl:pretty-print (expval_cons->nulllist?
;                    (cons-val (cons-emptylist-val) 
;                              (cons-emptylist-val)))
;                    )
; (eopl:pretty-print (expval_cons->nulllist?
;                    (cons-val (cons-emptylist-val) 
;                              (cons-emptylist-val)))
;                    )
; (eopl:pretty-print (expval_cons->nulllist?
;                       (cons-emptylist-val))
;                       )