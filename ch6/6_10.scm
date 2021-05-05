#lang eopl

; (define (end-cont)
;   0)
; 
; (define (list-sum-cont cont loi)
;   (+ cont 
;      (car loi)))
; 
; (define (apply-cont cont val)
;   (+ cont 0))
; 
; (define (list-sum loi)
;   (list-sum/k loi (end-cont)))
;                   
; 
; (define (list-sum/k loi cont)
;   (if (null? loi)
;       (apply-cont cont 0)
;       (list-sum/k (cdr loi)
;                   (list-sum-cont cont loi))))

; inline之后
(define (list-sum loi)
  (list-sum/k loi 0))
                  
(define (list-sum/k loi cont)
  (if (null? loi)
      (+ cont 0)
      (list-sum/k (cdr loi)
                  (+ cont 
                     (car loi)))))

(eopl:pretty-print (list-sum '(1 2 3 4 5)))
(eopl:pretty-print (list-sum '()))