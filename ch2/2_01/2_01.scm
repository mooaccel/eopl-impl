; 这样不行.
; (define N 16)

(define zero
 (lambda ()
  '()))

(define is-zero?
 (lambda (n)
  (null? n)))

; 高位右边可能会延长, 比如从(15, 15, 15) -> (0, 0, 0, 1)
(define successor
 (lambda (n)
  (if (is-zero? n)
      (cons 1 '())
      (let ((car_bigit (car n))
            (cdr_bigit (cdr n)))
              (let ((cur_bigit (+ car_bigit 1)))
                  (if (< cur_bigit 16)
                      (cons cur_bigit cdr_bigit)
                      (cons 0 (successor cdr_bigit)))
                )
              ; 这么写存在bug, why
              ;(if (< (+ car_bigit 1) N)
            ))))

; 最后一个不能是0
(define predecessor
 (lambda (n)
  (let ((car_bigit (car n))
        (cdr_bigit (cdr n)))
    (let ((cur_bigit (- car_bigit 1)))
      (if (null? cdr_bigit)
          (if (= cur_bigit 0)
              '()
              (cons cur_bigit '()))
          (if (not (= cur_bigit -1))
              (cons cur_bigit cdr_bigit)
              (cons 15 (predecessor cdr_bigit))))))))

; for debug
(define (translate-to-scheme-number n)
  (define (aux lo_reprentation base)
    (if (null? lo_reprentation)
        0
        (+ (* base (car lo_reprentation))
           (aux (cdr lo_reprentation) (* base 16)))))
  (aux n 1))

(define zero_instance (zero))
zero_instance
(is-zero? zero_instance)
(successor zero_instance)
(successor (successor zero_instance))
(define three (successor (successor (successor zero_instance))))
three
(define val15 (list 15))
(successor val15)
(define val15_15_15 (list 15 15 15))
(successor val15_15_15)
(equal? (translate-to-scheme-number (list 2 0 1)) 258)
(equal? (translate-to-scheme-number val15_15_15) 4095)
(equal? (translate-to-scheme-number (successor val15_15_15)) 4096)
(equal? (predecessor (cons 8 '())) '(7))
(equal? (predecessor '(0 0 0 1)) '(15 15 15))
(equal? (predecessor '(0 1 0 1)) '(15 0 0 1))
(equal? (predecessor '(1 10 0 1)) '(0 10 0 1))
(equal? (predecessor '(1)) '())
(equal? (predecessor '(0 1)) '(15))
(equal? (translate-to-scheme-number (predecessor '(0 0 0 1))) 4095)

; 好问题, predecessor和successor都需要寻找子问题, 并考虑corner case


; todo 2_01 参考https://github.com/chenyukang/eopl/blob/master/ch2/01.scm, 待重新实现, 主要是mult操作, 还有根据mult实现fact