; down加括号
; up去括号
(define (up lst)
  (if (null? lst)
      '()
      (let ((car_lst (car lst)))
        (if (list? car_lst)
            (append car_lst (up (cdr lst)))
            (cons car_lst (up (cdr lst)))))))

(up '((1 2) (3 4)))
(up '((x (y)) z))
; down
(load "./ch1/1_17.scm")
; down再up, 等于原先的
(up (down '((1 2) (3 4))))
; 不一定等于原来的了...
(down (up '((1 2) (3 4))))