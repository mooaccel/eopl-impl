;(define number-elements-from
; (lambda (lst n)
;  (if (null? lst)
;   '()
;   (cons (list n (car lst)) (number-elements-from (cdr lst) (+ n 1))))))
;(define number-elements-p23
; (lambda (lst)
;  (number-elements-from lst 0)))
;
;(number-elements-p23 '(a b c d e))

; ===================
(define (add-one-to-remaining lst)
  (map (lambda (item) (cons (+ (car item) 1)
                            (cdr item))) 
       lst))
;(add-one-to-remaining '((100 a) (2 b) (3 c)))
;(add-one-to-remaining (list (list 0 'b) (list 1 'c) (list 2 'd)))

; 后面的全部加1
(define (g node remaining_lst)
  (if (null? remaining_lst)
      (cons node '())
      (cons node 
            (add-one-to-remaining remaining_lst))))
;(g '(0 a) '((0 b) (1 c) (2 d)))

(define number-elements
 (lambda (lst)
  (if (null? lst)
   '()
   (g (list 0 (car lst)) 
      (number-elements (cdr lst))))))
; 
(number-elements '(a b c d)) 
(number-elements '(az by cx dw ev)) 
