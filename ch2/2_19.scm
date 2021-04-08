(define (number->bintree num)
  (list num '() '()))
(define (current-element bintree)
  (if (null? bintree)
      #f  ; todo 改成error
      (car bintree)))
(define (move-to-left-son bintree)
  (if (null? bintree)
      #f
      (cadr bintree)))
(define (move-to-right-son bintree)
  (if (null? bintree)
      #f
      (car (cddr bintree))))
(define (at-leaf? bintree)
  (null? bintree))

(define (insert-to-left num bintree)
  (define (insert-to-left-aux pre_left_br new_left_node)
    (list (current-element new_left_node)
          pre_left_br
          (move-to-right-son new_left_node))) ; 其实是'()
  (if (null? bintree)
      #f
      (let ((cur_elem (current-element bintree))
            (pre_left_br (move-to-left-son bintree))
            (pre_right_br (move-to-right-son bintree)))
          (if (null? pre_left_br)
              (list cur_elem 
                    (number->bintree num)
                    pre_right_br)
              (list cur_elem
                    (insert-to-left-aux pre_left_br (number->bintree num))
                    pre_right_br)))))

(define (insert-to-right num bintree)
  (define (insert-to-right-aux pre_right_br new_right_node)
    (list (current-element new_right_node)
          (move-to-left-son new_right_node) ; 其实是'()
          pre_right_br))
  (if (null? bintree)
      #f
      (let ((cur_elem (current-element bintree))
            (pre_left_br (move-to-left-son bintree))
            (pre_right_br (move-to-right-son bintree)))
          (if (null? pre_right_br)
              (list cur_elem 
                    pre_left_br 
                    (number->bintree num))
              (list cur_elem
                    pre_left_br 
                    (insert-to-right-aux pre_right_br (number->bintree num)))))))

(number->bintree 13)
(equal? (at-leaf? (number->bintree 13)) #f)
(equal? (at-leaf? (move-to-left-son (number->bintree 13))) #t)
(equal? (at-leaf? (move-to-right-son (number->bintree 13))) #t)

(define t1 (insert-to-left 12 (number->bintree 13)))
(equal? t1 '(13 (12 () ()) 
                ()))
(equal? (insert-to-left 15 t1) '(13 (15 (12 () ()) 
                                        ()) 
                                    ()))

(define t2 (insert-to-right 14 (insert-to-left 12 (number->bintree 13))))
(equal? t2 '(13 (12 () ()) 
                (14 () ())))
(equal? (insert-to-right 100 t2) '(13 (12 () ()) 
                                      (100 () 
                                           (14 () ()))))
(equal? (insert-to-right 200 (insert-to-right 100 t2)) '(13 (12 () ()) 
                                                            (200 ()
                                                              (100 () 
                                                                  (14 () ())))))