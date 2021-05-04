; 2 * 2 = 4种结果

;(lambda (x y) 
;  (+ (f (g x)) (h (j y))))
;  f g h j
;1.  (g x) (j y) (f ) (h )
;2.  (g x) (j y) (h ) (f )
;3.  (j y) (g x) (f ) (h )
;4.  (j y) (g x) (h ) (f )

(lambda (x y) 
  (+ (f (g x)) (h (j y))))

; 1. 
(lambda (x y cont)
  (g x (lambda (val_g_x)
          (j y (lambda (val_j_y)
                  (f val_g_x (lambda (val_f)
                                (h val_j_y (lambda (val_h) 
                                                (+ val_f val_h))))))))))

; 2.
(lambda (x y cont)
  (g x (lambda (val_g_x)
          (j y (lambda (val_j_y)
                  (h val_j_y (lambda (val_h) 
                                (f val_g_x (lambda (val_f)
                                                (+ val_f val_h))))))))))

; 3, 4略