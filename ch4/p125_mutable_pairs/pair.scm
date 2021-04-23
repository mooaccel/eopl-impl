(module pair (lib "eopl")

  (require "store.scm")                 ; for reference?

  (provide (all-defined-out))

  (define-datatype mutpair mutpair?
    (a-pair (left-loc reference?) 
            (right-loc reference?)))
  
  (define make-pair
    (lambda (val1 val2) 
      (a-pair (newref val1) 
              (newref val2))))
  
  ; 输入mutpair
  (define left
    (lambda (p) 
      (cases mutpair p 
        (a-pair (left-loc right-loc) 
          (deref left-loc)))))
  
  (define right
    (lambda (p) 
      (cases mutpair p 
        (a-pair (left-loc right-loc) 
          (deref right-loc)))))
  
  (define setleft
    (lambda (p val) 
      (cases mutpair p 
        (a-pair (left-loc right-loc) 
          (setref! left-loc val)))))
  
  (define setright 
    (lambda (p val) 
      (cases mutpair p 
        (a-pair (left-loc right-loc) 
          (setref! right-loc val)))))

)