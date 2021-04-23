(module array (lib "eopl")

  (require "store.scm")                 ; for reference?

  ; *-aux不应该export... todo
  (provide (all-defined-out))

  ; array?设计的不好 todo. 在这种表示下
  (define (array? v)  
    (reference? v))
  
  ; 返回list of ref
  ; 类似p129 Figure 4.13 representation
  (define (make-array size val_init)
    ; 可以不用list组合在一起...连续性依赖的是store产生的1, 2, 3, 4, 5这种索引数字逐个增大
    ; 像p129 Figure 4.13一样, 连续newref size_aux次即可
    (define (make-array-aux size_aux val_init_aux)
      (if (= size_aux 0)
          '()
          (cons (newref val_init_aux)
                (make-array-aux (- size_aux 1)
                                val_init_aux))))

    (let ((refs (make-array-aux size val_init)))
      (if (null? refs)
          (eopl:error 'make-array "not support len equal 0")
          (car refs))))

  ; 存在越界风险, 目前这种实现越界了依赖于store报错
  (define (arrayref-aux arr idx)
    (if (= idx 0)
        arr
        (arrayref-aux (+ arr 1) (- idx 1))))

  ; arrayref通过deref返回val
  (define (arrayref arr idx)
    (let ((ref (arrayref-aux arr idx)))
      (deref ref)))

  (define (arrayset arr idx val)
    (let ((ref (arrayref-aux arr idx)))
      (setref! ref 
               val)))

)