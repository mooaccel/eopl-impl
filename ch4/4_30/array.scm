(module array (lib "eopl")

  (require "store.scm")                 ; for reference?

  ; *-aux不应该export... todo
  (provide (all-defined-out))
  ; 提供arrays?, make-array, arrayref, arrayset

  ; array?设计的不好 todo. 在这种表示下
  ; 怎么才能独一的表示呢? 只想到了加symbol tag...
  (define (array? v)  
    (and (not (null? v))
         (list? (car v))
         (number? (cdr v))))
  
  (define (an-array refs size)
    (cons refs size))
  
  (define (make-array size val_init)
    (define (make-array-aux size_aux val_init_aux)
      (if (= size_aux 0)
          '()
          (cons (newref val_init_aux)
                (make-array-aux (- size_aux 1)
                                val_init_aux))))

    (let ((refs (make-array-aux size val_init)))
      (if (null? refs)
          (eopl:error 'make-array "not support len equal 0")
          (an-array refs size))))

  (define (arrayref-aux refs idx)
    (if (null? refs)
        (eopl:error 'arrayref-aux "array out of bound")
        (if (= idx 0)
            (car refs)
            (arrayref-aux (cdr refs) (- idx 1)))))

  ; arrayref通过deref返回val
  (define (arrayref arr idx)
    (let ((ref (arrayref-aux (car arr) idx)))
      (deref ref)))

  (define (arrayset arr idx val)
    (let ((ref (arrayref-aux (car arr) idx)))
      (setref! ref 
               val)))

  (define (arraylength arr)
    (cdr arr))

)