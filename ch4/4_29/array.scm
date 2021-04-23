(module array (lib "eopl")

  (require "store.scm")                 ; for reference?

  ; *-aux不应该export... todo
  (provide (all-defined-out))

  (define-datatype array array?
    (an-array 
      (refs (list-of reference?))))
  
  (define (make-array size val_init) 
    ; 返回list of ref
    (define (make-array-aux size_aux val_init_aux)
      (if (= size_aux 0)
          '()
          (cons (newref val_init_aux)
                (make-array-aux (- size_aux 1)
                                val_init_aux))))
    (an-array (make-array-aux size val_init)))

  (define (arrayref-aux listof_ref idx_aux)
    (if (null? listof_ref)
        (eopl:error 'arrayref "index out of border idx = ~s" idx_aux)
        (if (= idx_aux 0)
            (car listof_ref)
            (arrayref-aux (cdr listof_ref) (- idx_aux 1)))))

  ; deref返回val
  (define (arrayref arr idx)
    (cases array arr
      (an-array (refs)
        (deref (arrayref-aux refs idx)))))

  (define (arrayset arr idx val)
    (cases array arr
      (an-array (refs)
        (let ((ref (arrayref-aux refs idx)))
          (setref! ref 
                   val)))))

)