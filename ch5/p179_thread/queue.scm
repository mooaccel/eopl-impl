(module queue (lib "eopl")

  (provide (all-defined-out))
  
  (define empty-queue
    (lambda ()
      '()))

  ; 改成empty-queue?
  (define empty? null?)

  ; 返回一个新queue
  (define enqueue
    (lambda (q val)
      (append q (list val))))

  ; 返回什么? f * *的结果
  (define dequeue
    (lambda (q f)
      (f (car q) (cdr q))))

)