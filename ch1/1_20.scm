(define (accumulate op initial sequence) 
  (if (null? sequence) 
      initial 
      (op (car sequence) 
          (accumulate op initial (cdr sequence)))))

(define count-occurrences
  (lambda (s slist)
   (accumulate + 0 (map (lambda (item) (helper s item))
                    slist))))

(define helper
  (lambda (s sexp)
    (if (symbol? sexp) 
      (if (eqv? sexp s) 
          1
          0)
      (count-occurrences s sexp))))

(count-occurrences 'x '((f x) y (((x z) x))))
(count-occurrences 'x '((f x) y (((x z) () x))))
(count-occurrences 'x '((f x) y (((x z) (x (x)) x))))
(count-occurrences 'w '((f x) y (((x z) x))))