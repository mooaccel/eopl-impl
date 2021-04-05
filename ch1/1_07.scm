(define nth-element
 (lambda (lst nth)
  (let ((ans (nth-element-rec lst nth)))
   (if (not ans)
    (report-list-too-short lst nth)
    ans))))
(define nth-element-rec
 (lambda (lst nth)
  (if (null? lst)
   #f
   (if (zero? nth)
    (car lst)
    (nth-element-rec (cdr lst) (- nth 1))))))
(define report-list-too-short
 (lambda (lst nth)
  (error 'nth-element "~s does not have ~s elements. ~%" lst nth)))
(nth-element '(a b c d e) 3)
(nth-element '(a b c d e) 4)
(nth-element '(a b c d e) 5)
