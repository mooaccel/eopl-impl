; 这样表示一个节点?
(list num A B)
; 在num节点上面的, 放在A这个reversed list里. 也就是它的上面的节点.?

; 把num改造成num+ parent
; 不然再多一个parent field
(define (number->bintree num)
  (list num '() '()))

; 题目的意思是想搞成(list 父 子) ? 这样感觉并不方便啊
