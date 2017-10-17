 (define invert
   (lambda (lst)
     (cond
       ((null? lst)'())
       ((list? (car lst)) (cons (invert (car lst)) (invert (cdr lst))))
       (else (cons (car (cdr lst)) (car lst))))))



 (define list-set
   (lambda (lst n x)
     (cond
       ((zero? n) (cons x (cdr lst)))
     ((null? lst)'() )
     ((cons (car lst)(list-set (cdr lst) (- n 1) x))))))
 
 ;; gives contract violation for +
(define count-occurences
  (lambda (s slist)
    (cond
      ((null? slist )0)
      ((eq? s (car slist)) (+ 1 (count-occurences s (cdr slist))))
      ((list? (car slist)) (+ (count-occurences s (car slist)) (count-occurences s (cdr slist))))
       (else (+ 0 (count-occurences s (cdr slist)))))))


 
(define exists?
  (lambda (pred lst)
    (cond
      ((null? lst) #f)
      ((pred (car lst)) #t)
      (else (exists? pred (cdr lst))))))


(define list-sum
  (lambda (loi)
    (cond
      ((null? loi) 0)
      (else (+ (car loi)
               (list-sum(cdr loi)))))))
