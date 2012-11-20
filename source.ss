;;; page 10
(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

;;; page 53
(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cond
       ((eq? (car lat) a)
        (multirember a (cdr lat)))
        (else (cons (car lat)
                    (multirember a
                                 (cdr lat)))))))))

;;; page 54
(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? old (car lat))
      (cons old (cons new (cdr lat))))
     (else
      (cons (car lat) (multiinsertR new old (cdr lat)))))))

;;; page 57
(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? old (car lat))
      (cons new (multisubst new old (cdr lat))))
     (else
      (cons (car lat) (multisubst new old (cdr lat)))))))

(define add1
  (lambda (a)
    (+ a 1)))

(define sub1
  (lambda (a)
    (- a 1)))

;;; page 60
(define plus
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else
      (plus (add1 n) (sub1 m))))))
