#lang racket/load

(load "common.rkt")

;;; 34
(define rember
  (lambda (a lat)
    (cond
     ((null? lat) (list))
     (else
      (cond
       ((eq? (car lat) a) (rember a (cdr lat)))
       (else
        (cons (car lat) (rember a (cdr lat)))))))))

;;; 44
(define firsts
  (lambda (l)
    (cond
     ((null? l) (list))
     (else
      (cons (car (car l)) (firsts (cdr l)))))))

;;; 51
(define insertL
  (lambda (new old l)
    (cond
     ((null? l) (list))
     ((eq? old (car l)) (cons new (cons old (insertL new old (cdr l)))))
     (else
      (cons (car l) (insertL new old (cdr l)))))))

;;; page 53
(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) (list))
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
     ((null? lat) (list))
     ((eq? old (car lat))
      (cons old (cons new (cdr lat))))
     (else
      (cons (car lat) (multiinsertR new old (cdr lat)))))))

;;; page 57
(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) (list))
     ((eq? old (car lat))
      (cons new (multisubst new old (cdr lat))))
     (else
      (cons (car lat) (multisubst new old (cdr lat)))))))
