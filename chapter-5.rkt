#lang racket

(load "common.rkt")

;;; page 81
(define rember*
  (lambda (a l)
    (cond
     ((null? l) (list))
     (else
      (cond
       ((atom? (car l))
        (cond
         ((eq? (car l) a) (rember* a (cdr l)))
         (else
          (cons (car l) (rember* a (cdr l))))))
       (else
        (cons (rember* a (car l)) (rember* a (cdr l)))))))))

;;; page 82
(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) (list))
     (else
      (cond
       ((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l)))))
       (else
        (cond
         ((atom? (car l)) (cons (car l) (insertR* new old (cdr l))))
         (else
          (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))))))

;;; page 85
(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((eq? a (car l)) (add1 (occur* a (cdr l))))
       (else (occur* a (cdr l)))))
     (else
      (+ (occur* a (car l)) (occur* a (cdr l)))))))

;;; page 85
(define subst*
  (lambda (new old l)
    (cond
     ((null? l) (list))
     ((atom? (car l))
      (cond
       ((eq? old (car l)) (cons new (subst* new old (cdr l))))
       (else
        (cons (car l) (subst* new old (cdr l))))))
     (else
      (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

;;; page 86
(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) (list))
     ((atom? (car l))
      (cond
       ((eq? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
       (else
        (cons (car l) (insertL* new old (cdr l))))))
     (else
      (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

;;; page 86
(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l)) (or (eq? a (car l)) (member* a (cdr l))))
     (else
      (or (member* a (car l)) (member* a (cdr l)))))))

;;; page 88
(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else
      (leftmost (car l))))))

;;; page 91
(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else
      (cond
       ((and (atom? (car l1)) (atom? (car l2)))
        (cond
         ((eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))
         (else #f)))
       (else
        (cond
         ((or (atom? (car l1)) (atom? (car l2))) #f)
         (else
          (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))))))

;;; 92
(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2))
      (eqan? s1 s2))
     ((or (atom? s1) (atom? s2)) #f)
     (else
      (eqlist? s1 s2)))))

;;; page 93
(define eqlist?
  ;; Simplified.
  (lambda (l1 l2)
    (equal? l1 l2)))

;;; 94
(define rember
  ;; Simplified.
  (lambda (s l)
    (cond
     ((null? l) ((list)))
     ((equal? (car l) s) (cdr l))
     (else (cons (car l)
                 (rember s
                         (cdr l)))))))
