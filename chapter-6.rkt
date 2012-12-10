#lang racket/load

(load "/Users/wbert/scm/the-little-schemer/common.rkt")

;;; 101
(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     ((eq? (car (cdr aexp)) '+)
      (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) 'x)
      (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) 'up-arrow)
      (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
     (else #f))))

(define numbered?
  ;; Simplified.
  ;; `aexp` is understood to be an arithmetic expression.
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     ((and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
     (else #f))))

;;; 103
(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (car (cdr nexp)) 'o+)       (o+ (value (car nexp)) (value (car (cdr (cdr nexp))))))
     ((eq? (car (cdr nexp)) 'x)        (x (value (car nexp)) (value (car (cdr (cdr nexp))))))
     ((eq? (car (cdr nexp)) 'my-expt)  (my-expt (value (car nexp)) (value (car (cdr (cdr nexp)))))))))

;;; 104
(define value
  ;; Rewritten.
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (car nexp) 'o+)       (o+ (value (car (cdr nexp))) (value (car (cdr (cdr nexp))))))
     ((eq? (car nexp) 'x)        (x (value (car (cdr nexp))) (value (car (cdr (cdr nexp))))))
     ((eq? (car nexp) 'my-expt)  (my-expt (value (car (cdr nexp))) (value (car (cdr (cdr nexp)))))))))

;;; 105
(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

;;; 106
(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

;;; 106
(define operator
  (lambda (aexp)
    (car aexp)))

;;; 106
(define value
  ;; Rewritten.
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp) 'o+)       (o+ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
     ((eq? (operator nexp) 'x)        (x (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
     ((eq? (operator nexp) 'my-expt)  (my-expt (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))))))

;;; 108
(define sero?
  (lambda (n)
    (null? n)))

;;; 108
(define edd1
  (lambda (n)
    (cons (list) n)))

;;; 108
(define zub1
  (lambda (n)
    (cdr n)))

;;; 108
(define zo+
  (lambda (n m)
    (cond
     ((sero? m) n)
     (else
      (zo+ (edd1 n) (zub1 m))))))

