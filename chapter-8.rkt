#lang racket/load

(load "/Users/wbert/scm/the-little-schemer/common.rkt")

;;; note: s-exp is an atom or a (possibly empty) list of s-exps.

(define (funcall fun . args)
  ;; (A little helper for Scheme.)
  (apply fun args))

(define rember-f
  (lambda (test? a l)
    (cond
     ((null? l) (list))
     ((funcall test? a (car l)) (cdr l))
     (else
      (cons (car l) (rember-f test? a (cdr l)))))))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-salad (eq?-c 'salad))

;;; 128
(define rember-f-1
  ;; Rewritten
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) (list))
       ((funcall test? a (car l))
        (cdr l))
       (else
        (cons (car l) ((rember-f-1 test?) a (cdr l))))))))

;;; 130
(define insertL-f
  (lambda (test?)
    (lambda (new old l)
    (cond
     ((null? l) (list))
     ((funcall test? old (car l))
      (cons new (cons old ((insertL-f test?) new old (cdr l)))))
     (else
      (cons (car l) ((insertL-f test?) new old (cdr l))))))))

;;;
(define insertR-f
  (lambda (test?)
    (lambda (new old l)
    (cond
     ((null? l) (list))
     ((funcall test? old (car l))
      (cons old (cons new ((insertR-f test?) new old (cdr l)))))
     (else
      (cons (car l) ((insertR-f test?) new old (cdr l))))))))
