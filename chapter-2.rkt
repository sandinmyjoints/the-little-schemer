#lang racket

(load "common.rkt")

;;; 22
(define member?
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((eq? a (car l)) #t)
     (else
      (member a (cdr l))))))

