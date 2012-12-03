#lang racket

;;; page 10
(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))
