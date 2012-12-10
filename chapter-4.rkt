#lang racket/load

(load "common.rkt")

;;;
(define add1
  (lambda (a)
    (+ a 1)))

;;;
(define sub1
  (lambda (a)
    (- a 1)))

;;; page 60
(define o+
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else
      (o+ (add1 n) (sub1 m))))))

;;; page 64
(define addtup
  (lambda (tup)
  (cond
   ((null? tup) 0)
   (else
    (o+ (car tup) (addtup (cdr tup)))))))

;;; page 65
(define x
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else
      (o+ n (x n (sub1 m)))))))

;;; page 69
(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else
      (cons
       (o+ (car tup1) (car tup2))
       (tup+ (cdr tup1) (cdr tup2)))))))

;;; page 72
(define >
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else
      (> (sub1 n) (sub1 m))))))

;;; page 73
(define <
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else
      (< (sub1 n) (sub1 m))))))

;;; page 74
(define =
  (lambda (n m)
    (cond
     ((< n m) #f)
     ((> n m) #f)
     (else #t))))

;;; page 74
(define my-expt
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else
      (x n (my-expt n (sub1 m)))))))

;;; page 76
(define length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else
      (o+ 1 (length (cdr lat)))))))

;;;
(define pick
  (lambda (n lat)
    (cond
     ((null? lat) lat)
     ((zero? n) (car lat))
     (else
      (pick (sub1 n) (cdr lat))))))

;;; page 77
(define rempick
  (lambda (n lat)
    (cond
     ((null? lat) lat)
     ((zero? (sub1 n)) (cdr lat))
     (else
      (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

;;;
(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) lat)
     (else
      (cond
       ((number? (car lat)) (no-nums (cdr lat)))
       (else
        (cons (car lat) (no-nums (cdr lat)))))))))

;;; page 78
(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) lat)
    (else
     (cond
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else
       (all-nums (cdr lat))))))))

;;;
(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2))
      (= a1 a2))
     ((or (number? a1) (number? a2)) #f)
     (else
      (eq? a1 a2)))))

;;; 78
(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     (else
      (cond
       ((eqan? (car lat) a) (add1 (occur a (cdr lat))))
       (else
        (occur a (cdr lat))))))))

;;; page 79
(define one?
  (lambda (a)
    (= a 1)))

;;; page 79
(define rempick
  ;; Simplified.
  (lambda (n lat)
    (cond
     ((null? lat) lat)
     ((one? n) (cdr lat))
     (else
      (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

