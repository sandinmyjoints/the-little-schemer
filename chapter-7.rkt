#lang racket

(load "common.rkt")

;;; note: s-exp is an atom or a (possibly empty) list of s-exps.
;;; note: * functions recur on both car and cdr

;;; 111
(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat) (cdr lat)) #f)
     (else
       (set? (cdr lat))))))

;;; 112
(define member?
  ;; Rewritten with equal?.
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((equal? a (car l)) #t)
     (else
      (member a (cdr l))))))

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) (list))
     ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
     (else
      (cons (car lat) (makeset (cdr lat)))))))

;;; 112
(define multirember
  ;; Rewritten with equal?.
  (lambda (a lat)
    (cond
     ((null? lat) (list))
     (else
      (cond
       ((equal? (car lat) a)
        (multirember a (cdr lat)))
        (else (cons (car lat)
                    (multirember a
                                 (cdr lat)))))))))

(define makeset
  ;; Using multirember.
  (lambda (lat)
    (cond
     ((null? lat) (list))
     (makeset (cons (car lat) (multirember (car lat) (cdr lat)))))))

;;;
(define subset?
  ;; My initial version.
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     ((null? set2) #f)
     (else
      ;; remove each of set2 from set1
      (subset? (multirember (car set2) set1) (cdr set2))))))

;;;
(define subset?
  ;; TLS's version.
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     ((member? (car set1) set2) (subset? (cdr set1) set2))
     (else #f))))

;;;
(define subset?
  ;; Using and.
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else
      (and (member? (car set1) set2) (subset? (cdr set1) set2))))))

;;;
(define eqset?
  ;; My initial eqset?
  (lambda (set1 set2)
    (cond
     ((and (null? set1) (null? set2)) #t)
     ((or (null? set1) (null? set2)) #f)
     (else
      (eqset? (multirember (car set1) set1) (multirember (car set1) set2))))))

;;;
(define eqset?
  ;; TLS's eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

;;;
(define intersect?
  ;; My initial intersect?
  (lambda (set1 set2)
    (cond
     ((or (null? set1) (null? set2)) #f)
     (else
      (or (subset? set1 set2)
          (intersect? (cdr set1) set2))))))

;;; 116
(define intersect
 (lambda (set1 set2)
   (cond
    ((null? set1) (list))
    (else
     (cond
      ((member? (car set1) set2)
       (cons (car set1) (intersect (cdr set1) set2)))
      (else
       (intersect (cdr set1) set2)))))))

;;; 116
(define union
  ;; Mine.
  (lambda (set1 set2)
    (cond
     ((null? set1) (makeset set2))
     ((null? set2) (makeset set1))
     (else
      (makeset (cons (car set1) (cons (car set2) (union (cdr set1) (cdr set2)))))))))

(define union
  ;; TLS's.
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2)
      (union (cdr set1) set2))
     (else
      (cons (car set1) (union (cdr set1) set2))))))

;;; 117 
(define intersectall
  (lambda (l-set)
    (cond
     ((null? (cdr l-set)) (car l-set))
     (else
      (intersect (car l-set) (intersectall (cdr l-set)))))))

;;;
(define a-pair?
  (lambda (x)
    (null? (cdr (cdr x)))))

;;; 119
(define first
  (lambda (p)
    (car p)))

;;; 119
(define second
  (lambda (p)
    (car (cdr p))))

;;; 119
(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (list)))))

;;; 119
(define third
  (lambda (p)
    (car (cdr (cdr p)))))

;;; 120
(define fun?
  (lambda (rel)
    (set? (firsts rel))))

;;; 120
(define revrel
  (lambda (rel)
    (cond
     ((null? rel) (list))
     (else
      (cons
       (build (second (car rel)) (first (car rel)))
       (revrel (cdr rel)))))))


;;; 122
(define fullfun?
  ;; My fullfun?
  (lambda (fun)
    (and (fun? fun) (fun? (revrel fun)))))

;;; 122
(define fullfun?
  ;; TLS's
  (lambda (fun)
    (fun? (revrel fun))))
