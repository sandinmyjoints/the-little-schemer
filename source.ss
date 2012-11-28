;;; page 10
(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

;;;
(define rember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cond
       ((eq? (car lat) a) (rember a (cdr lat)))
       (else
        (cons (car lat) (rember a (cdr lat)))))))))

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


(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2))
      (= a1 a2))
     ((or (number? a1) (number? a2)) #f)
     (else
      (eq? a1 a2)))))

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
(define rempick2
  (lambda (n lat)
    (cond
     ((null? lat) lat)
     ((one? n) (cdr lat))
     (else
      (cons (car lat) (rempick2 (sub1 n) (cdr lat)))))))

;;; page 81
(define rember*
  (lambda (a l)
    (cond
     ((null? l) ())
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
     ((null? l) ())
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
     ((null? l) ())
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
     ((null? l) ())
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
(define re-eqlist?
  (lambda (l1 l2)
    (equal? l1 l2)))

;;; 94
(define simpl-rember
  (lambda (s l)
    (cond
     ((null? l) ('()))
     ((equal? (car l) s) (cdr l))
     (else (cons (car l)
                 (rember s
                         (cdr l)))))))))

;;; note: s-exp is an atom or a (possibly empty) list of s-exps.
;;; note: * functions recur on both car and cdr

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

(define simp-numbered?
  ;; `aexp` is understood to be an arithmetic expression.
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     ((and (simp-numbered? (car aexp)) (simp-numbered? (car (cdr (cdr aexp))))))
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
(define new-value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (car nexp) 'o+)       (o+ (new-value (car (cdr nexp))) (new-value (car (cdr (cdr nexp))))))
     ((eq? (car nexp) 'x)        (x (new-value (car (cdr nexp))) (new-value (car (cdr (cdr nexp))))))
     ((eq? (car nexp) 'my-expt)  (my-expt (new-value (car (cdr nexp))) (new-value (car (cdr (cdr nexp)))))))))

;;; 105
(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

