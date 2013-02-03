;;; page 10
(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

;;;
(define member?
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((eq? a (car l)) #t)
     (else
      (member a (cdr l))))))

(define insertL
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((eq? old (car l)) (cons new (cons old (insertL new old (cdr l)))))
     (else
      (cons (car l) (insertL new old (cdr l)))))))


;;; 44
(define firsts
  (lambda (l)
    (cond
     ((null? l) (quote ()))
     (else
      (cons (car (car l)) (firsts (cdr l)))))))

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

;;;
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

;;; page 81
(define rember*
  (lambda (a l)
    (cond
     ((null? l) (quote ()))
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
     ((null? l) (quote ()))
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
     ((null? l) (quote ()))
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
     ((null? l) (quote ()))
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
     ((null? l) ((quote ())))
     ((equal? (car l) s) (cdr l))
     (else (cons (car l)
                 (rember s
                         (cdr l)))))))

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
     ((eq? (car (cdr aexp)) 'my-expt)
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
    (cons (quote ()) n)))

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
     ((null? lat) (quote ()))
     ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
     (else
      (cons (car lat) (makeset (cdr lat)))))))

;;; 112
(define multirember
  ;; Rewritten with equal?.
  (lambda (a lat)
    (cond
     ((null? lat) (quote (quote ())))
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
     ((null? lat) (quote ()))
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
    ((null? set1) (quote ()))
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
    (cons s1 (cons s2 (quote ())))))

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
     ((null? rel) (quote ()))
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

;;; Chapter 8

(define (funcall fun . args)
  ;; A little helper.
  (apply fun args))

(define rember-f
  (lambda (test? a l)
    (cond
     ((null? l) (quote ()))
     ((funcall test? a (car l)) (cdr l))
     (else
      (cons (car l) (rember-f test? a (cdr l)))))))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-salad (eq?-c 'salad))

;;; 128
(define rember-f
  ;; Rewritten
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) (quote ()))
       ((funcall test? a (car l))
        (cdr l))
       (else
        (cons (car l) ((rember-f test?) a (cdr l))))))))

;;; 130
(define insertL-f
  (lambda (test?)
    (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((funcall test? old (car l))
      (cons new (cons old ((insertL-f test?) new old (cdr l)))))
     (else
      (cons (car l) ((insertL-f test?) new old (cdr l))))))))

;;; 130
(define insertR-f
  (lambda (test?)
    (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((funcall test? old (car l))
      (cons old (cons new ((insertR-f test?) new old (cdr l)))))
     (else
      (cons (car l) ((insertR-f test?) new old (cdr l))))))))

;;; 131
(define insert-g
  (lambda (inserter)
    (lambda (new old l)
      (cond
       ((null? l) (quote ()))
       ((funcall eq? old (car l))
        (funcall inserter new old ((insert-g inserter) new old (cdr l))))
       (else
        (cons (car l) ((insert-g inserter) new old (cdr l))))))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

;;; 132
(define insertL (insert-g seqL))

(define insertR (insert-g seqR))

(define insertL
  (insert-g
   (lambda (new old l)
     (cons news (cons old (cdr l))))))

(define insertR
  (insert-g
   (lambda (new old l)
     (cons old (cons new (cdr l))))))

(define seqS
  (lambda (new old l)
    (cons new  l)))

(define subst
  (insert-g seqS))

;;; page 134
(define atom-to-function
  (lambda (a)
    (cond
     ((eq? a (quote o+)) o+)
     ((eq? a (quote x)) x)
     (else
      my-expt))))

;;; 135
(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     (else
      (funcall
       (atom-to-function (operator nexp))
       (1st-sub-exp nexp)
       (2nd-sub-exp nexp))))))

;;; 135
(define multirember-f
  (lambda (test?)    
    (lambda (a lat)
      (cond
       ((null? lat) (quote ()))
       ((test? a (car lat))        
        ((multirember-f test?) a (cdr lat)))
       (else
        (cons (car lat) ((multirember-f test?) a (cdr lat))))))))

;;; 137
(define multirember&co
  (lambda (a lat col)
    (cond
     ((null? lat)
      (col (quote ()) (quote ())))
     ((eq? (car lat) a)
      (multirember&co a
                      (cdr lat)
                      (lambda (newlat seen)
                        (col newlat
                             (cons (car lat) seen)))))
     (else
      (multirember&co a
                      (cdr lat)
                      (lambda (newlat seen)
                        (col (cons (car lat) newlat)
                             seen)))))))

;;; 138
(define a-friend
       (lambda (x y)
         (null? y)))

;;; 141
(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? oldL (car lat))
      (cons new
            (cons (car lat)
                  (multiinsertLR new oldL oldR (cdr lat)))))
     ((eq? oldR (car lat))
      (cons (car lat)
            (cons new
                  (multiinsertLR new oldL oldR (cdr lat)))))
     (else
      (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))

;;; 142
(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
     ((null? lat)
      (col (quote ()) 0 0))
     ((eq? (car lat) oldL)
      (cons new
            (cons (car lat)
                  (multiinsertLR&co new oldL oldR (cdr lat)
                                 (lambda (newlat seenL seenR)
                                   (col (cons new (cons oldL newlat)) (add1 seenL) seenR))))))
     ((eq? (car lat) oldR)
      (cons (car lat)
            (cons new
                  (multiinsertLR&co new oldL oldR (cdr lat)
                                 (lambda (newlat seenL seenR)
                                   (col (cons oldR (cons new newlat)) seenL (add1 seenR)))))))
     (else
      (cons (car lat) (multiinsertLR&co new oldL oldR (cdr lat)
                                     (lambda (newlat seenL seenR)
                                       (col (cons (car lat) newlat) seenL seenR))))))))

;;; 144
(define even?
  (lambda (n)
    (= (* (quotient n 2) 2) n)))

;; 144
(define evens-only*
  (lambda (l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((even? (car l))
        (cons (car l) (evens-only* (cdr l))))
       (else
        (evens-only* (cdr l)))))
     (else
      (cons (evens-only* (car l)) (evens-only* (cdr l)))))))

;; 145
;; p = product (evens)
;; s = sum (odds)
(define evens-only*&co
  (lambda (l col)
    (cond
     ((null? l) (col (quote ()) 1 0))
     ((atom? (car l))
      (cond
       ((even? (car l))
        (evens-only*&co (cdr l)
                        (lambda (newl p s)
                          (col (cons (car l) newl)
                               (x (car l) p) s))))
       (else
        (evens-only*&co (cdr l)
                        (lambda (newl p s)
                          (col newl p (+ (car l) s)))))))
     (else
      (evens-only*&co (car l)
                      (lambda (al ap as)
                        (evens-only*&co (cdr l)
                                        (lambda (dl dp ds)
                                          (col (cons al dl)
                                               (x ap dp)
                                               (+ as ds))))))))))

(define the-last-friend
  (lambda (newl product sum)
    (cons sum
          (cons product newl))))  
;; C-c M-e eval-def-and-go
;; C-c M-r eval-region-and-go  
