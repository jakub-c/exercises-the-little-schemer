#lang racket

(require rackunit)
(require "00-preface.rkt")
(require "04.rkt")

; Atom S-expression -> S-expression
; (define (rember* a l) '()) ;stub

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((list? (car l)) (cons
                        (rember* a (car l))
                        (rember* a (cdr l))))
      (else
       (cond
         ((eq? a (car l)) (rember* a (cdr l)))
         (else
          (cons (car l) (rember* a (cdr l)))))))))

(check-equal? (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
              '((coffee) ((tea)) (and (hick))))

(check-equal? (rember* 'sauce '(((tomato sauce)) ((bean) sauce)
                                                 (and ((flying)) sauce)))
              '(((tomato)) ((bean))
                           (and ((flying)))))

; Atom Atom S-expression -> S-expression
; (define (insertR* new old l) '()) ;stub

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond ((eq? old (car l))
              (cons old
                    (cons new
                          (insertR* new old (cdr l)))))
             (else
              (cons (car l)
                    (insertR* new old (cdr l))))))
      (else
       (cons (insertR* new old (car l))
             (insertR* new old (cdr l)))))))

(check-equal? (insertR* 'roast 'chuck
                        '((how much (wood))
                          could
                          ((a (wood) chuck))
                          (((chuck)))
                          (if (a) ((wood chuck)))
                          could chuck wood))
              '((how much (wood))
                could
                ((a (wood) chuck roast)) (((chuck roast)))
                (if (a) ((wood chuck roast))) could chuck roast wood))


; Atom S-expression -> Number
; count the occurences of an atom
; (define (occur* a l) 0) ;stub

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? a (car l)) (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else
       (+
        (occur* a (car l))
        (occur* a (cdr l)))))))

(check-eq? (occur* 'banana '((banana)
                             (split ((((banana ice)))
                                     (cream (banana))
                                     sherbet))
                             (banana)
                             (bread)
                             (banana brandy)))
           5)

; Atom Atom S-expression -> S-expression
; replace the occurance of the old atom with the new
; (define (subst* new old l) '()) ;stub

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? old (car l))
          (cons new
                (subst* new old (cdr l))))
         (else
          (cons (car l)
                (subst* new old (cdr l))))))
      (else
       (cons (subst* new old (car l))
             (subst* new old (cdr l)))))))

(check-equal? (subst* 'orange 'banana '((banana)
                                        (split ((((banana ice)))
                                                (cream (banana))
                                                sherbet))
                                        (banana)
                                        (bread)
                                        (banana brandy)))
              '((orange)
                (split ((((orange ice)))
                        (cream (orange))
                        sherbet))
                (orange)
                (bread)
                (orange brandy)))

; Atom Atom S-expression -> S-expression
; insert the new atom to the left of the old one
; (define (insertL* new old l) '()) ;stub

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? old (car l))
          (cons new
                (cons old
                      (insertL* new old (cdr l)))))
         (else
          (cons (car l)
                (insertL* new old (cdr l))))))
      (else
       (cons (insertL* new old (car l))
             (insertL* new old (cdr l)))))))

(check-equal? (insertL* 'peeker 'chuck '((how much (wood))
                                         could
                                         ((a (wood) chuck))
                                         (((chuck)))
                                         (if (a) ((wood chuck)))
                                         could chuck wood))
              '((how much (wood))
                could
                ((a (wood) peeker chuck))
                (((peeker chuck)))
                (if (a) ((wood peeker chuck)))
                could peeker chuck wood))

; Atom S-expression -> Boolean
; check if an atom is exisits in the S-expression
; (define (member* a l) #f) ;stub
(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l)) (or
                        (eq? a (car l))
                        (member* a (cdr l))))
      (else
       (or
        (member* a (car l))
        (member* a (cdr l)))))))

(check-eq? (member* 'chips '((potato) (chips ((with) fish) (chips))))
           #t)

; S-expression -> Atom
; return the leftmost atom from an S-expression
; (define (leftmost l) #f)

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else
       (leftmost (car l))))))

(check-eq? (leftmost '((potato) (chips ((with) fish) (chips))))
           'potato)
(check-eq? (leftmost '(((hot) (tuna (and))) cheese))
           'hot)


; Give an example for x and l where
; (and (atom? (car l))
;      (eq? (car l) x))
;is true.

(check-eq? ((lambda (l x)
              (and (atom? (car l))
                   (eq? (car l) x)))
            '(one two) 'one)
           #t)


; S-expression S-expression -> Boolean
; check if two inputs are the same
; (define (eqlist? l1 l2) #f) ;stub

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((and (atom? (car l1)) (atom? (car l2)))
       (and
        (eqan? (car l1)
               (car l2))
        (eqlist? (cdr l1) (cdr l2))))
      ((and (list? (car l1))
            (list? (car l2))) (and (eqlist? (car l1) (car l2))
                                   (eqlist? (cdr l2) (cdr l2))))
      (else #f))))

(check-eq? (eqlist? '(banana ((split)))
                    '((banana) (split)))
           #f)
(check-eq? (eqlist? '(beef ((sausage)) (and (soda)))
                    '(beef ((salami)) (and (soda))))
           #f)
(check-eq? (eqlist? '(beef ((sausage)) (and (soda)))
                    '(beef ((sausage)) (and (soda))))
           #t)

; S-expression S-expression -> Boolean
; check if two S-expressions are the same
; (define (equal? s1 s2) #f)

(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2))
       (eqan? s1 s2))
      ((or (atom? s1) (atom? s2))
       #f)
      (else
       (eqlist?.v2 s1 s2)))))

(check-eq? (equal? 'a 'b) #f)
(check-eq? (equal? 'a 'a) #t)
(check-eq? (equal? '(banana ((split)))
                   '((banana) (split)))
           #f)
(check-eq? (equal? '(beef ((sausage)) (and (soda)))
                   '(beef ((sausage)) (and (soda))))
           #t)
