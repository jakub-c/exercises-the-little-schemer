#lang racket

(require rackunit)
(require "00-preface.rkt")
(require "02.rkt") ; member?
(require "03.rkt") ; mutlirember, firsts

; Set -> Boolean
; (define (set? lat) #f) ;stub

; version without member? function
; it's using a double recursive call in the body
; so it's rather hard to understand
(define set?-nomember
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((null? (cdr lat)) #t)
      (else
       (and
        (and (not (eq? (car lat) (car (cdr lat))))
             (set?-nomember (cons (car lat) (cdr (cdr lat)))))
        (set?-nomember (cdr lat)))))))

(check-eq? (set?-nomember '()) #t)
(check-eq? (set?-nomember '(apple)) #t)
(check-eq? (set?-nomember '(apple apple)) #f)
(check-eq? (set?-nomember '(apple orange)) #t)
(check-eq? (set?-nomember '(apples peaches pears plums)) #t)
(check-eq? (set?-nomember '(apple peaches apple plum)) #f)
(check-eq? (set?-nomember '(apple plum pears plum)) #f)

; my initial take on the function
; as the book suggests it can be simplified
;(define set?
;  (lambda (lat)
;    (cond
;      ((null? lat) #t)
;      (else
;       (and
;        (not (member? (car lat) (cdr lat)))
;        (set? (cdr lat)))))))

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(check-eq? (set? '()) #t)
(check-eq? (set? '(apple)) #t)
(check-eq? (set? '(apple apple)) #f)
(check-eq? (set? '(apple orange)) #t)
(check-eq? (set? '(apples peaches pears plums)) #t)
(check-eq? (set? '(apple peaches apple plum)) #f)
(check-eq? (set? '(apple plum pears plum)) #f)
(check-eq? (set? '(apple 3 pear 4 9 apple 3 4)) #f)

; List -> Set
; (define (makeset lat) '()) ;stub

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((member? (car lat) (cdr lat))
       (makeset (cdr lat)))
      (else
       (cons (car lat)
             (makeset (cdr lat)))))))

(check-equal? (makeset '(apple peach pear peach plum apple lemon peach))
              '(pear plum apple lemon peach))

; List -> Set
; (define (makeset-multirem lat) '()) ;stub
(define makeset-multirem
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else
       (cons (car lat)
             (makeset-multirem
              (multirember (car lat) (cdr lat))))))))

(check-equal? (makeset-multirem
               '(apple peach pear peach plum apple lemon peach))
              '(apple peach pear plum lemon))


; Set -> Boolean
; (define (subset? set1 set2) #f) ;stub

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else
       (and (member? (car set1) set2)
            (subset? (cdr set1) set2))))))

(check-eq? (subset? '(5 chicken wings)
                    '(5 hamburgers
                        2 pieces fried chicken and
                        light duckling wings))
           #t)

(check-eq? (subset? '(4 pounds of horseradish)
                    '(four pounds chicken and 5 ounces horseradish))
           #f)

; Set -> Boolean
; (define (eqset? set1 set2) #f) ;stub

; this was my initial idea
;(define eqset?
;  (lambda (set1 set2)
;    (cond
;      ((and (null? set1)
;            (null? set2)) #t)
;      (else
;       (and
;        (eq? (car set1) (car set2))
;        (eqset? (cdr set1) (cdr set2)))))))

; the solution from the book is inedded smarter and cleaner
(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(check-eq? (eqset? '(6 large chickens with wings)
                   '(6 large chickens with wings))
           #t)
(check-eq? (eqset? '(6 large chickens with wings)
                   '(6 large chickens with wings 7))
           #f)

; Set -> Boolean
; (define (insersect? set1 set2) #f) ;stub

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else
       (or (member? (car set1) set2)
           (intersect? (cdr set1) set2))))))

(check-eq? (intersect? '(stewed tomatoes and macaroni)
                       '(macaroni and cheese))
           #t)

; Set -> Set
; (define (intersect set1 set2) '()) ;stub

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      (else
       (cond
         ((member? (car set1) set2)
          (cons (car set1)
                (intersect (cdr set1) set2)))
         (else
          (intersect (cdr set1) set2)))))))

(check-equal? (intersect '(stewed tomatoes and macaroni)
                         '(macaroni and cheese))
              '(and macaroni))

; Set -> Set
; (define (union set1 set2) '()) ;stub

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)
       (union (cdr set1) set2))
      (else
       (cons (car set1)
             (union (cdr set1) set2))))))

(check-equal? (union '(stewed tomatoes and macaroni casserole)
                     '(macaroni and cheese))
              '(stewed tomatoes casserole macaroni and cheese))

; this is my approach to intersectall (using member?)
; I had to create a multimember? helper function to keep
; things readable, yet the solution in the book is way simpler

; Set -> Boolean
; (define (multimember? a l) #f) ;stub

(define multimember?
  (lambda (a l)
    (cond
      ((null? l) #t)
      (else
       (and (member? a (car l))
            (multimember? a (cdr l)))))))
     

(check-eq? (multimember? 'a '((a b) (a c) (a d))) #t)
(check-eq? (multimember? 'a '((a b) (a c) (d))) #f)

; Set -> Set
; find an intersection of all given sets
; (define (intersectall l) '()) ;stub
(define intersectall
  (lambda (l)
    (cond
      ((null? (car l)) '())
      ((multimember? (car (car l)) l)
       (cons (car (car l))
             (intersectall (cons (cdr (car l))
                                 (cdr l)))))
      (else (intersectall (cons (cdr (car l))
                                (cdr l)))))))

(check-equal? (intersectall '((a b c) (c a d e) (e f g h a b)))
              '(a))
(check-equal? (intersectall '((6 pears and)
                              (3 peaches and 6 peppers)
                              (8 pears and 6 plums)
                              (and 6 prunes with some apples)))
              '(6 and))

(define intersectall-book
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set)
                       (intersectall (cdr l-set)))))))

(check-equal? (intersectall-book '((6 pears and)
                                   (3 peaches and 6 peppers)
                                   (8 pears and 6 plums)
                                   (and 6 prunes with some apples)))
              '(6 and))


; List -> Boolean
; (define (a-pair? l) #f) ;stub

(define a-pair?
  (lambda (l)
    (and (or (atom? (car l)) (list? (car l)))
         (or (atom? (car (cdr l))) (list? (car (cdr l))))
         (= 2 (length l)))))

(check-eq? (a-pair? '(pear pear)) #t)
(check-eq? (a-pair? '(3 7)) #t)
(check-eq? (a-pair? '((2) (pair))) #t)
(check-eq? (a-pair? '(full (house))) #t)

; S-expression -> S-expression
(define first
  (lambda (sexp) (car sexp)))

(check-equal? (first '((1) 2)) '(1))

; S-expression -> S-expression
(define second
  (lambda (sexp) (car (cdr sexp))))

(check-eq? (second '((1) 2)) 2)

; S-expression -> S-expression
(define build
  (lambda (s1 s2) (cons s1 (cons s2 '()))))

(check-equal? (build 1 '(2 3)) '(1 (2 3)))

; S-expression -> S-expression
(define third
  (lambda (sexp) (car (cdr (cdr sexp)))))

(check-equal? (third '(1 (2 3) c)) 'c)

; [List-of Pair] -> Boolean

; (define (fun? l) #f) ;stub
(define fun?
  (lambda (rel) (set? (firsts rel))))

(check-eq? (fun? '((8 3) (4 2) (7 6) (6 2) (3 4))) #t)
(check-eq? (fun? '((d 4) (b 0) (b 9) (e 5) (g 4))) #f)

; Relation -> Relation
; (define (revrel rel) '()) ;stub

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else
       (cons (build (second (car rel))
                    (first (car rel)))
             (revrel (cdr rel)))))))

(check-equal? (revrel  '((8 a) (pumpkin pie) (got sick)))
              '((a 8) (pie pumpkin) (sick got)))

; Pair -> Pair
(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

; Relation -> Relation
(define revrel.v2
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else
       (cons (revpair (car rel))
             (revrel.v2 (cdr rel)))))))

(check-equal? (revrel.v2  '((8 a) (pumpkin pie) (got sick)))
              '((a 8) (pie pumpkin) (sick got)))

; Relation -> Set
(define seconds
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else
       (cons (second (car rel))
             (seconds (cdr rel)))))))

; Relation -> Boolean
; (define (fullfun? fun) #f) ;stub

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(check-eq? (fullfun? '((grape raisin)
                       (plum prune)
                       (stewed prune)))
           #f)
(check-eq? (fullfun? '((grape raisin)
                       (plum prune)
                       (stewed grape)))
           #t)

(define one-to-one? (lambda (fun)
                      (fun? (revrel fun))))

(check-eq? (one-to-one? '((grape raisin)
                          (plum prune)
                          (stewed prune)))
           #f)
(check-eq? (one-to-one? '((grape raisin)
                          (plum prune)
                          (stewed grape)))
           #t)
