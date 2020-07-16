#lang racket

(require rackunit)
(require "00-preface.rkt")

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

; Number Number -> Number
(define plus
  (lambda (a b)
    (cond
      ((zero? a) b)
      (else
       (plus (sub1 a) (add1 b))))))

(define plus.v2
  (lambda (a b)
    (cond
      ((zero? a) b)
      (else
       (add1 (plus a (sub1 b)))))))

(check-eq? (plus.v2 1 3) 4)

; Number Number -> Number
(define minus
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else
       (minus (sub1 a) (sub1 b))))))

(check-eq? (minus 10 5) 5)

(define minus.v2
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else
       (sub1 (minus.v2 a (sub1 b)))))))

(check-eq? (minus.v2 10 5) 5)

; A Tuple is one of:
; - '()
; - '(Number Tuple)

; Tuple -> Number
; sum all numbers in the tup
(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else
       (plus.v2 (car tup)
                (addtup (cdr tup)))))))

(check-eq? (addtup '(1 2 3)) 6)

; Number Number -> Number
; multiply two numbers
(define mult
  (lambda (a b)
    (cond
      ((zero? b) 0)
      (else
       (plus.v2 a
                (mult a (sub1 b)))))))

(check-eq? (mult 5 3) 15)

; Tuple Tuple -> Tuple
; sum frist number from first and second tup,
; sum second number from first and second tup,
; continue for the lenght of the tup
; Note: tups need to be the same lenght
; (define (tup+ t1 t2) '()) ;stub

(define tup+
  (lambda (t1 t2)
    (cond ((and (null? t1)
                (null? t2)) '())
          (else
           (cons
            (plus.v2 (car t1)
                     (car t2))
            (tup+
             (cdr t1)
             (cdr t2)))))))

(check-equal? (tup+ '(1 2 3) '(3 2 1)) '(4 4 4))

; Tuple Typle -> Tuple
; Note: tups don't have to be the same size
; (define (tup+.v2 t1 t2) '()) ;stub

(define tup+.v2
  (lambda (t1 t2)
    (cond ((and (null? t1)
                (null? t2)) '())
          ((null? t1) t2)
          ((null? t2) t1)
          (else
           (cons
            (plus.v2 (car t1)
                     (car t2))
            (tup+.v2
             (cdr t1)
             (cdr t2)))))))

(check-equal? (tup+.v2 '(1 2 3) '(3 2 1 6 6)) '(4 4 4 6 6))

; Tuple Typle -> Tuple
; Note: tups don't have to be the same size
; Note: (and ...) from .v2 has been simplified
; (define (tup+.v2 t1 t2) '()) ;stub

(define tup+.v3
  (lambda (t1 t2)
    (cond ((null? t1) t2)
          ((null? t2) t1)
          (else
           (cons
            (plus.v2 (car t1)
                     (car t2))
            (tup+.v3
             (cdr t1)
             (cdr t2)))))))

(check-equal? (tup+.v3 '(1 2 3) '(3 2 1 6 6)) '(4 4 4 6 6))


; Number Number -> Boolean
; return #t if a is greater than b; #f otherwise
; (define (> a b) #f) ;stub

(define >
  (lambda (a b)
    (cond
      ((zero? a) #f)
      ((zero? b) #t)
      (else
       (> (sub1 a) (sub1 b))))))

(check-eq? (> 12 133) #f)
(check-eq? (> 120 11) #t)
(check-eq? (> 3 3) #f)

; Number Number -> Boolean
; return #t if a is smaller than b; #f otherwise
; (define (< a b) #f) ;stub

(define <
  (lambda (a b)
    (cond
      ((zero? b) #f)
      ((zero? a) #t)
      (else
       (< (sub1 a) (sub1 b))))))

(check-eq? (< 4 6) #t)
(check-eq? (< 8 3) #f)
(check-eq? (< 6 6) #f)

(define =.v1
  (lambda (m n)
    (cond
      ((zero? m) (zero? n))
      ((zero? n) #f)
      (else
       (=.v1 (sub1 m) (sub1 n))))))

(check-eq? (=.v1 2 2) #t)
(check-eq? (=.v1 3 2) #f)
(check-eq? (=.v1 2 3) #f)

; Number Number -> Boolean
; check if two arguments are equal
; (define (=.v2 a b) #f) ;stub

(define =.v2
  (lambda (a b)
    (and (not (> a b))
         (not (< a b)))))

(check-eq? (=.v2 2 2) #t)
(check-eq? (=.v2 2 3) #f)
(check-eq? (=.v2 3 2) #f)

; the solution in the book:
(define =.v3
  (lambda (a b)
    (cond
      ((> a b) #f)
      ((< a b) #f)
      (else #t))))

; Number Number -> Number
; return the value of a to the power of b
; (define (^ a b) 0) ;stub

(define ^
  (lambda (a b)
    (cond
      ((zero? b) 1)
      (else
       (mult a
             (^ a (sub1 b)))))))

(check-eq? (^ 1 1) 1)
(check-eq? (^ 2 3) 8)
(check-eq? (^ 5 3) 125)

; [X] [List-of X] -> Number
; produce the lenght of a given list
; (define (length l) 0) ;stub

(define (length l)
  (cond
    ((null? l) 0)
    (else (add1 (length (cdr l))))))

(check-eq? (length '()) 0)
(check-eq? (length '(one)) 1)
(check-eq? (length '(one two three)) 3)

; [X] [List-of X] -> X
; produce the nth element of the list
; (define (pick n l) '()) ;stub

(define pick
  (lambda (n l)
    (cond
      ((= n 1) (car l))
      (else
       (pick (sub1 n) (cdr l))))))

(check-eq? (pick 1 '(a)) 'a)
(check-eq? (pick 3 '(a b c d e)) 'c)

; [X] [List-of X] -> [List-of X]
; produce a list with an nth element removed
; (define (rempick n l) '()) ;stub

(define rempick
  (lambda (n l)
    (cond
      ((zero? (sub1 n)) (cdr l))
      (else
       (cons (car l)
             (rempick (sub1 n) (cdr l)))))))

(check-equal? (rempick 3 '(one two three four))
              '(one two four))

; [X] [List-of X] -> [List-of X]
; remove all numbers from the list
; (define (no-nums l) '()) ;stub

(define no-nums
  (lambda (l)
    (cond
      ((null? l) '())
      ((number? (car l)) (no-nums (cdr l)))
      (else
       (cons (car l)
             (no-nums (cdr l)))))))

(check-equal? (no-nums '(5 pears 6 prunes dates))
              '(pears prunes dates))

; [X] [List-of X] -> Tuple
; (define (all-nums l) '()) ;stub

(define all-nums
  (lambda (l)
    (cond
      ((null? l) '())
      ((number? (car l))
       (cons (car l) (all-nums (cdr l))))
      (else
       (all-nums (cdr l))))))

(check-equal? (all-nums '(5 pears 6 prunes dates))
              '(5 6))

; Atom Atom -> Boolean
; produce #t if two provided atoms are equal
;(define (eqan? a1 a2) #f) ;stub

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1)
            (number? a2)) (= a1 a2))
      ((and (atom? a1)
            (atom? a2) (eq? a1 a2) #t))
      (else #f))))
        

(check-eq? (eqan? 1 1) #t)
(check-eq? (eqan? 'a 'a) #t)
(check-eq? (eqan? 'a 1) #f)
(check-eq? (eqan? 'a 1) #f)

; Atom [List-of Atom] -> Number
; count the number of occurences of an atom
; (define (occur a lat) 0) ;stub

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eq? a (car lat))
       (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(check-eq? (occur 'a '()) 0)
(check-eq? (occur 'b '(a)) 0)
(check-eq? (occur 'a '(a)) 1)
(check-eq? (occur 'a '(a '())) 1)
(check-eq? (occur ' a'(a '() b a)) 2)

; Number -> Boolean
; produce #f if a given number is equal to one
; (define (one? n) #f)

(define one?
  (lambda (a)
    (= a 1)))

(check-eq? (one? 1) #t)
(check-eq? (one? 3) #f)

; Number [List-of Atom] -> [List-of Atom]
; remove the nth element from the list
; (define (rempick.v2 n lat) '()) ;stub

(define rempick.v2
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else
       (cons (car lat)
             (rempick.v2 (sub1 n) (cdr lat)))))))

(check-equal? (rempick.v2 2 '(one two three))
              '(one three))