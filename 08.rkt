#lang racket

(require rackunit)
(require "00-preface.rkt")
(require "06.rkt")

; [X] [X -> Boolean] X List -> List
; (define (rember-f test? a l) '()) ;stub

(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) '())
      ((test? a (car l)) (rember-f test? a (cdr l)))
      (else
       (cons (car l)
             (rember-f test? a
                       (cdr l)))))))

(check-equal? (rember-f = 5 '(6 2 5 3))
              '(6 2 3))

(check-equal? (rember-f eq? 'jelly '(jelly beans are good))
              '(beans are good))

(check-equal? (rember-f equal? '(pop corn) '(lemonade (pop corn) and (cake)))
              '(lemonade and (cake)))

; [Atom -> Boolean] -> [Atom List] -> [List]
(define rember-f-curr
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? a (car l)) (cdr l))
        (else
         (cons (car l)
               ((rember-f-curr test?) a
                                      (cdr l))))))))

(check-equal? ((rember-f-curr =) 5 '(6 2 5 3))
              '(6 2 3))

(define rember-eq? (rember-f-curr eq?))

(check-equal? (rember-eq? 'tuna '(tuna salad is good))
              '(salad is good))


; [Atom -> Boolean] -> [Atom Atom List] -> [List]
(define insertR-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        ((null? lat) '())
        (else
         (cond
           ((test? (car lat) old)
            (cons old
                  (cons new (cdr lat))))
           (else
            (cons (car lat)
                  ((insertR-f test?) new old (cdr lat))))))))))
              

(check-equal? ((insertR-f eq?) 'e 'd '(a b c d f g d h))
              '(a b c d e f g d h))

; Any Any Any -> [List-of Any]
; (define (seqL new old l) '()) ;stub

(define seqL
  (lambda (new old list)
    (cons new (cons old list))))

(check-equal? (seqL 1 2 '(3)) '(1 2 3))

(define seqR
  (lambda (new old list)
    (cons old (cons new list))))

(check-equal? (seqR 1 2 '(3))
              '(2 1 3))

; [[Any Any Any] -> [List-of Any]] -> [Atom Atom List] -> List
; (define ((insert-g seq) old new list) '()) ;stub

(define insert-g
  (lambda (seq)
    (lambda (new old lat)
      (cond
        ((null? lat) '())
        (else
         (cond
           ((eq? (car lat) old)
            (seq new old (cdr lat)))
           (else
            (cons (car lat)
                  ((insert-g seq) new old
                                  (cdr lat))))))))))

(check-equal? ((insert-g seqR) 'e 'd '(a b c d f g d h))
              '(a b c d e f g d h))
(check-equal? ((insert-g seqL) 'e 'd '(a b c d f g d h))
              '(a b c e d f g d h))
(check-equal? ((insert-g (lambda (new old list)
                           (cons 'hello (cons new (cons old list)))))
               'a 'b '(a b))
              '(a hello a b))

(define inserL (insert-g seqL))

(define seqSubst
  (lambda (new old list)
    (cons new list)))

(check-equal? ((insert-g seqSubst)
               'a 'b
               '(a b c))
              '(a a c))


; Atom List -> List
(define yyy
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

; Atom Atom List -> List
(define seqrem
  (lambda (new old l) l))

(check-equal? (yyy 'a '(a b c)) '(b c))

; Atom -> [[Atom Atom] -> Number]
; translate the provided atom into a function represented by that atom
; (define (atom-to-function a) -) ;stub

(define (atom-to-function a)
  (cond ((eq? a '+) +)
        ((eq? a '*) *)
        ((eq? a '^) expt)))

(check-eq? ((atom-to-function '+) 1 1) 2)


; (define (value nexp) nexp) ;stub

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
       ((atom-to-function
         (operator nexp))
        (value (1st-sub-expr nexp))
        (value (2nd-sub-expr nexp)))))))

(check-eq? (value '(+ 1 2)) 3)
(check-eq? (value '(+ (+ 1 2) (+ 3 4))) 10)

; [Atom Atom -> Boolean] Atom [List-of Atom] -> [List-of Atom]
; remove an element from a list based on the provided test
; (define (multirember-f test? a lat) '()) ;stub

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? a (car lat))
         ((multirember-f test?) a (cdr lat)))
        (else
         (cons (car lat)
               ((multirember-f test?) a (cdr lat))))))))
         

(check-equal? ((multirember-f eq?) 'a '(a b c a b c))
              '(b c b c))
(check-equal? ((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna))
              '(shrimp salad salad and))

(define multirember-eq?
  (multirember-f eq?))


; Atom -> Atom -> Boolean
(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

; Atom -> Boolean
(define eq?-tuna
  (eq?-c 'tuna))

; (define (multiremberT test?) (lambda (lat) '())) ;stub

(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) '())
      ((test? (car lat))
       (multiremberT test? (cdr lat)))
      (else
       (cons (car lat)
             (multiremberT test? (cdr lat)))))))

(check-equal? (multiremberT eq?-tuna '(shrimp salad tuna salad and tuna))
              '(shrimp salad salad and))


; Atom Atom Atom [List-of Atom] -> [List-of Atom]
; (define (multiinsertLR new oldL oldR lat) '()) ;stub

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) oldL)
       (cons new
             (cons oldL
                   (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? (car lat) oldR)
       (cons oldR
             (cons new
                   (multiinsertLR new oldL oldR (cdr lat)))))
      (else
       (cons (car lat)
             (multiinsertLR new oldL oldR (cdr lat)))))))

(check-equal? (multiinsertLR 1 'c 'd '(a b c d e))
              '(a b 1 c d 1 e))


; Atom Atom Atom [List-of Atom] [List-of Atom Number Number -> ?] -> [List-of Atom]

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) (col '() 0 0))
      ((eq? (car lat) oldL)
       (multiinsertLR&co new oldL oldR
                         (cdr lat)
                         (lambda (newlat L R)
                           (col (cons new (cons oldL newlat))
                                (add1 L) R))))
      ((eq? (car lat) oldR)
       (multiinsertLR&co new oldL oldR
                         (cdr lat)
                         (lambda (newlat L R)
                           (col (cons oldR (cons new newlat))
                                L (add1 R)))))
      (else
       (multiinsertLR&co new oldL oldR
                         (cdr lat)
                         (lambda (newlat L R)
                           (col (cons (cdr lat) newlat) L R)))))))

;(check-equal? (multiinsertLR&co 'salty 'fish 'chips
;                                '(chips and fish or fish and chips))
;              '(chips salty and salty fish or salty fish and chips salty))


;(define (evens-only* sexp) '()) ;stub

(define evens-only*
  (lambda (expr)
    (cond
      ((null? expr) '())
      ((atom? (car expr))
       (cond
         ((even? (car expr)) (cons (car expr) (evens-only* (cdr expr))))
         (else
          (evens-only* (cdr expr)))))
      (else
       (cons (evens-only* (car expr))
             (evens-only* (cdr expr)))))))

(check-equal? (evens-only* '())
              '())
(check-equal? (evens-only* '(1 2))
              '(2))
(check-equal? (evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))
              '((2 8) 10 (() 6) 2))


(define the-last-friend
  (lambda (newl product sum)
    (cons sum
          (cons product
                newl))))

; (define (evens-only*&co l col) '()) ;stub

(define evens-only*&co
  (lambda (l col)
    (cond
      ((null? l) (col '() 1 0))
      ((atom? (car l))
       (cond
         ((even? (car l))
          (evens-only*&co (cdr l)
                          (lambda (newl p s)
                            (col (cons (car l) newl)
                                 (* (car l) p) s))))
         (else
          (evens-only*&co (cdr l)
                          (lambda (newl p s)
                            (col newl
                                 p (+ (car l) s)))))))
      (else
       (evens-only*&co (car l)
                       (lambda (al ap as)
                         (evens-only*&co (cdr l)
                                         (lambda (dl dp ds)
                                           (col (cons al dl)
                                                (* ap dp)
                                                (+ as ds))))))))))
        

(check-equal? (evens-only*&co '(2) the-last-friend)
              '(0 2 2))
(check-equal? (evens-only*&co '((1 2) (3 4)) the-last-friend)
              '(4 8 (2) (4)))
(check-equal? (evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2)
                              the-last-friend)
              '(38 1920 (2 8) 10 (() 6) 2))

; a few examples to understand continuations better

; the simplest example I could think of:
(define (plus+n a b con)
  (con (+ a b)))

(plus+n 1 2 (lambda (new) (+ new 3)))

; example of gethering two values across the evaluation
(define (show-sum-and-prod s p)
  (cons s (cons p '())))

(define result&co
  (lambda (lon con)
    (cond
      ((null? lon) (con 0 1))
      (else
       (result&co (cdr lon)
                  (lambda (sum prod)
                    (con (+ (car lon) sum)
                         (* (car lon) prod))))))))

(check-equal? (result&co '(1 2 3 4) show-sum-and-prod)
              `(,(+ 1 2 3 4) ,(* 1 2 3 4)))


; add1 to even numbers on the list, count occurances of even and odd numbers
(define count
  (lambda (lon con)
    (cond
      ((null? lon) (con '() 0 0))
      ((even? (car lon))
       (count (cdr lon) (lambda (newlon even odd)
                          (con (cons (car lon) newlon)
                               (add1 even) odd))))
      (else
       (count (cdr lon) (lambda (newlon even odd)
                          (con newlon
                           even (add1 odd))))))))

(check-equal? (count '(1 2 3 4 5 6) (lambda (lon even odd) (list lon even odd)))
              '((2 4 6) 3 3))