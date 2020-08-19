#lang racket

(require rackunit)
(require "00-preface.rkt")

; AE is an ArithmeticExpression and is one of:
;  - Atom
;  - AE + AE
;  - AE * AE
;  - AE ^ AE

; S-expression -> Boolean
; check if the expression constans only numbers and +, * and ^
; (define (numbered? x) #f)

(define numbered?
  (lambda (axpr)
    (cond
      ((atom? axpr) (number? axpr))
      (else
       (and (numbered? (car axpr))
            (numbered? (car (cdr (cdr axpr)))))))))

      
(check-eq? (numbered? 1) #t)
(check-eq? (numbered? '(3 + (4 ^ 5))) #t)
(check-eq? (numbered? '(2 * sausage)) #f)

; S-expression -> Number
; evaluate a numeric expression
; (define (value nexp) 0) ;stub

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? '+ (car (cdr nexp)))
       (+ (value (car nexp))
          (value (car (cdr (cdr nexp))))))
      ((eq? '* (car (cdr nexp)))
       (* (value (car nexp))
          (value (car (cdr (cdr nexp))))))
      ((eq? '^ (car (cdr nexp)))
       (expt (value (car nexp))
             (value (car (cdr (cdr nexp)))))))))

(check-eq? (value 13) 13)
(check-eq? (value '(1 + 3)) 4)
(check-eq? (value '(1 + (10 + 2))) 13)
(check-eq? (value '(1 + (3 * 4))) 13)
(check-eq? (value '(1 + (3 ^ 4))) 82)

; S-expression -> Number
; (define (value.v2 nexp) 0) ;stub

(define value.v2
  (lambda (nexp)
    (cond
      ((number? nexp) nexp)
      ((eq? '+ (car nexp))
       (+ (value.v2 (car (cdr nexp)))
          (value.v2 (car (cdr (cdr nexp))))))
      ((eq? '* (car nexp))
       (* (value.v2 (car (cdr nexp)))
          (value.v2 (car (cdr (cdr nexp))))))
      ((eq? '^ (car nexp))
       (expt (value.v2 (car (cdr nexp)))
             (value.v2 (car (cdr (cdr nexp)))))))))

(check-eq? (value.v2 4) 4)
(check-eq? (value.v2 '(+ 4 4)) 8)
(check-eq? (value.v2 '(* (+ 4 4) 4)) 32)
(check-eq? (value.v2 '(^ (* (+ 1 1) 2)
                         2))
           16)

(provide 1st-sub-expr)
; S-expression -> S-expression
; (define (1st-sub-expr axpr) '())

(define 1st-sub-expr
  (lambda (axpr)
    (car (cdr axpr))))

(check-eq? (1st-sub-expr '(+ 1 2)) 1)
(check-equal? (1st-sub-expr '(+ (+ 3 6) 2)) '(+ 3 6))

(provide 2nd-sub-expr)
; S-expression -> S-expression
; (define (1st-sub-expr axpr) '())

(define 2nd-sub-expr
  (lambda (axpr)
    (car (cdr (cdr axpr)))))

(check-eq? (2nd-sub-expr '(+ 1 2)) 2)
(check-equal? (2nd-sub-expr '(+ (+ 3 6) (^ 3 3))) '(^ 3 3))

(provide operator)
; S-expression -> Atom
; retrun the operator of an expression, in this case
; it is the first Atom
; (define (operator anexpr) 'a) ;stub

(define operator
  (lambda (axpr)
    (car axpr)))

(check-eq? (operator '(+ 1 2)) '+)
(check-eq? (operator '(* (+ 3 6) (^ 3 3))) '*)


(define value.v3
  (lambda (nexpr)
    (cond
      ((atom? nexpr) nexpr)
      ((eq? '+ (operator nexpr))
       (+ (value.v3 (1st-sub-expr nexpr))
          (value.v3 (2nd-sub-expr nexpr))))
      ((eq? '* (operator nexpr))
       (* (value.v3 (1st-sub-expr nexpr))
          (value.v3 (2nd-sub-expr nexpr))))
      ((eq? '^ (operator nexpr))
       (expt (value.v3 (1st-sub-expr nexpr))
          (value.v3 (2nd-sub-expr nexpr)))))))

(check-eq? (value.v3 4) 4)
(check-eq? (value.v3 '(+ 4 4)) 8)
(check-eq? (value.v3 '(* (+ 4 4) 4)) 32)
(check-eq? (value.v3 '(^ (* (+ 1 1) 2)
                         2))
           16)

; representing numbers as lists
; this is zero: '()
; this is one: '(())
; this is two: '(() ())

; ListNumber is one of:
;  - '()
;  - (cons '() ListNumber)

; ListNumber -> Boolean
; check if list number represents 0
; (define (lnzero? n) #f)

(define lnzero?
  (lambda (n) (null? n)))

(check-eq? (lnzero? '()) #t)
(check-eq? (lnzero? '(())) #f)

; ListNumber -> ListNumber
; add representation of 1 to ListNumber
; (define (lnadd1 n) '(())) ;stub

(define lnadd1
  (lambda (n)
    (cons '() n)))

(check-equal? (lnadd1 '()) '(()))
(check-equal? (lnadd1 '(())) '(() ()))

; ListNumber -> ListNumber
; remove representation of 1 to ListNumber
; requirement: ListNumber can't be zero
; (define (lnsub1 n) '()) ;stub

(define lnsub1
  (lambda (n)
    (cdr n)))

(check-equal? (lnsub1 '(())) '())
(check-equal? (lnsub1 '(() ())) '(()))

; ListNumber ListNumber -> ListNumber
; add two LNs to each other
; (define (ln+ n m) '()) ; stub

(define ln+
  (lambda (n m)
    (cond
      ((lnzero? m) n)
      (else
       (cons '() (ln+ n (lnsub1 m)))))))

(check-equal? (ln+ '() '(())) '(()))
(check-equal? (ln+ '(()) '(())) '(() ()))
(check-equal? (ln+ '(() ()) '(())) '(() () ()))