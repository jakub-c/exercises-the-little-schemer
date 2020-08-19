#lang racket

(require rackunit)

; Atom [List-of Atom] -> [List-of Atom]
(define rember.v1 (lambda (a lat)
                    (cond
                      ((null? lat) (quote ()))
                      (else (cond
                              ((eq? (car lat) a) (cdr lat))
                              (else (cons (car lat)
                                          (rember.v1 a
                                                     (cdr lat)))))))))

(check-equal? (rember.v1 'and '(bacon lettuce and tomato))
              '(bacon lettuce tomato))


; Atom [List-of Atom] -> [List-of Atom]
(define rember.v2 (lambda (a lat)
                    (cond
                      ((null? lat) (quote ()))
                      ((eq? (car lat) a) (cdr lat))
                      (else (cons (car lat)
                                  (rember.v2 a (cdr lat)))))))

(check-equal? (rember.v2 'and '(bacon lettuce and tomato))
              '(bacon lettuce tomato))

; See if you can write the function firsts

; [List-of [Non-emoty List-of S-expr] -> [List-of S-expr]
; (define (firsts los) '()) ;stub

; Scheme version for the reference
#;(define firsts
    (lambda (los)
      (cond ((null? los) '())
            (else
             (cons (car (car los))
                   (firsts (cdr los)))))))

; Racket version
(define firsts
  (lambda (los)
    (cond [(empty? los) '()]
          [else
           (cons (first (first los))
                 (firsts (rest los)))])))

(provide firsts)
(check-equal? (firsts '()) '())
(check-equal? (firsts '((a b) (c d) (e f)))
              '(a c e))

; Atom Atom [List-of Atom] -> [List-of Atom]
; build a lat with new inserted to the right of the first occurrence of old
; (define (insertR new old lat) '()) ;stub

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((eq? (car lat) old)
          (cons old
                (cons new (cdr lat))))
         (else
          (cons (car lat)
                (insertR new old (cdr lat)))))))))
              

(check-equal? (insertR 'e 'd '(a b c d f g d h))
              '(a b c d e f g d h))


; Atom Atom [List-of Atom] -> [List-of Atom]
; replace the first occurrence of old in the lat with new
; (define (subst new old lat) '()) ;stub

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((eq? (car lat) old)
          (cons new (cdr lat)))
         (else
          (cons (car lat)
                (subst new old (cdr lat)))))))))
              

(check-equal? (subst 'e 'd '(a b c d f g d h))
              '(a b c e f g d h))


; Atom Atom Atom [List-of Atom] -> [List-of Atom]
; replaces either the first occurrence of o1 or
; the first occurrence of o2 by new
; (define (subst new old lat) '()) ;stub

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((or
           (eq? (car lat) o1)
           (eq? (car lat) o2))
          (cons new (cdr lat)))
         (else
          (cons (car lat)
                (subst2 new o1 o2 (cdr lat)))))))))
              

(check-equal? (subst2 'vanilla 'chocolate 'banana
                      '(banana ice cream with chocolate topping))
              '(vanilla ice cream with chocolate topping))


(provide multirember)

; Atom [List-of Atom] -> [List-of Atom]
; return lat with all occurrences of a removed.
; (define (multirember a lat) '()) ;stub

(define multirember
  (lambda (a lat)
    (cond ((null? lat) '())
          ((eq? (car lat) a) (multirember a (cdr lat)))
          (else
           (cons (car lat)
                 (multirember a (cdr lat)))))))
                  

(check-equal? (multirember 'cup
                           '(coffee cup tea cup and hick cup))
              '(coffee tea and hick))

; Atom Atom [List-of Atom] -> [List-of Atom]
; build a lat with new inserted to the right of the all occurrences of old
;(define (multiinsertR new old lat) '())

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons old
             (cons new
                   (multiinsertR new old (cdr lat)))))
      (else
       (cons (car lat)
             (multiinsertR new old (cdr lat)))))))

(check-equal? (multiinsertR 'a 1 '(1 2 3 1 2 1))
              '(1 a 2 3 1 a 2 1 a))


; Atom Atom [List-of Atom] -> [List-of Atom]
; replace the all occurrences of old in the lat with new
; (define (multisubst new old lat) '()) ;stub

(define multisubst
  (lambda (new old lat)
  (cond ((null? lat) '())
        ((eq? (car lat) old)
         (cons new (multisubst new old (cdr lat))))
        (else
         (cons (car lat)
               (multisubst new old (cdr lat)))))))

(check-equal? (multisubst 'a 1 '(1 2 3 1 2 1 2))
              '(a 2 3 a 2 a 2))