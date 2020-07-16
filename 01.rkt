#lang racket

(require rackunit)
(require "00-preface.rkt")

(check-true (atom? 'atom)) ;#t
(check-true (atom? 'turkey)) ;#t
(check-true (atom? '1492)) ;#t
(check-true (atom? 'u)) ;#t
(check-true (atom? '*abc$)) ;#t


(check-true (list? '(atom))) ;#t
(check-true (list? '(atom turkey or))) ;#t
; (check-exn exn:fail? (list? '(atom turkey) or)) ;#f and bad syntax
(check-true (list? '((atom turkey) or))) ;#t

; (sexpr? '(x y z)) ;#t
(check-true (list? '(x y z))) ;#t
; (sexpr? '((x y) z)) ;#t
(list? '(how are you doing so far)) ;#t
(length '(how are you doing so far)) ;6
(list? '(((how) are) ((you) (doing so)) far)) ;#t

; '(((how) are) ((you) (doing so)) far) - consitst of 3 S-expressions

(list? '()) ;#t
(atom? '()) ;#f
(list? '(() () () ())) ;#t

(car '(a b c)) ; 'a
(car '((a b c) x y z)) ;'(a b c)
; (car hotdot) ; error - argument needs to be a list
; (car '()) ; error - the list can not be empty


;;;; law of car

(car '(((hotdogs)) (and) (pickle) relish)) ; hotdogs