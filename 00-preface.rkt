#lang racket/base

(provide atom?)

; Atom -> Boolean
(define atom?
  (λ(x)
    (and (not (pair? x))
         (not (null? x)))))
