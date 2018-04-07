#lang typed/racket

(require typed/rackunit)

(provide (all-defined-out))

;; Defining non-empty cons lists as a recursive type.
(struct: (A) WrapCons ([v : A]))
(struct: (A B) Cons ([v : A] [l : B]))
(define-type (ListR+ A) (Rec LR+ (U (WrapCons A) (Cons A LR+))))

;; Defining non-empty snoc lists as a recursive type.
(struct: (A) WrapSnoc ([v : A]))
(struct: (B A) Snoc ([l : B] [v : A]))
(define-type (ListL+ A) (Rec LL+ (U (WrapSnoc A) (Snoc LL+ A))))

;; Foldr for ne cons lists.
(: foldr+ (All (A B) ((A -> B) (A B -> B) -> ((ListR+ A) -> B))))
(define (foldr+ f g)
  (: y ((ListR+ A) -> B))
  (define (y l)
    (match l
      [(WrapCons e) (f e)]
      [(Cons e rest) (g e (y rest))]))
   y)

(: outl (All (A B) (A B -> A)))
(define (outl m n) m)

(: outr (All (A B) (A B -> B)))
(define (outr m n) n)

(: head (All (A) ((ListR+ A) -> A)))
(define (head l) ((foldr+ (inst identity A) (inst outl A A)) l))

(check-equal? (head (Cons 5 (Cons 7 (WrapCons 19)))) 5)
(check-equal? (head (WrapCons 89)) 89)