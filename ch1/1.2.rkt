#lang typed/racket

(require typed/rackunit)

(provide (all-defined-out))

;; Defining nats as a recursive type.
(struct: Zero ())
(struct: (N) Succ ([n : N]))
(define-type Nat (Rec N (U Zero (Succ N))))

(: zero Nat)
(define zero (Zero))

(: one Nat)
(define one (Succ zero))

(: two Nat)
(define two (Succ one))

(: three Nat)
(define three (Succ two))

(: plus (Nat Nat -> Nat))
(define (plus m n)
  (match (list m n)
    [(list m (Zero)) m]
    [(list m (Succ n1)) (Succ (plus m n1))]))

(define: five : Nat (plus two three))
(define: seven : Nat (plus five two))
    
(: convertNat (Nat -> Natural))
(define (convertNat n)
  (match n
    [(Zero) 0]
    [(Succ n1) (add1 (convertNat n1))]))

(check-equal? (convertNat five) 5)
(check-equal? (convertNat seven) 7)
(check-equal? (convertNat (plus zero two)) 2)
(check-equal? (convertNat (plus (plus two two) zero)) 4)

(: mult (Nat Nat -> Nat))
(define (mult m n)
  (match (list m n)
    [(list m (Zero)) (Zero)]
    [(list m (Succ n1)) (plus m (mult m n1))]))

(check-equal? (convertNat (mult seven five)) 35)

(: fact (Nat -> Nat))
(define (fact n)
  (match n
    [(Zero) one]
    [(Succ n1) (mult n (fact n1))]))

(define: four : Nat (plus two two))
(check-equal? (convertNat (fact four)) 24)

(: fib (Nat -> Nat))
(define (fib n)
  (match n
    [(Zero) zero]
    [(Succ (Zero)) one]
    [(Succ (Succ n1)) (plus (fib (Succ n1)) (fib n1))]))

(define: six : Nat (plus five one))
(check-equal? (convertNat (fib six)) 8)

(: foldn (All (A) (A (A -> A) -> (Nat -> A))))
(define (foldn c h)
  (: f (Nat -> A))
  (define (f n)
    (match n
      [(Zero) c]
      [(Succ n1) (h (f n1))]))
  f)

(check-equal? ((foldn 0 add1) seven) 7)

(: succ (Nat -> Nat))
(define (succ n) (Succ n)) ; :|

;; Defining curried versions of plus, mult, expn.
(: cplus : (Nat -> (Nat -> Nat)))
(define (cplus m) (foldn m succ))

(: cmult : (Nat -> (Nat -> Nat)))
(define (cmult m) (foldn zero (cplus m)))

(: cexpn : (Nat -> (Nat -> Nat)))
(define (cexpn m) (foldn one (cmult m)))

(check-equal? (convertNat ((cexpn two) five)) 32)

;; Defining non-zero nats.
(struct: One ())
(struct: (N) Succ+ ([n : N])) 
(define-type Nat+ (Rec N+ (U One (Succ+ N+))))

(: foldn+ (All (A) (A (A -> A) -> (Nat+ -> A))))
(define (foldn+ c h)
  (: f (Nat+ -> A))
  (define (f n)
    (match n
      [(One) c]
      [(Succ+ n1) (h (f n1))]))
  f)

(: succ+ (Nat+ -> Nat+))
(define (succ+ n) (Succ+ n))

;; f and g give an isomorphism between Nat and Nat+.
(: f (Nat -> Nat+))
(define f (foldn (One) succ+))

(: g (Nat+ -> Nat))
(define g (foldn+ (Zero) succ))

(: four+ Nat+)
(define four+ (Succ+ (Succ+ (Succ+ (One)))))

(: convertNat+ (Nat+ -> Natural))
(define (convertNat+ n+)
  (match n+
    [(One) 1]
    [(Succ+ n1) (add1 (convertNat+ n1))]))

(check-equal? (convertNat+ (f (g four+))) (convertNat+ four+))
(check-equal? (convertNat (g (f seven))) (convertNat seven))

;; One way to define sqr in the form (f (foldn c h)) (where f is identity). :|
(: sqr (Nat -> Nat))
(define (sqr n) ((foldn zero (cplus n)) n))

(check-equal? (convertNat (sqr seven)) 49)

(: last ((Nat -> Boolean) Nat -> Nat))
(define (last p n)
  (: h ((Pair Nat Nat) -> (Pair Nat Nat))) ; cdr contains last known good value
  (define (h x)
    (cons (succ (car x)) (if (p (succ (car x))) (succ (car x)) (cdr x))))
  (cdr ((foldn (cons zero zero) h) n)))

(: evenn? (Nat -> Boolean))
(define (evenn? n) (even? (convertNat n)))

(check-equal? (convertNat (last evenn? seven)) 6)
(check-equal? (convertNat (last evenn? four)) 4)