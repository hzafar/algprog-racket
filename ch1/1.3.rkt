#lang typed/racket

(require typed/rackunit)

(provide (all-defined-out))

;; Defining cons lists as a recursive type.
(struct: NilCons () #:transparent)
(struct: (A B) Cons ([v : A] [l : B]) #:transparent)
(define-type (ListR A) (Rec LR (U NilCons (Cons A LR))))

;; Defining snoc lists as a recursive type.
(struct: NilSnoc () #:transparent)
(struct: (B A) Snoc ([l : B] [v : A]) #:transparent)
(define-type (ListL A) (Rec LL (U NilSnoc (Snoc LL A))))

;; Convert a snoc list to a cons list.
(: convert (All (A) ((ListL A) -> (ListR A))))
(define (convert ll)
  (match ll
    [(NilSnoc) (NilCons)]
    [(Snoc rest v) (snocr (convert rest) v)]))

(: snocr (All (A) ((ListR A) A -> (ListR A))))
(define (snocr ll v)
  (match ll
    [(NilCons) (Cons v (NilCons))]
    [(Cons e rest) (Cons e (snocr rest v))]))

;; Comparison function for cons lists.
(: lrequal? (All (A) ((ListR A) (ListR A) -> Boolean)))
(define (lrequal? l1 l2)
  (match l1
    [(NilCons) (equal? l2 (NilCons))]
    [(Cons e1 rest1)
     (match l2
       [(NilCons) #f]
       [(Cons e2 rest2) (and (equal? e1 e2) (lrequal? rest1 rest2))])]))

(check-equal? (lrequal? (Cons 'a (Cons 'b (NilCons))) (NilCons)) #f)
(check-equal? (lrequal? (Cons 1 (Cons 5 (NilCons))) (Cons 1 (Cons 5 (NilCons)))) #t)

(: l1 (ListR Natural))
(define l1 (Cons 1 (Cons 32 (Cons 9 (Cons 15 (NilCons))))))

(: l2 (ListL Natural))
(define l2 (Snoc (Snoc (Snoc (Snoc (NilSnoc) 1) 32) 9) 15))

(check-equal? (lrequal? (convert l2) l1) #t)

;; Implementation of listr functor that doesn't use foldr.
;; See below for foldr-based implementation.
(: listr1 (All (A B) ((B -> A) -> ((ListR B) -> (ListR A)))))
(define (listr1 f)
  (: helper ((ListR B) -> (ListR A)))
  (define (helper l)
    (match l
      [(NilCons) (NilCons)]
      [(Cons e rest) (Cons (f e) (helper rest))]))
  helper)

(define: inc : (Natural -> Natural) add1)
(check-equal? (lrequal? ((listr1 inc) (Cons 1 (Cons 3 (NilCons)))) (Cons 2 (Cons 4 (NilCons)))) #t)

;; Fold over cons lists.
(: foldr (All (A B) (B (A B -> B) -> ((ListR A) -> B))))
(define (foldr c h)
  (: f ((ListR A) -> B))
  (define (f l)
    (match l
      [(NilCons) c]
      [(Cons e rest) (h e (f rest))]))
  f)

;; Defining `listr f` using foldr.
(: listr (All (A B) ((B -> A) -> ((ListR B) -> (ListR A)))))
(define (listr f)
  (: h (B (ListR A) -> (ListR A)))
  (define (h a x) (Cons (f a) x))
  (foldr (NilCons) h))

(check-equal? (lrequal? ((listr inc) (Cons 1 (Cons 3 (NilCons)))) (Cons 2 (Cons 4 (NilCons)))) #t)

;; Fold over snoc lists.
(: foldl (All (A B) (B (B A -> B) -> ((ListL A) -> B))))
(define (foldl c h)
  (: f ((ListL A) -> B))
  (define (f l)
    (match l
      [(NilSnoc) c]
      [(Snoc rest e) (h (f rest) e)]))
  f)

(: sum ((ListR Natural) -> Natural))
(define sum (foldr 0 +))

;; Length as a fold after map.
(: length (All (A) (ListR A) -> Natural))
(define (length l) (sum ((listr (λ (x) 1)) l)))

(check-equal? (length l1) 4)

;; Length directly as a fold.
(: length-2 (All (A) (ListR A) -> Natural))
(define (length-2 l)
  (: h (A Natural -> Natural))
  (define (h e n) (inc n))
  ((foldr 0 h) l))
 
(check-equal? (length-2 l1) 4)

;; Filter for cons lists.
(: filterlr (All (A) (A -> Boolean) -> ((ListR A) -> (ListR A))))
(define (filterlr p)
  (: g (A -> (ListR A)))
  (define (g x)
    (if (p x) (Cons x (NilCons)) (NilCons)))
  (lambda ([l : (ListR A)]) (appendlr ((listr g) l))))

;; Append for cons lists.
(: appendlr (All (A) ((ListR (ListR A)) -> (ListR A))))
(define (appendlr lol)
  (: appendlr2 ((ListR A) (ListR A) -> (ListR A)))
  (define (appendlr2 l1 l2)
    (match l1
      [(NilCons) l2]
      [(Cons e rest) (Cons e (appendlr2 rest l2))]))
  (match lol
    [(NilCons) (NilCons)]
    [(Cons lol1 rlols) (appendlr2 lol1 (appendlr rlols))]))

(check-equal? (length (appendlr (Cons (Cons 2 (NilCons)) (Cons (Cons 5 (NilCons)) (NilCons))))) 2)
(check-equal? (length (((inst filterlr Integer) even?) l1)) 1)