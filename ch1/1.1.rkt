#lang typed/racket

(require typed/rackunit)

(provide (all-defined-out))

;; Defining types as a collection of possible values.
(define-type Bool (U 'True 'False))
(define-type Char (U 0 1 2 3 4 5 6 7 8 9 10)) ; etc, don't want to type #s upto 127

;; Sum types using the union operator.
(define-type Either (U Bool Char))

;; Typed structs are product types.
(struct: Both ([bool : Bool] [char : Char]))

(: x Bool)
(define x 'True)

(: y Both)
(define y (Both 'False 4))
; (define y (Both 'False 300)) --> type error!

(define: z : Either 2)
(define: w : Either 'True)

;; Defining data types with custom type constructors.
;; Also see section 4.7 here: 
;; http://download.racket-lang.org/docs/5.1/html/ts-guide/types.html)
(struct: (A) Just ([val : A]))
(struct: Nothing ())
(define-type (Maybe A) (U Nothing (Just A)))

(: m (Maybe Bool))
(define m (Just 'True))

(: n (Maybe Bool))
(define n (Nothing))