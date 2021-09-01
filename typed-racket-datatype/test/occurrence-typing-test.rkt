#lang typed/racket

;; Doesn't test datatypes or match directly,
;; tests the occurrence typing pattern behind match.

(: f (∀ (A B C D E F)
        (-> (-> (U A B D) Boolean : (U B D))
            (-> (U B D) C)
            (-> (U B D) C Boolean : #:+ D #:- (! D))
            (-> D C E)
            (-> (U A B) F)
            (-> (U A B D) (U E F)))))
(define (f condition-b computation-c condition-d computation-e computation-f)
  (lambda (a)
    (define b (condition-b a))
    (define c (if b (computation-c a) (ann #false (Refine [_ : False] (: b False)))))
    (if (and b (condition-d a c))
        (computation-e a c)
        (computation-f a))))

