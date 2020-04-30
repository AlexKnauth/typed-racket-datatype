#lang typed/racket/base

(require racket/match
         typed-racket-datatype)

(module+ test
  (require typed/rackunit))

;; ---------------------------------------------------------
;; One variant with same name

(define-datatype Posn (Posn [x : Real] [y : Real]))

(define-datatype (Pair A B) (Pair [fst : A] [snd : B]))

(module+ test
  (void (ann Posn? (pred Posn)))
  (check-pred Posn? (Posn 1 2))
  (check-equal? (Posn-x (Posn 3 4)) 3)
  (check-equal? (Posn-y (Posn 5 6)) 6)
  (check-equal? (match (Posn 7 8) [(Posn x y) (list x y)]) '(7 8))
  (check-equal? (Posn 9 10) (Posn 9 10))

  (void (ann Pair? (pred (Pair Any Any))))
  (check-pred Pair? (Pair 'a #t))
  (check-equal? (Pair-fst (Pair "a" '())) "a")
  (check-equal? (Pair-snd (Pair 5 'b)) 'b)
  (check-equal? (match (Pair 7 8) [(Pair x y) (list x y)]) '(7 8))
  (check-equal? (Pair 'c "d") (Pair 'c "d")))

;; ---------------------------------------------------------
;; Multiple variants, no parameters

(define-datatype Bool (True) (False))

(define-datatype Nat (Zero) (Succ [n : Nat]))

(define (Nat->Natural [n : Nat]) : Natural
  (match n
    [(Zero) 0]
    [(Succ nm1) (add1 (Nat->Natural nm1))]))

(module+ test
  (void (ann Bool? (pred Bool)))
  (check-pred Bool? (True))
  (check-pred Bool? (False))
  (check-true (True? (True)))
  (check-true (False? (False)))
  (check-false (True? (False)))
  (check-false (False? (True)))
  (check-equal? (match (True) [(True) 'yes]) 'yes)
  (check-equal? (False) (False))

  (void (ann Nat? (pred Nat)))
  (check-pred Nat? (Zero))
  (check-pred Nat? (Succ (Succ (Succ (Succ (Zero))))))
  (check-true (Zero? (Zero)))
  (check-true (Succ? (Succ (Zero))))
  (check-false (Zero? (Succ (Zero))))
  (check-false (Succ? (Zero)))
  (check-equal? (Succ-n (Succ (Succ (Zero)))) (Succ (Zero)))
  (check-equal? (match (Zero) [(Zero) 'yes]) 'yes)
  (check-equal? (match (Succ (Succ (Zero))) [(Succ n) n]) (Succ (Zero)))
  (check-equal? (Nat->Natural (Succ (Succ (Succ (Succ (Zero)))))) 4))

;; ---------------------------------------------------------
;; Multiple variants, with paraeters

(define-datatype (Option A) (None) (Some [value : A]))

(define-datatype (Result A B) (Ok [value : A]) (Err [value : B]))

(define-datatype (Lst A) (Empty) (Cons [first : A] [rest : (Lst A)]))

(: Lst->List (All (A) (-> (Lst A) (Listof A))))
(define (Lst->List l)
  (match l
    [(Empty) '()]
    [(Cons fst rst) (cons fst (Lst->List rst))]))

(module+ test
  (void (ann Option? (pred (Option Any))))
  (check-pred Option? (None))
  (check-pred Option? (Some 4))
  (check-true (None? (None)))
  (check-true (Some? (Some 'a)))
  (check-false (None? (Some 'a)))
  (check-false (Some? (None)))
  (check-equal? (match (None) [(None) 'yes]) 'yes)
  (check-equal? (match (Some 'inside) [(Some a) a]) 'inside)
  (check-equal? (Some 7) (Some 7))

  (void (ann Result? (pred (Result Any Any))))
  (check-pred Result? (Ok 1))
  (check-pred Result? (Err "bad stuff"))
  (check-true (Ok? (Ok 2)))
  (check-true (Err? (Err "badness")))
  (check-false (Ok? (Err "badness")))
  (check-false (Err? (Ok 2)))
  (check-equal? (match (Ok 3) [(Ok a) a]) 3)
  (check-equal? (match (Err "bad") [(Err a) a]) "bad")
  (check-equal? (Ok 200) (Ok 200))
  (check-equal? (Err 404) (Err 404))

  (void (ann Lst? (pred (Lst Any))))
  (check-pred Lst? (Empty))
  (check-pred Lst? (Cons 1 (Cons 2 (Cons 3 (Empty)))))
  (check-true (Empty? (Empty)))
  (check-true (Cons? (Cons 4 (Empty))))
  (check-false (Empty? (Cons 4 (Empty))))
  (check-false (Cons? (Empty)))
  (check-equal? (Cons-first (Cons 4 (Empty))) 4)
  (check-equal? (Cons-rest (Cons 4 (Empty))) (Empty))
  (check-equal? (match (Empty) [(Empty) 0]) 0)
  (check-equal? (match (Cons 5 (Empty)) [(Cons a _) a]) 5)
  (check-equal? (Lst->List (Cons 1 (Cons 2 (Cons 3 (Empty))))) '(1 2 3)))

;; ---------------------------------------------------------
;; Contravariance

(define-datatype (Consumerof A)
  (Consumer1 [function : (-> A Void)])
  (Consumer2 [function : (-> A A Void)]))

(module+ test
  (check-pred Consumerof? (Consumer1 void))
  (check-pred Consumerof? (Consumer2 void))
  (check-pred Consumerof? (Consumer1 write-char))
  (check-pred Consumerof? (Consumer2 write-char))
  (void (ann (Consumer1 void) (Consumerof Any)))
  (void (ann (Consumer1 write-char) (Consumerof Char)))
  (void (ann (Consumer2 void) (Consumerof Any)))
  (void (ann (Consumer2 write-char) (Consumerof Nothing)))
  (check-equal? (Consumer1-function (Consumer1 write-char)) write-char))

;; ---------------------------------------------------------
