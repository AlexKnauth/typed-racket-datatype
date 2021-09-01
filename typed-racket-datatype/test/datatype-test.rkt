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
;; Empty parameters are no parameters
;; https://github.com/AlexKnauth/typed-racket-datatype/issues/1

(define-datatype (A) (a))

(define the-a : A (a))
(define (accept-a [v : A]) (match v [(a) 'accepted]))

(module+ test
  (void (ann A? (pred A)))
  (check-pred A? (a))
  (check-pred a? (a))
  (check-equal? (accept-a the-a) 'accepted)
  (check-equal? (a) (a)))

;; ---------------------------------------------------------
;; Multiple variants, with parameters

(define-datatype (Option A) (None) (Some [value : A]))

(define-datatype (Result A B) (Ok [value : A]) (Err [value : B]))

(define-datatype (Lst A) (Empty) (Cons [first : A] [rest : (Lst A)]))

(: option-default (All (A) (-> (Option A) A A)))
(define (option-default o d)
  (match o
    [(Some v) v]
    [(None) d]))

(: option-then (All (A B) (-> (Option A) (-> A (Option B)) (Option B))))
(define (option-then o f)
  (match o
    [(Some a) (f a)]
    [(None) (None)]))

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
  (check-equal? (option-default (Some 8) 9) 8)
  (check-equal? (option-default (None) 10) 10)
  (check-equal? (option-then (Some 11)
                             (compose (inst Some String) number->string))
                (Some "11"))
  (check-equal? (option-then (None)
                             (compose (inst Some String) number->string))
                (None))
  (check-equal? (option-then (Some 12) (λ (a) (None)))
                (None))

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
;; Binary Tree based on Empty

(define-datatype (Tree0of A)
  (Empty0)
  (Node0 [value : A] [left : (Tree0of A)] [right : (Tree0of A)]))

(: Leaf0 (All (A) (-> A (Tree0of A))))
(define (Leaf0 v) (Node0 v (Empty0) (Empty0)))

(: tree0-flatten-list (All (A) (-> (Tree0of A) (Listof A))))
(define (tree0-flatten-list t)
  (match t
    [(Empty0) '()]
    [(Node0 v l r) (cons v (append (tree0-flatten-list l)
                                   (tree0-flatten-list r)))]))

(module+ test
  (void (ann Tree0of? (pred (Tree0of Any))))
  (check-pred Tree0of? (Empty0))
  (check-pred Tree0of? (Node0 1
                              (Node0 2 (Leaf0 3) (Leaf0 4))
                              (Node0 5 (Leaf0 6) (Leaf0 7))))
  (check-true (Empty0? (Empty0)))
  (check-true (Node0? (Node0 4 (Empty0) (Empty0))))
  (check-false (Empty0? (Node0 4 (Empty0) (Empty0))))
  (check-false (Node0? (Empty0)))
  (check-equal? (Node0-value (Node0 4 (Empty0) (Empty0))) 4)
  (check-equal? (Node0-left (Node0 4 (Leaf0 5) (Empty0))) (Leaf0 5))
  (check-equal? (Node0-right (Node0 4 (Leaf0 5) (Empty0))) (Empty0))
  (check-equal? (match (Empty0) [(Empty0) 0]) 0)
  (check-equal? (match (Leaf0 5) [(Node0 a _ _) a]) 5)
  (check-equal? (tree0-flatten-list
                 (Node0 1
                        (Node0 2 (Leaf0 3) (Leaf0 4))
                        (Node0 5 (Leaf0 6) (Leaf0 7))))
                '(1 2 3 4 5 6 7)))

;; ---------------------------------------------------------
;; Binary Tree based on Leaf

(define-datatype (Tree1of A)
  (Leaf1 [value : A])
  (Node1 [left : (Tree1of A)] [right : (Tree1of A)]))

(: tree1-flatten-list (All (A) (-> (Tree1of A) (Listof A))))
(define (tree1-flatten-list t)
  (match t
    [(Leaf1 v) (list v)]
    [(Node1 l r) (append (tree1-flatten-list l)
                         (tree1-flatten-list r))]))

;; ---------------------------------------------------------
;; Binary Tree based on Empty/Leaf

(define-datatype (Tree01of A)
  (Empty01)
  (Leaf01 [value : A])
  (Node01 [left : (Tree01of A)] [right : (Tree01of A)]))

(: tree01-flatten-list (All (A) (-> (Tree01of A) (Listof A))))
(define (tree01-flatten-list t)
  (match t
    [(Empty01) '()]
    [(Leaf01 v) (list v)]
    [(Node01 l r) (append (tree01-flatten-list l)
                          (tree01-flatten-list r))]))

;; ---------------------------------------------------------
