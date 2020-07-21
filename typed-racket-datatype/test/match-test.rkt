#lang typed/racket/base

(require typed-racket-datatype
         typed-racket-datatype/match)

(: len (All (A) (-> (Listof A) Natural)))
(define (len xs)
  (match xs
    ['() 0]
    [(cons _ ys) (add1 (len ys))]))

(define-type Bit (U 0 1))
(define-type Bits (Listof Bit))

(: bits-add1 (-> Bits Bits))
(define (bits-add1 bs)
  (match bs
    ['() (list 1)]
    [(cons '0 rst) (cons 1 rst)]
    [(cons '1 rst) (cons 0 (bits-add1 rst))]))

(: unzip2 (All (A B) (-> (Listof (List A B)) (List (Listof A) (Listof B)))))
(define (unzip2 xys)
  (match xys
    ['() '(() ())]
    [(cons (cons x (cons y '())) rst)
     (match (unzip2 rst)
       [(cons xs (cons ys '()))
        (list (cons x xs) (cons y ys))])]))

#|
(: zip2 (All (A B) (-> (Listof A) (Listof B) (Listof (List A B)))))
(define (zip2 xs ys)
  (match (list xs ys)
    ['(() ()) '()]
    [(cons (cons x xs*) (cons (cons y ys*) '()))
     (cons (list x y) (zip2 xs* ys*))]
    [_ (error 'zip2 "lists have different lengths")]))
|#

(define-datatype Day (Sun) (Mon) (Tue) (Wed) (Thu) (Fri) (Sat))
(define-type Weekend (U Sat Sun))
(define-type Weekday (U Mon Tue Wed Thu Fri))

(: next-day (-> Day Day))
(define (next-day d)
  (match d
    [(Sun) (Mon)]
    [(Mon) (Tue)]
    [(Tue) (Wed)]
    [(Wed) (Thu)]
    [(Thu) (Fri)]
    [(Fri) (Sat)]
    [(Sat) (Sun)]))

(: next-weekday (-> Weekday Weekday))
(define (next-weekday d)
  (match d
    [(Mon) (Tue)]
    [(Tue) (Wed)]
    [(Wed) (Thu)]
    [(Thu) (Fri)]
    [(Fri) (Mon)]))

