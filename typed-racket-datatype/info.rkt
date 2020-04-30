#lang info

;; Package Info

(define collection "typed-racket-datatype")

(define deps '("base" "typed-racket-lib" "typed-racket-datatype-lib"))

(define build-deps
  '("scribble-lib"
    "racket-doc"
    "typed-racket-doc"
    "rackunit-lib"
    "rackunit-typed"))

(define implies '("typed-racket-datatype-lib"))
(define update-implies '("typed-racket-datatype-lib"))

;; Collection Info

(define scribblings '(("scribblings/typed-racket-datatype.scrbl" ())))

