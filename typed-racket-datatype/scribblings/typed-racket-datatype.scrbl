#lang scribble/manual

@(require (for-label typed/racket/base
                     typed-racket-datatype))

@title{typed-racket-datatype}
@author{Alex Knauth}

Source code:
@url{https://github.com/AlexKnauth/typed-racket-datatype}.

@defmodule[typed-racket-datatype]{
Provides @racket[define-datatype], a form for defining
Algebraic Data Types in Typed Racket.
}

@defform[#:literals [:]
  (define-datatype header (variant-id field ...) ...)
  #:grammar [(header name-id
                     (name-id param-id ...))
             (field [field-id : field-type])]]{
Defines @racket[name-id] as an Algebraic Data Type with the
given @racket[variant-id]s defined as structs with their
respective @racket[field]s. If @racket[param-id]s are
provided, the @racket[name-id] type and the
@racket[variant-id] structs are all polymorphic with those
@racket[param-ids], otherwise they are monomorphic.

Each variant struct includes a predicate and field
accessors, and @racket[define-datatype] also generates a
predicate @racket[name-id?] that accepts values that pass
any of the variant predicates.

For example
@racketblock[
  (define-datatype name-id
    (variant-id [field-id : field-type] ...)
    ...)]
is equivalent to
@racketblock[
  (struct variant-id ([field-id : field-type] ...) #:transparent)
  ...
  (define-type name-id (U variant-id ...))
  (define (name-id? x) (or (variant-id? x) ...))]
}
