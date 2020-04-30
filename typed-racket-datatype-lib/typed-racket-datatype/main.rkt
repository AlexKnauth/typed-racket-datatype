#lang typed/racket/base

(provide define-datatype)

(require syntax/parse/define
         (for-syntax racket/base
                     racket/syntax
                     syntax/stx))

(begin-for-syntax
  (define-syntax-class id/?
    #:attributes [?]
    [pattern x:id
      #:with ? (format-id #'x "~a?" #'x #:source #'x #:props #'x)])

  (define-syntax-class lhs
    #:attributes [name name.? xs]
    [pattern name:id/?            #:attr xs #f]
    [pattern (name:id/? x:id ...) #:attr xs #'(x ...)])

  (define-syntax-class (variant-name xs)
    #:attributes [name name.? type]
    [pattern name:id/?
      #:with type (if xs #`(name #,@xs) #'name)])

  (define-syntax-class (variant xs)
    #:attributes [name name.? type def]
    [pattern ({~var || (variant-name xs)} field ...)
      #:with def (cond [xs #`(struct #,xs name (field ...) #:transparent)]
                       [else #'(struct name (field ...) #:transparent)])]))

(define-syntax define-datatype
  (syntax-parser
    [(_ l:lhs {~var v (variant (attribute l.xs))})
     #:when (bound-identifier=? #'l.name #'v.name)
     (syntax-property #'v.def
                      'disappeared-use
                      (list (syntax-local-introduce #'l.name)))]
    [(_ l:lhs {~var v (variant (attribute l.xs))} ...)
     #'(begin
         v.def
         ...
         (define-type l (U v.type ...))
         (define (l.name.? x) (or (v.name.? x) ...)))]))

