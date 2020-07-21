#lang racket/base

(provide match)

(require (only-in racket/match match-expander?)
         (only-in typed/racket/base ann typecheck-fail Refine : False)
         syntax/parse/define
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse/experimental/template
                     syntax/parse/class/struct-id))

;; (nest (outer ...) ... inner)
(begin-for-syntax
  (define (nest outers inner) (foldr nest1 inner outers))
  (define (nest1 outer inner)
    (datum->syntax outer (append (syntax->list outer) (list inner)))))
(define-syntax-parser nest [(_ outer ... inner) (nest (attribute outer) #'inner)])

(begin-for-syntax
  (define-syntax-class id/expr
    #:attributes [id [nesting 1]]
    [pattern id:id
      #:with [nesting ...] #'[]]
    [pattern e:expr
      #:with id (generate-temporary #'e)
      #:with [nesting ...] #'[(let ([id e]))]])

  (define (length=? a b) (= (length a) (length b)))

  (define-syntax-class (match-case target)
    #:attributes [[nesting 1]]
    [pattern [p:pat then-expr:expr]
      #:with [nesting ...]
      #`[(let* ([p.outer-id #,target] [p.inner-id p.inner-expr] ...))
         (if p.condition-expr then-expr)]])

  (define-syntax-class simple-pat-ctor
    #:attributes [outer-id outer-condition-expr [inner-expr 1]]
    #:literals [box cons]
    [pattern box
      #:with outer-id (generate-temporary)
      #:with outer-condition-expr #'(box? outer-id)
      #:with [inner-expr ...] #'[(unbox outer-id)]]
    [pattern cons
      #:with outer-id (generate-temporary)
      #:with outer-condition-expr #'(pair? outer-id)
      #:with [inner-expr ...] #'[(car outer-id) (cdr outer-id)]]
    [pattern s:struct-id
      #:with outer-id (generate-temporary)
      #:with outer-condition-expr #'(s.predicate-id outer-id)
      #:with [inner-expr ...] #'[(s.accessor-id outer-id) ...]])

  (define-syntax-class pat
    #:attributes [outer-id [inner-id 1] [inner-expr 1] condition-expr]
    #:literals [quote]
    [pattern {~literal _}
      #:with outer-id (generate-temporary)
      #:with [[inner-id inner-expr] ...] '()
      #:with condition-expr #'#true]
    [pattern {~and x:id {~not (~var _ (static match-expander? "match expander"))}}
      #:with outer-id #'x
      #:with [[inner-id inner-expr] ...] '()
      #:with condition-expr #'#true]
    [pattern (quote datum)
      #:with outer-id (generate-temporary)
      #:with [[inner-id inner-expr] ...] '()
      #:with condition-expr #'(equal? outer-id 'datum)]
    [pattern (c:simple-pat-ctor p:pat ...)
      #:fail-when (and (not (length=? (attribute c.inner-expr) (attribute p))) #'c)
      (format "wrong number of arguments: expected ~a, given ~a"
              (length (attribute c.inner-expr))
              (length (attribute p)))
      #:with outer-id #'c.outer-id
      #:with outer-condition-tmp (generate-temporary)
      #:with [[inner-id inner-expr] ...]
      #'[[outer-condition-tmp c.outer-condition-expr]
         [p.outer-id
          (if outer-condition-tmp
              c.inner-expr
              (ann #false (Refine [_ : False] (: outer-condition-tmp False))))]
         ...
         [p.inner-id
          (if outer-condition-tmp
              p.inner-expr
              (ann #false (Refine [_ : False] (: outer-condition-tmp False))))]
         ...
         ...]
      #:with condition-expr #`(and outer-condition-tmp p.condition-expr ...)]))

(define-syntax match
  (lambda (stx)
    (syntax-parse stx
      [(_ e:id/expr c ...)
       #:declare c (match-case #'e.id)
       #:with this-stx stx
       #'(nest
          e.nesting
          ...
          c.nesting
          ...
          ...
          (typecheck-fail this-stx
                          "incomplete coverage"
                          #:covered-id e.id))])))

