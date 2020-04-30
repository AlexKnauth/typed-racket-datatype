typed-racket-datatype
=====================
Algebraic Datatypes for Typed Racket

```
(require typed-racket-datatype)
```

Provides `define-datatype`, a form for defining Algebraic Data Types in Typed Racket.

```
(define-datatype header (variant-id field ...) ...)

  header = name-id
         | (name-id param-id ...)
   field = [field-id : field-type]
```

Defines `name-id` as an Algebraic Data Type with the given `variant-id`s defined as structs with their respective `field`s.

---

The repository contains 2 packages:
 - `typed-racket-datatype-lib` has just the implementation
 - `typed-racket-datatype` adds tests and documentation

For the Racket Package Server, they have the package sources:
 - `git://github.com/AlexKnauth/typed-racket-datatype.git?path=typed-racket-datatype-lib`
 - `git://github.com/AlexKnauth/typed-racket-datatype.git?path=typed-racket-datatype`
