#lang racket/base

(require (for-syntax racket/base
                     racket/function
                     syntax/parse)
         (prefix-in 6: rnrs/base-6)
         "import.rkt")

(provide cond-expand features)

(define-syntax supported-features
  '(r7rs racket exact-closed exact-complex ieee-float full-unicode ratios))

(define-syntax (define-features-list stx)
  (syntax-parse stx
    [(_ features:id)
     (with-syntax ([(supported-feature ...) (syntax-local-value #'supported-features)])
       #'(define (features)
           (6:list 'supported-feature ...)))]))

(define-features-list features)

(begin-for-syntax
  (define (module-exists? path)
    (with-handlers ([exn:missing-module? (const #f)])
      (dynamic-require path #f)
      #t))

  (define-syntax-class ce-requirement
    #:literals (6:and 6:or 6:not 6:else)
    #:datum-literals (library else)
    #:attributes (true?)
    (pattern (6:and req:ce-requirement ...)
             #:attr true? (andmap values (attribute req.true?)))
    (pattern (6:or req:ce-requirement ...)
             #:attr true? (ormap values (attribute req.true?)))
    (pattern (6:not req:ce-requirement)
             #:attr true? (not (attribute req.true?)))
    (pattern (library name:library-name)
             #:attr true? (module-exists? (syntax->datum #'name.module-path)))
    (pattern else
             #:attr true? #t)
    (pattern 6:else
             #:attr true? #t)
    (pattern feature:id
             #:attr true? (member (syntax->datum #'feature)
                                  (syntax-local-value #'supported-features)))))

(define-syntax cond-expand
  (syntax-parser
    [(_ [req:ce-requirement . body] ...+)
     (or (for/or ([true? (in-list (attribute req.true?))]
                  [body (in-list (attribute body))])
           (and true? #`(begin . #,body)))
         #'(begin))]))

(module+ test
  (require rackunit)

  (check-true
   (cond-expand [racket #t]
                [6:else #f]))

  (check-false
   (cond-expand [non-supported-feature #t]
                [6:else #f]))

  (check-true
   (cond-expand [(library (racket base)) #t]
                [6:else #f]))
  (check-false
   (cond-expand [(library (some really long module that should never exist)) #t]
                [6:else #f])))
