#lang racket/base

(require (for-syntax racket/base
                     racket/format
                     syntax/modresolve
                     racket/file
                     racket/string
                     syntax/parse))

(provide (for-syntax library-name)
         import only except rename prefix)

(define-syntax (only stx)
  (raise-syntax-error 'only "import transformer not allowed as an expression"))
(define-syntax (except stx)
  (raise-syntax-error 'except "import transformer not allowed as an expression"))
(define-syntax (rename stx)
  (raise-syntax-error 'rename "import transformer not allowed as an expression"))
(define-syntax (prefix stx)
  (raise-syntax-error 'prefix "import transformer not allowed as an expression"))

(begin-for-syntax
  (define (join-library-name-elements els ctx/srcloc/props)
    (let* ([path-elements (map (compose1 ~a syntax->datum) els)]
           [module-path (string->symbol (string-join path-elements "/"))])
      (datum->syntax ctx/srcloc/props module-path ctx/srcloc/props ctx/srcloc/props)))
  
  (define-syntax-class library-name-element
    #:attributes []
    (pattern :id)
    (pattern :integer))

  (define-syntax-class library-name
    #:attributes [module-path]
    #:datum-literals [scheme]
    (pattern (~and stx (scheme element:library-name-element ...))
             #:with module-path
             (join-library-name-elements (cons #'r7rs (attribute element)) #'stx))
    (pattern (~and stx (element:library-name-element ...))
             #:with module-path
             (join-library-name-elements (attribute element) #'stx)))
  
  (define-syntax-class import-spec
    #:literals [only except rename prefix]
    #:attributes [require-spec]
    (pattern (only spec:import-spec id:id ...)
             #:with require-spec
             #'(only-in spec.require-spec id ...))
    (pattern (except spec:import-spec id:id ...)
             #:with require-spec
             #'(except-in spec.require-spec id ...))
    (pattern (rename spec:import-spec [id-orig:id id-new:id] ...)
             #:with require-spec
             #'(rename-in spec.require-spec [id-orig id-new] ...))
    (pattern (prefix spec:import-spec prefix-id:id)
             #:with require-spec
             #'(prefix-in prefix-id spec.require-spec))
    (pattern name:library-name
             #:with require-spec
             #'name.module-path)))

(begin-for-syntax
  (define (make-import-file target-path rkt-path)
    (define-values (dirname/p basename/p whatever)
      (split-path target-path))
    (define basename (path->string basename/p))
    (call-with-output-file
     rkt-path #:exists 'truncate
     (lambda (p)
       (write
        `(module _ r7rs (include ,basename))
        p)))
    rkt-path)

  (define (require-spec->module-id first)
    (if (list? first)
        (case (car first)
          ((only-in) (list-ref first 1))
          ((except-in) (list-ref first 1))
          ((rename-in) (list-ref first 1))
          ((prefix-in) (list-ref first 2))
          (else (car first)))
        first))

  (define (handle-import resolved)
    (define (try-path target-path)
      (and (file-exists? target-path)
           (make-import-file target-path resolved)))

    (or (file-exists? resolved)
        (try-path (path-replace-extension resolved ".sld"))
        (try-path (path-replace-extension resolved ".scm"))
        (try-path (path-replace-extension resolved ".ss"))
        (try-path (path-replace-extension resolved ".sls"))))

  )

(define-syntax (import stx)
  (syntax-parse stx
    [(_ spec:import-spec ...)
     (let* ((fullspec (syntax->datum #'(spec.require-spec ...)))
            (mods (map require-spec->module-id fullspec))
            (resolveds (map resolve-module-path mods))
            )
       (for-each handle-import resolveds)
       #'(require spec.require-spec ...))
     ]))
