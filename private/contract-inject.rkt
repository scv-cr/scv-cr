#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide contract-inject)

(require
 racket/require
 (multi-in racket (syntax function set path list))
 (multi-in syntax (parse modresolve))
 (multi-in scv-cr private (contract-extract
                           configure
                           syntax-util)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Path Syntax Ctc-Data Boolean -> Syntax
;; takes original syntax and contract data and uses contract information
;; to inject provide and require contracts into the unexpanded syntax
(define (contract-inject target stx data as-opaque?)
  (syntax-parse stx
    #:datum-literals (module)
    [(module name lang (mb forms ...))
     (define forms*
       (for/fold ([stx       #'(forms ...)])
                 ([injection (list (inject-require target as-opaque?)
                                   inject-provide
                                   inject-predicates)])
         (injection stx data)))
     #`(module name #,(no-check #'lang)
         (#,(syntax-attach-scope #'mb)
          #,@(syntax-fresh-scope forms*)))]))

;; Syntax -> Syntax
;; changes Typed Racket #lang to the no-check variant
(define (no-check lang)
  (syntax-attach-scope (format-id lang "~a/no-check" (syntax-e lang))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Syntax Ctc-Data -> Syntax
;; takes original syntax and contract data and uses contract information
;; to inject provide contracts into the unexpanded syntax
(define (inject-provide forms data)
  (define-values (p-bundle r-bundle)
    (values (ctc-data-provide data)
            (ctc-data-require data)))
  (define provide-box (box '()))
  (define forms* (munge-provides forms p-bundle r-bundle provide-box))
  #`((require soft-contract/fake-contract
              #,@(ctc-bundle-deps p-bundle))
     #,@(ctc-bundle-defns p-bundle)
     #,@forms*
     #,@(unbox provide-box)
     (provide #,@(ctc-bundle-outs p-bundle))))

;; Syntax Ctc-Data [Box [List-of Syntax]] -> Syntax
;; removes already provided identifiers from provide forms
(define (munge-provides stx p-bundle r-bundle provide-box)
  (define not-provided
    (syntax-parser
      #:datum-literals (provide)
      [(provide x ...)
       (define xs*
         (append-map (ctc-bundle-provides p-bundle r-bundle)
                     (syntax-e #'(x ...))))
       (set-box! provide-box (cons #`(provide #,@xs*) (unbox provide-box)))
       #'(void)]
       [x #'x]))
  (syntax-parse stx
    [(x ...)
     (define provides*
       (map not-provided (syntax-e #'(x ...))))
     (datum->syntax stx provides* stx stx)]
    [x #'x]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Path -> (Syntax Ctc-Data -> Syntax)
;; takes original syntax and contract data and uses contract information
;; to inject require contracts into the unexpanded syntax
(define ((inject-require target as-opaque?) forms data)
  (define bundle (ctc-data-require data))
  (define required-set (mutable-set))
  (define opaque-types (box '()))
  (define opaque-defns (box '()))
  (define forms* ((munge-requires target
                                  required-set
                                  opaque-types
                                  (and as-opaque? opaque-defns))
                  forms))
  (define-values (defs defs*) (make-definition bundle))
  #`((module require/contracts racket/base
       (require soft-contract/fake-contract
                #,@(set->list required-set)
                #,@(ctc-bundle-deps bundle))
       #,@(ctc-bundle-defns bundle)
       #,@(unbox opaque-defns)
       (provide #,@(hash-keys (ctc-bundle-i/c-hash bundle))
                #,@(ctc-bundle-outs bundle)))
     (require (prefix-in -: (only-in 'require/contracts #,@defs))
              (except-in 'require/contracts #,@defs))
     (define-values (#,@defs) (values #,@defs*))
     #,@(unbox opaque-types)
     #,@forms*))

;; Ctc-Bundle -> [List-of Symbol] [List-of Symbol]
;; identifiers that should be defined by the require form
(define (make-definition bundle)
  (define to-define
    (append (hash-keys (ctc-bundle-p/c-hash bundle))
            (map (λ (x)
                   (syntax-parse x
                     [x:struct-name
                      (format-symbol "~a?" #'x.name)]))
                 (hash-keys (ctc-bundle-s/o-hash bundle)))))
  (values to-define
          (map (λ (x) (format-symbol "-:~a" x)) to-define)))

(define-syntax-class rt-clause
  #:attributes ([outs 1] opaque-def)
  (pattern [n:id _]
           #:with [outs ...] (list #'n)
           #:with opaque-def #'(define n #:opaque))
  (pattern [(~datum #:struct) sn:struct-name ([f:id : t] ...)]
           #:with [outs ...] (list #'(struct-out sn.name)
                                   (format-id #'sn.name "~a?" (syntax-e #'sn.name)))
           #:with opaque-def (if (syntax-e #'sn.super)
                                 #'(struct sn.name sn.super (f ...) #:transparent)
                                 #'(struct sn.name (f ...) #:transparent)))
  (pattern [(~datum #:opaque) _ pred:id]
           #:with [outs ...] (list #'pred)
           #:with opaque-def #'(define pred #:opaque)))

;; Path Set -> Syntax -> Syntax
;; extracts and munges require forms
(define ((munge-requires target required-set opaque-types opaque-defns) stx)
  (define opaque? #f)
  (define to-provide
    (syntax-parse stx
      #:datum-literals (require/typed/provide
                        require/typed/provide/opaque
                        require/typed/opaque)
      [(require/typed/provide m x:rt-clause ...)
       #'(provide x.outs ... ...)]
      [(require/typed/provide/opaque m x:rt-clause ...)
       (set! opaque? #t)
       (when opaque-defns
         (set-box! opaque-defns
                   (cons #'(begin x.opaque-def ...) (unbox opaque-defns))))
       #'(provide x.outs ... ...)]
      [(require/typed/opaque m x:rt-clause ...)
       (set! opaque? #t)
       (when opaque-defns
         (set-box! opaque-defns
                   (cons #'(begin x.opaque-def ...) (unbox opaque-defns))))
       #'(void)]
      [_ #f]))
  (syntax-parse stx
    #:datum-literals (require/typed
                      require/typed/check
                      require/typed/opaque
                      require/typed/provide
                      require/typed/provide/opaque)
    [(~or* (require/typed m x ...)
           (require/typed/check m x ...)
           (require/typed/opaque m x ...)
           (require/typed/provide m x ...)
           (require/typed/provide/opaque m x ...))
     #:with [opaque ...] (filter-map make-opaque-type (syntax->list #'[x ...]))
     (define required-module (syntax-e #'m))
     (define m-typed?
       (if (path-string? required-module)
           (parameterize ([current-directory (path-only target)])
             (module-typed? (path->complete-path required-module)))
           #f))
     (define to-require
       (cond
         [(and m-typed? (not (ignore-check)))
          #'(require m)]
         [else
          (set-box! opaque-types (append (syntax->list #'[opaque ...])
                                         (unbox opaque-types)))
          (unless (or (and opaque? opaque-defns)
                      (eq? (syntax-e #'m) 'racket/base))
            (set-add! required-set #'m))
          #'(void)]))
     #`(begin #,to-require #,(or to-provide #'(void)))]
    [(x ...)
     (datum->syntax stx
                    (map (munge-requires target
                                         required-set
                                         opaque-types
                                         opaque-defns)
                         (syntax-e #'(x ...)))
                    stx
                    stx)]
    [x #'x]))

;; Syntax -> Syntax or #f
;; creates an opaque type definition from require/typed forms
(define (make-opaque-type stx)
  (syntax-parse stx
    [((~datum #:opaque) ?type ?pred)
     #'(define-type ?type (Opaque ?pred))]
    [_ #f]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (inject-predicates stx data)
  (define-values (d-pred m-pred)
    (values (ctc-data-d-pred data)
            (ctc-data-m-pred data)))
  (let go ([stx stx])
    (syntax-parse stx
      #:datum-literals (define-predicate make-predicate)
      [(define-predicate x _)
       #`(define x #,(hash-ref-stx d-pred stx))]
      [(make-predicate _)
       (hash-ref-stx m-pred stx)]
      [(x ...)
       (datum->syntax stx
                      (map go (syntax-e #'(x ...)))
                      stx
                      stx)]
      [x #'x])))
