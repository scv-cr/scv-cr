#lang racket/base

(require (only-in typed/racket/base
                  require/typed/provide
                  require/typed))

(provide (rename-out [require/typed/provide require/typed/provide/opaque]
                     [require/typed require/typed/opaque])
         require/opaque)

(define-syntax-rule (require/opaque m x ...)
  (require m))
