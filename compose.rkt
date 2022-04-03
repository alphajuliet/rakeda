#lang racket
;; Implement a compose infix operator ⊕ for use within braces {}
;; See https://lexi-lambda.github.io/blog/2017/08/12/user-programmable-infix-operators-in-racket/

(require (for-syntax syntax/parse/class/paren-shape syntax/transformer)
         (prefix-in racket/base/ racket/base)
         syntax/parse/define)

(provide (all-defined-out))

(define-syntax-parser #%app
  [{~braces _ arg ...}
   #'(#%infix arg ...)]
  [(_ arg ...)
   #'(racket/base/#%app arg ...)])

(define-syntax-parser #%infix
  [(_ a op b)
   #'(racket/base/#%app op a b)]
  [(_ a op b more ...)
   #'(#%infix (#%infix a op b) more ...)])

(begin-for-syntax
  (struct infix-operator (runtime-binding fixity)
    #:property prop:procedure
    (λ (operator stx)
      ((set!-transformer-procedure
        (make-variable-like-transformer
         (infix-operator-runtime-binding operator)))
       stx))))

(define-syntax ⊕ (infix-operator #'compose 'right))

;; The End