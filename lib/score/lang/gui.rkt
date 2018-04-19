#lang racket/base

(provide execute-callback-with-rep:after-expression
         message-box)

(require racket/class
         racket/gui/base
         drracket/tool-lib)

(define (execute-callback-with-rep:after-expression frame after-expr-thunk)
  (parameterize ([drracket:rep:after-expression after-expr-thunk])
    (send frame execute-callback)))

