#lang s-exp syntax/module-reader
#:language (lambda (p)
             (read-syntax (object-name p) p))
#:info get-info


;; get-info : Symbol X [Symbol X -> Any] -> Any
;; The get-info function is a function of three arguments:
;;  - a symbol indicating the kind of information requested (as
;;    defined by external tools)
;;  - a default value that normally should be returned if the symbol
;;    is not recognized
;;  - a default-filtering function that takes the first two arguments
;;    and returns a result.
(define (get-info key dv df)
  (case key
    [(color-lexer) (df key dv)]
    [(drracket:toolbar-buttons)
     ;; return a (Listof ButtonSpec)
     (list
      (dynamic-require 'music-lang/lib/score/lang/button 'render-score-button))]
[else (df key dv)]))

