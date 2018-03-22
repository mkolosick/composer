#lang racket

(require (prefix-in music: "repr.rkt")
         "types.rkt")

(provide (all-defined-out))

;; a MeasureChecker is a function measure-t time-signature key-signature -> Void

;; MeasureChecker
;; blames the measure when the measure is too long
(define (check-measure-length measure time-signature key-signature)
  (define measure-length (length (music:measure-notes measure)))
  (define num-beats (music:time-signature-beats time-signature))
  (define stx (music:measure-t-stx measure))
  
  (when (> measure-length num-beats) (blame stx "measure too long"))
  (when (< measure-length num-beats) (blame stx "measure too short")))