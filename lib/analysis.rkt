#lang racket

(require (prefix-in music: "repr.rkt")
         "types.rkt"
         "notes.rkt")

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

;; [Listof Chord]
;; [Listof Transition]
;; [Listof Transition]
;; Universe -> Void
(define (check-harmonies chords transitions pivots universe)
  (define figures (map chord->figure chords))
  (define proto-numerals
    (build-list 12 (λ (n) (list n (map (λ (figure)
                                         (list
                                          (modulo (- (first figure) n) 12)
                                          (second figure)))
                                       figures)))))
  
  (define numerals
    (map (λ (numeral-list)
           (list (first numeral-list)
                 (map (λ (proto-numeral)
                        (define symbol (assoc proto-numeral universe))
                        (and symbol (second symbol)))
                      (second numeral-list)))) proto-numerals))

  (define (correct? numerals)
    (or (and (null? (rest numerals)) (printf "[Analyzer: Success!]\n"))
        (and (member (list (first numerals) (second numerals)) transitions)
             (correct? (rest numerals)))))

  (define valid-numerals (filter (λ (numeral) 
                                   (printf "[Analyzer: trying ~a]\n" numeral)
                                   (correct? (second numeral)))
                                 numerals))
  (println valid-numerals)
  (not (null? valid-numerals)))