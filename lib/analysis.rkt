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
;; Universe -> [Listof (list Key [Listof chord-symbol-t])]
(define (check-harmonies chords transitions pivots universe)
  (define figures (map chord->figure chords))
  
  (define (gen-numerals type)
    (build-list
     12
     (λ (n) (list
             (music:key-signature
              (PitchNumber->pitch-class n)
              type)
             (map (λ (figure)
                    (music:figure-t
                     (modulo (- (music:figure-bass figure) n) 12)
                     (music:figure-intervals figure)
                     (music:figure-t-stx figure)))
                  figures)))))
  
  (define proto-numerals
    (append (gen-numerals 'major) (gen-numerals 'minor)))
  
  (define numerals
    (map (λ (numeral-list)
           (list (first numeral-list)
                 (map (λ (proto-numeral)
                        (music:chord-symbol-t
                         (dict-ref
                          universe
                          (music:figure
                           (music:figure-bass proto-numeral)
                           (music:figure-intervals proto-numeral))
                          #f)
                         (music:figure-t-stx proto-numeral)))
                      (second numeral-list))))
         proto-numerals))

  (define (find-path analyses)

    (define (min-result r1 r2) (if (< (second r1) (second r2)) r1 r2))
    
    (define (try-pivot analysis other-analyses acc)
      (cond
        [(null? acc) (list (first analysis) (length analysis))]
        ;; if there is a pivot, perform it, otherwise, try the next pivot
        [(member (list (music:chord-symbol-symbol (first analysis)) (music:chord-symbol-symbol (caar acc))) pivots)
         ;; perform the pivot and try to find a path
         (let ([result (find-path/helper
                        (rest (first acc))
                        (map rest (cons analysis (remove* (list (first acc)) other-analyses))))])
           ;; if a path is found, return the result, otherwise, try the next pivot
           (if (zero? (second result))
               result
               (min-result result (try-pivot analysis other-analyses (rest acc)))))]
        [else (try-pivot analysis other-analyses (rest acc))]))

    (define (find-path/helper analysis other-analyses)
      (cond
        [(null? analysis) (list analysis 0)]
        [(null? (rest analysis)) (list (first analysis) 0)]
        ;; if the transition is valid, make it, otherwise, try pivoting
        [(member (list (music:chord-symbol-symbol (first analysis)) (music:chord-symbol-symbol (second analysis))) transitions)
         (let ([result (find-path/helper (rest analysis) (map rest other-analyses))])
           ;; if following the transition led to success, return the result, otherwise, try pivoting
           (if (zero? (second result))
               result
               (min-result result (try-pivot analysis other-analyses other-analyses))))]
        [else (try-pivot analysis other-analyses other-analyses)]))
    
    (map (λ (analysis) (find-path/helper analysis (remove* (list analysis) analyses))) analyses))
  
  (define analyses-no-key (map second numerals))
  (define result (argmin second (find-path analyses-no-key)))
  (when (> (second result) 0) (blame (music:chord-symbol-t-stx (first result)) "score failed harmonic analysis")))