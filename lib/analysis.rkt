#lang racket

(require (prefix-in music: "repr.rkt")
         "types.rkt")

(provide (all-defined-out))

;; measure-t TimeSignature -> Void
;; blames the measure when the measure is too long
(define (check-measure-length measure time-signature)
  (define measure-length (length (music:measure-notes measure)))
  (define num-beats (first time-signature))
  (define stx (music:measure-t-stx measure))
  
  (when (> measure-length num-beats) (blame stx "measure too long"))
  (when (< measure-length num-beats) (blame stx "measure too short")))

(define (chord->figure chord)
  (define (get-root chord acc)
    (cond
      [(set-empty? chord) acc]
      [else
       (define note (set-first chord))
       (get-root (set-rest chord)
                 (cond
                   [(not acc) note]
                   [(= (music:raw-note-octave acc) (music:raw-note-octave note))
                    (if
                     (< (music:raw-note-pitch note)
                        (music:raw-note-pitch acc))
                     note
                     acc)]
                   [(> (music:raw-note-octave acc) (music:raw-note-octave note)) note]
                   [else acc]))]))
  (define root (get-root chord #f))
  (define root-pitch (music:raw-note-pitch root))
  (define chord-no-root (set->list (set-remove chord root)))
  (list root-pitch (sort (map (λ (note) (modulo (- (music:raw-note-pitch note) root-pitch) 12)) chord-no-root) <)))

;; [Listof Chord] [Listof (list ChordNumber ChordNumber)]
;; [Listof (list ChordNumber ChordNumber)]
;; [Listof (list NoteNumber [Listof NoteNumber])] -> Void
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
                        (define roman (assoc proto-numeral universe))
                        (and roman (second roman)))
                      (second numeral-list)))) proto-numerals))

  (define (correct? numerals)
    (println numerals)
    (or (null? (rest numerals))
        (and (member (list (first numerals) (second numerals)) transitions)
             (correct? (rest numerals)))))

  (define valid-numerals (filter correct? (map second numerals)))
  (println valid-numerals)
  (not (null? valid-numerals)))