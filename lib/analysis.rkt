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

(define major '(0 2 2 1 2 2 2))
(define minor '(0 2 1 2 2 1 2))

(define (scan f base values)
  (if (null? values) values (cons (f base (first values)) (scan f (f base (first values)) (rest values)))))

(define (make-scales type)
  (build-list 12 (λ (n) (scan (λ (x y) (modulo (+ x y) 12)) n type))))

(define scales (append (make-scales major) (make-scales minor)))

(define (chord->figure chord)
  (define (get-root chord acc)
    (cond
      [(null? chord) acc]
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
    (define root (get-root chord))
    (define root-pitch (music:raw-note-pitch root))
    (define chord-no-root (set-remove set root))
    (cons (list root) (map (λ (note) (- (music:raw-note-pitch note) root)))))
         

;; [Listof Chord] [Listof (list ChordNumber ChordNumber)] [Listof (list ChordNumber ChordNumber)] [Listof (list NoteNumber [Listof NoteNumber])] -> Void
(define (check-harmonies chords transitions pivots universe)
  (define figures (map chord->figure chords))
  (println figures))
