#lang racket

(require (for-syntax syntax/parse))

(provide (all-defined-out))

(define-syntax make-struct-t
  (syntax-parser
    [(_ struct-name super-name)
     #'(struct struct-name super-name (stx)
         #:methods
         gen:equal+hash
         [(define (equal-proc a b equal?-recur)
            (equal?/recur (struct-copy super-name a)
                          (struct-copy super-name b)
                          equal?-recur))
          (define (hash-proc a hash-recur)
            (hash-recur (struct-copy super-name a)))
          
          (define (hash2-proc a hash2-recur)
            (hash2-recur (struct-copy super-name a)))])]))

;; A Transition is a (List Symbol Symbol)
;; A Universe is a [dict figure Symbol]

;; A ChordForest is a weighted, directed graph
;; it has a node 'start, a node 'end,
;; and chord-forest-nodes
;; it has edge-property transition-type: 'in-key | 'modulation

(struct chord-forest-node (key symbol index) #:transparent)

;; A cadence is a list of symbol
(struct cadence (key progressions) #:transparent)

(struct phrase (length start-key cadence) #:transparent)

;; A TimeSignature is a pair of numbers
;; A time-signature is a pair of Number
(struct time-signature (beats size) #:transparent)

;; A Name is one of
;; 'A 'B 'C 'D 'E 'F 'G

;; An Accidental is one of
;; 'none 'sharp 'flat 'natural
;; 'sharpsharp 'flatflat

(define num-intervals 12)

;; a PitchNumber is an integer in the range [0, 11]
;; an Inverval is an integer in the range [0, 11]

;; a raw-note is a PitchNumber, an integer octave, a beat, and a duration
(struct raw-note (pitch octave duration beat) #:transparent)
(make-struct-t raw-note-t raw-note)

;; A Scale is a list-of PitchNumber

;; A Chord is a [set raw-note]

;; A figure has a PitchNumber bass and an ordered [List-of Interval]
(struct figure (bass intervals) #:transparent)
(make-struct-t figure-t figure)

;; A figure-in-context has a PitchNumber bass and an ordered [List-of Interval], as well as a beat
;; for reasons unknown, it is completely unrelated to figure/figure-t
(struct fic (bass intervals beat) #:transparent)
(make-struct-t fic-t fic)

;; A pitch-class has a Name and an Accidental
(struct pitch-class (name accidental) #:transparent)
(make-struct-t pitch-class-t pitch-class)

;; a note has a pitch-class, an integer octave, a beat, and a duration
(struct note (pitch-class octave duration beat) #:transparent)
(make-struct-t note-t note)

(define (note-name n)
  (pitch-class-name (note-pitch-class n)))

(define (note-accidental n)
  (pitch-class-accidental (note-pitch-class n)))

(struct rest (duration beat))
(make-struct-t rest-t rest)

(define (duration note/rest)
  ((cond
     [(rest? note/rest)
       rest-duration]
     [(raw-note? note/rest)
      raw-note-duration]
     [else
      note-duration])
   note/rest))

;; macro me
(define (beat note/rest)
  ((cond
     [(rest? note/rest)
       rest-beat]
     [(raw-note? note/rest)
      raw-note-beat]
     [else
      note-beat])
   note/rest))

;; a key-signature, a time-signature, and a [List-of measure]
(struct voice (key time measures) #:transparent)
(make-struct-t voice-t voice)

;; a [List-of note]
(struct measure (notes time) #:transparent)
(make-struct-t measure-t measure)

(define (measure-length measure)
  (define notes (measure-notes measure))
  (define size (time-signature-size (measure-time measure)))
  (define (helper partial-notes)
    (if (null? partial-notes)
        0
        (+ (* size (duration (first partial-notes))) (helper (cdr partial-notes)))))
  (helper notes))

;; type is 'major or 'minor
(struct key-signature (root type) #:transparent)
(make-struct-t key-signature-t key-signature)

;; a chord symbol is a symbol | #f
(struct chord-symbol (s beat) #:transparent)
(make-struct-t chord-symbol-t chord-symbol)