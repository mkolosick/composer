#lang racket

(provide (all-defined-out))

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
(struct raw-note (pitch octave) #:transparent)
(struct raw-note-t raw-note (stx))

;; A Scale is a list-of PitchNumber

;; A Chord is a [set raw-note]

;; A figure has a PitchNumber bass and an ordered [List-of Interval]
(struct figure (bass intervals) #:transparent)
(struct figure-t figure (stx))

;; A pitch-class has a Name and an Accidental
(struct pitch-class (name accidental) #:transparent)
(struct pitch-class-t pitch-class (stx))

;; a note has a pitch-class and an integer octave
(struct note (pitch-class octave) #:transparent)
(struct note-t note (stx))

(define (note-name n)
  (pitch-class-name (note-pitch-class n)))

(define (note-accidental n)
  (pitch-class-accidental (note-pitch-class n)))

(struct rest ())
(struct rest-t rest (stx))

;; a key-signature, a time-signature, and a [List-of measure]
(struct voice (key time measures) #:transparent)
(struct voice-t voice (stx))

;; a [List-of note]
(struct measure (notes) #:transparent)
(struct measure-t measure (stx))

;; type is 'major or 'minor
(struct key-signature (root type) #:transparent)
(struct key-signature-t key-signature (stx))

;; a chord symbol is a symbol
(struct chord-symbol (symbol))
(struct chord-symbol-t chord-symbol (stx))