#lang racket

(provide (all-defined-out))

;; A TimeSignature is a pair of numbers

;; A Name is one of
;; 'A 'B 'C 'D 'E 'F 'G

;; An Accidental is one of
;; 'none 'sharp 'flat 'natural
;; 'sharpsharp 'flatflat

;; a PitchNumber is an integer in the range [0, 11]

;; a raw-note is a PitchNumber and an integer octave
(struct raw-note (pitch octave) #:transparent)
(struct raw-note-t raw-note (stx))

;; A Scale is a list-of PitchNumber

;; A Chord is a [set PitchNumber]

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

;; a key-signature, a TimeSignature, and a [List-of measure]
(struct voice (key time measures) #:transparent)
(struct voice-t voice (stx))

;; a [List-of note]
(struct measure (notes) #:transparent)
(struct measure-t measure (stx))

;; type is 'major or 'minor
(struct key-signature (root type) #:transparent)
(struct key-signature-t key-signature (stx))