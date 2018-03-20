#lang racket

(provide (all-defined-out))

;; A Name is one of
;; 'A 'B 'C 'D 'E 'F 'G

;; An Accidental is one of
;; 'none 'sharp 'flat 'natural
;; 'sharpsharp 'flatflat

;; a PitchNumber is an integer in the range [0, 11]

;; a raw-note is a PitchNumber and an integer octave
(struct raw-note (pitch octave) #:transparent)

;; A Scale is a list-of PitchNumber

;; A Chord is a [set PitchNumber]

;; A pitch-class has a Name and an Accidental
(struct pitch-class (name accidental) #:transparent)

;; a note has a pitch-class and an integer octave
(struct note (pitch-class octave) #:transparent)

(define (note-name n)
  (pitch-class-name (note-pitch-class n)))

(define (note-accidental n)
  (pitch-class-accidental (note-pitch-class n)))

(struct rest ())

;; a key-signature, a pair (number number), and a listof listof note
(struct voice (key time measures) #:transparent)

;; type is 'major or 'minor
(struct key-signature (root type) #:transparent)