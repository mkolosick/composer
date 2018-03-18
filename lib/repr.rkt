#lang racket

;; A Name is one of
;; 'A 'B 'C 'D 'E 'F 'G

;; An Accidental is one of
;; 'none 'sharp 'flat 'natural
;; 'sharpsharp 'flatflat

;; A Scale is a list-of pitch-class

;; An Inversion is one of
;; 'root 'first 'second 'third

;; A ChordType is one of
;; 'major 'minor 'augmented 'dimished

;; A pitch-class has a Name and an Accidental
(struct pitch-class (name accidental))

;; a note has a pitch-class and an integer octave
(struct note (pitch-class octave))

;; an Extension is a number and an accidental
(struct extension (degree accidental))

;; a chord-number is a number in the range [1, 7],
;; an Accidental, a ChordType, an Inversion,
;; and a listof Extension
(struct chord-number (number accidental type inversion extensions))
