#lang racket

(require (for-syntax syntax/parse
                     "notes.rkt"
                     (prefix-in music: "repr.rkt")
                     syntax/stx
                     "types.rkt"
                     racket
                     "constraints.rkt")
         (prefix-in music: "repr.rkt"))

(provide
 (rename-out [music-module-begin #%module-begin])
 voice)

(define-syntax music-module-begin
  (syntax-parser
    [(_ voice ...)
     (define typed-voices (stx-map type-of #'(voice ...)))
     #;(println (voices->chords (map second typed-voices)))
     (with-syntax ([(voice+ ...) (map first typed-voices)])
       #'(#%module-begin (provide score)
                         (define score (list voice+ ...))
                         score))]))

(define-syntax voice
  (syntax-parser
    [(~and voice (_ key numerator:exact-positive-integer denominator:time-denominator measure ...))
     (match-define (list key+ key-type) (type-of #'(key-parser key)))
     (define typed-measures (stx-map type-of #'((measure-parser measure) ...)))
     (define time-signature (list (syntax->datum #'numerator) (syntax->datum #'denominator)))
     (map (λ (typed-measure) (check-measure-length (second typed-measure) time-signature)) typed-measures)
     (with-syntax ([(measure+ ...) (map first typed-measures)]
                   [key+ key+])
       (assign-type
        #'(music:voice key+ '(numerator denominator) (list measure+ ...))
        (music:voice-t key-type time-signature
                     (map second typed-measures)
                     #'voice)))]))

(define-syntax measure-parser
  (syntax-parser
    [(_ (~and measure (n ...)))
     (define typed-notes (stx-map type-of #'((note-parser n) ...)))
     (with-syntax ([(n+ ...) (map first typed-notes)])
       (assign-type #'(music:measure (list n+ ...))
                    (music:measure-t (map second typed-notes)
                                     #'measure)))]))

(define-syntax note-parser
    (syntax-parser
      [(_ n:note)
       (if (music:rest? (attribute n.note))
           (assign-type #'(music:rest) (music:rest-t #'n))
           (with-syntax ([note-name (datum->syntax #'n (music:note-name (attribute n.note)))]
                         [note-accidental (datum->syntax #'n (music:note-accidental (attribute n.note)))]
                         [note-octave (datum->syntax #'n (music:note-octave (attribute n.note)))])
             (assign-type
              #'(music:note (music:pitch-class 'note-name 'note-accidental)
                            'note-octave)
              (attribute n.note))))]))

(define-syntax key-parser
  (syntax-parser
    [(_ k:key-signature)
     (define root-pitch (music:key-signature-root (attribute k.key-signature)))
     (with-syntax ([pitch-name (datum->syntax #'k (music:pitch-class-name root-pitch))]
                   [pitch-accidental (datum->syntax #'k (music:pitch-class-accidental root-pitch))]
                   [key-type (datum->syntax #'k (music:key-signature-type (attribute k.key-signature)))])
       (assign-type
        #'(music:key-signature (music:pitch-class 'pitch-name 'pitch-accidental)
                               'key-type)
        (attribute k.key-signature)))]))
