#lang racket

(require (for-syntax syntax/parse
                     "notes.rkt"
                     (prefix-in music: "repr.rkt")
                     syntax/stx
                     syntax/id-set
                     "types.rkt"
                     racket
                     "analysis.rkt")
         (prefix-in music: "repr.rkt"))

(provide
 (rename-out [constraint-module-begin #%module-begin]))
     
(define-syntax constraint-module-begin
  (syntax-parser
    [(_ (~alt (~once (~seq ((~datum chord-names) chord-names-things ...)))
              (~once (~seq ((~datum progressions) progression-things ...)))
              (~once (~seq ((~datum pivots) pivot-things ...))))
        ...)
     #:with (universe-hash (chord-name-id ...)) (chord-names #'(chord-names-things ...))
     #:with progressions-hash (progressions #'(progression-things ...))
     #:with pivots-hash (progressions #'(pivot-things ...))
     
     #'(#%module-begin
        (provide (rename-out [music-module-begin #%module-begin])
                 ; needed for the "Render Score" button for some reason
                 #%top-interaction
                 voice)

        (begin-for-syntax
          (define chord-name-id 'chord-name-id) ...
          (define chord-names universe-hash)
          (define progressions progressions-hash)
          (define pivots pivots-hash)
          (define measure-checkers (list check-measure-length)))

        (define-syntax music-module-begin
          (syntax-parser
            [(_ voice (... ...))
             (define typed-voices (stx-map type-of #'(voice (... ...))))
             (define voices (map second typed-voices))

             (define chords (voices->chords voices))

             (define chord-forest (chords->ChordForest chords
                                                       progressions
                                                       pivots
                                                       chord-names))

             (when (not (empty? chords))
               (verify-harmonic-progression chord-forest
                                            (music:raw-note-t-stx (set-first (first chords)))))
                                                                     

             (with-syntax ([(voice+ (... ...)) (map first typed-voices)])
               #'(#%module-begin (provide score)
                                 (define score (list voice+ (... ...)))
                                 score))]))
        
        (define-syntax voice
          (syntax-parser
            [(~and voice (_ key numerator:exact-positive-integer denominator:time-denominator measure (... ...)))
             (match-define (list key+ key-type) (type-of #'(key-parser key)))
             (define typed-measures (stx-map type-of #'((measure-parser measure) (... ...))))
             (define time-signature (music:time-signature (syntax->datum #'numerator) (syntax->datum #'denominator)))

             (for ([measure-checker measure-checkers])
                  (for ([measure (map second typed-measures)])
                       (measure-checker measure time-signature key-type)))
              
             (with-syntax ([(measure+ (... ...)) (map first typed-measures)]
                           [key+ key+])
               (assign-type
                #'(music:voice key+ (music:time-signature 'numerator 'denominator) (list measure+ (... ...)))
                (music:voice-t key-type time-signature
                               (map second typed-measures)
                               #'voice)))])))]))

(begin-for-syntax
  (define (chord-names stx)
    (syntax-parse stx
      [((~seq chord-name:id (~and figure
                                  (bass:exact-nonnegative-integer
                                   interval:exact-nonnegative-integer ...))) ...)
       (list
        #`(hash #,@(intersperse
                    (syntax->list #'((music:figure 'bass '(interval ...)) ...))
                    (syntax->list #'(chord-name ...))))
        (bound-id-set->list (immutable-bound-id-set (syntax->list #'(chord-name ...)))))]))

  ;; add some free-indentifier-set stuff to prevent duplicates
  (define (progressions stx)
    (syntax-parse stx
      [((start-chord:id (~and rhs (~or _:id
                                       (_:id _:id ...)))) ...)

       #:with (rhs-list ...)
       (map (λ (rhs-thing)
              (if (stx-list? rhs-thing)
                  #`(list #,@(syntax->list rhs-thing))
                  #`(list #,rhs-thing)))
            (syntax->list #'(rhs ...)))
       #`(hash #,@(intersperse
                     (syntax->list #'(start-chord ...))
                     (syntax->list #'(rhs-list ...))))]))

  (define (intersperse as bs)
    (foldr (λ (a b result)
             (cons a (cons b result)))
           '() as bs)))

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
