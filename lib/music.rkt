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
              (~once (~seq ((~datum pivots) pivot-things ...)))
              (~once (~seq ((~datum phrases) phrase-things ...))))
        ...)
     #:with (universe-hash (chord-name-id ...)) (chord-names #'(chord-names-things ...))
     #:with progressions-hash (progressions #'(progression-things ...))
     #:with pivots-hash (progressions #'(pivot-things ...))
     #:with phrase-list (phrases #'(phrase-things ...))
     
     #'(#%module-begin
        (provide (rename-out [music-module-begin #%module-begin])
                 voice)

        (begin-for-syntax
          (define chord-name-id 'chord-name-id) ...
          (define chord-names universe-hash)
          (define progressions progressions-hash)
          (define pivots pivots-hash)
          (define measure-checkers (list check-measure-length))
          (define phrases phrase-list))

        (define-syntax music-module-begin
          (syntax-parser
            [(_ voice (... ...))
             (define typed-voices (stx-map type-of #'(voice (... ...))))
             (define voices (map second typed-voices))

             (define chords (voices->chords voices))
             
             #;(define
               phrases
               (list
                (music:phrase
                 2
                 (music:key-signature (music:pitch-class 'C 'none) 'major)
                 (music:cadence (music:key-signature (music:pitch-class 'G 'none) 'major) '((V I))))
                (music:phrase
                 2
                 (music:key-signature (music:pitch-class 'G 'none) 'major)
                 (music:cadence (music:key-signature (music:pitch-class 'C 'none) 'major) '((V I) (V7 I))))))

             (define chord-forest (chords->ChordForest chords
                                                       progressions
                                                       pivots
                                                       chord-names))

             (when (and (not (empty? chords)) (not (dict-empty? progressions)))
               (verify-harmonic-progression chord-forest
                                            (music:raw-note-t-stx (set-first (first chords)))))

             (verify-form phrases voices chord-forest)

             (with-syntax ([(voice+ (... ...)) (map first typed-voices)])
               #'(#%module-begin (provide score)
                                 (define score (list voice+ (... ...)))
                                 score))]))
        
        (define-syntax voice
          (syntax-parser
            [(~and voice (_ key numerator:exact-positive-integer denominator:time-denominator measure (... ...)))
             (match-define (list key+ key-type) (type-of #'(key-parser key)))
             (define time-signature (music:time-signature (syntax->datum #'numerator) (syntax->datum #'denominator)))
             (define typed-measures (build-measure-structs (syntax->list #'(measure (... ...))) time-signature 0))

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

  #;(define (phrases stx)
    (syntax-parse stx
      [((length:number key:key-signature ((~datum cadence) cadence-key:key-signature type)) ...)
       (define key-root (music:key-signature-root (attribute key.key-signature)))
       (define cadence-root (music:key-signature-root (attribute cadence-key.key-signature)))
       (with-syntax ([key-pitch (datum->syntax #'stx (music:pitch-class-name key-root))]
                     [key-accidental (datum->syntax #'stx (music:pitch-class-accidental key-root))]
                     [key-type (datum->syntax #'stx (music:key-signature-type (attribute key.key-signature)))]
                     [cadence-pitch (datum->syntax #'stx (music:pitch-class-name cadence-root))]
                     [cadence-accidental (datum->syntax #'stx (music:pitch-class-accidental cadence-root))]
                     [cadence-type (datum->syntax #'stx (music:key-signature-type (attribute cadence-key.key-signature)))])
       #'(list (music:phrase
                'length
                key*
                (music:cadence cadence-key* 'type))
               ...))]))

  #;(music:phrase
     2
     (music:key-signature (music:pitch-class 'C 'none) 'major)
     (music:cadence (music:key-signature (music:pitch-class 'G 'none) 'major) '((V I))))

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

(define-for-syntax (build-measure-structs measures time beat)
  (if (null? measures)
      measures
      (let ([measure (type-of #`(measure-parser #,time #,beat #,(first measures)))])
        (cons measure (build-measure-structs (rest measures) time (+ (music:measure-length (second measure)) beat))))))

(define-for-syntax (build-note-structs notes time beat)
  (if (null? notes)
      notes
      (let ([note (type-of #`(note-parser #,beat #,(first notes)))])
        (cons note (build-note-structs
                    (rest notes)
                    time
                    (+ (* (music:time-signature-size time) (music:duration (second note))) beat))))))

(define-syntax (measure-parser stx)
  (syntax-parse stx
    [(_ time beat (~and measure (n ...)))
     (define typed-notes (build-note-structs (syntax->list #'(n ...)) (syntax->datum #'time) (syntax->datum #'beat)))
     (with-syntax ([(n+ ...) (map first typed-notes)])
       (assign-type #'(music:measure (list n+ ...) time)
                    (music:measure-t (map second typed-notes)
                                     (syntax->datum #'time)
                                     #'measure)))]))

(define-syntax note-parser
  (syntax-parser 
    [(_ beat n:note)
     #'(note-parser beat (1/4 n))]
    [(_ beat (duration n:note))
     (if (music:rest? (attribute n.note))
         (assign-type #'(music:rest 'duration 'beat) (music:rest-t (syntax->datum #'duration) (syntax->datum #'beat) #'n))
         (with-syntax ([note-name (datum->syntax #'n (music:note-name (attribute n.note)))]
                       [note-accidental (datum->syntax #'n (music:note-accidental (attribute n.note)))]
                       [note-octave (datum->syntax #'n (music:note-octave (attribute n.note)))]
                       [note-duration #'duration]
                       [note-beat #'beat])
           (assign-type
            #'(music:note (music:pitch-class 'note-name 'note-accidental)
                          'note-octave
                          'note-duration
                          note-beat) ;;sketchy
            (music:note-t (music:note-pitch-class (attribute n.note)) (music:note-octave (attribute n.note))
                          (syntax->datum #'note-duration)
                          (syntax->datum #'note-beat)
                          #'n))))]))

(define-syntax key-parser
  (syntax-parser
    [(_ k:key-signature)
     (define root-pitch (music:key-signature-root (attribute k.key-signature)))
     (with-syntax ([pitch-name (datum->syntax #'k (music:pitch-class-name root-pitch))]
                   [pitch-accidental (datum->syntax #'k (music:pitch-class-accidental root-pitch))]
                   [key-type (datum->syntax #'k (music:key-signature-type (attribute k.key-signature)))])
       (assign-type
        #'(music:key-signature (music:pitch-class
                                'pitch-name
                                'pitch-accidental)
                               'key-type)
        (attribute k.key-signature)))]))
