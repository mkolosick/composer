#lang racket

(require (prefix-in music: "repr.rkt")
         syntax/parse
         (prefix-in p: parsack))

(provide note
         time-denominator
         key-signature
         voices->chords)

(define note-name-parser
  (p:parser-compose (note-name <- (p:oneOf "abcdefgABCDEFG"))
                    (p:return (string->symbol
                               (list->string
                                (list (char-upcase note-name)))))))

(define flat-component-parser
  (p:<or> (p:char #\b)
          (p:char #\♭)))

(define flat-parser
  (p:parser-compose flat-component-parser
                    (p:<any> (p:try (p:>> flat-component-parser
                                          (p:return 'flatflat)))
                             (p:return 'flat))))

(define sharp-component-parser
  (p:<or> (p:char #\#)
          (p:char #\♯)))

(define sharp-parser
  (p:parser-compose sharp-component-parser
                    (p:<any> (p:try (p:>> sharp-component-parser
                                          (p:return 'sharpsharp)))
                             (p:return 'sharp))))

(define nat-parser
  (p:parser-compose (p:<or> (p:string "♮")
                            (p:string "nat"))
                    (p:return 'nat)))

(define accidental-parser
  (p:<any>               
   (p:<or> flat-parser
           sharp-parser
           nat-parser)
   (p:return 'none)))

(define octave-parser
  (p:parser-compose (octave-string <- (p:many1 p:$digit))
                    (p:return (string->number (list->string octave-string)))))

(define (make-note-parser stx)
  (p:parser-compose (note-name <- note-name-parser)
                    (accidental <- accidental-parser)
                    (octave <- octave-parser)
                    (p:return (music:note-t (music:pitch-class note-name accidental) octave stx))))

(define (make-note-string-parser stx)
  (p:<or>
   (p:try (p:parser-compose
           (result <- (p:<or> (p:>> (p:string "rest")
                                    (p:return (music:rest-t stx)))
                              (make-note-parser stx)))
           p:$eof
           (p:return result)))
   (p:return #f)))

;; Stx -> note | rest | #f
(define (note-stx->note note-stx)
  (p:parse-result (make-note-string-parser note-stx)
                  (symbol->string (syntax->datum note-stx))))

;; Stx -> key-signature
(define (key-stx->key key-stx)
  (define key-sym (syntax->datum key-stx))
  (define key-string (symbol->string key-sym))
  (define note-char (list-ref (string->list key-string) 0))
  (define key-type (if (char-upper-case? note-char)
                       'major
                       'minor))
  (define note-name (string->symbol (string-upcase (substring key-string 0 1))))
  (define accidental (p:parse-result accidental-parser (substring key-string 1)))
  (define key-root (music:pitch-class-t note-name accidental key-stx))
  (music:key-signature-t key-root key-type key-stx))
  
(define-syntax-class note
  #:opaque
  #:attributes (note)
  (pattern note-value:id
           #:do [(define note-result (note-stx->note #'note-value))]
           #:fail-unless note-result (format "invalid note: ~a" #'note-value)
           #:attr note note-result))

(define-syntax-class time-denominator
  #:attributes ()
  (pattern time:exact-positive-integer
           #:fail-unless (let ([value (syntax->datum #'time)])
                           (integer? (log value 2)))
           "invalid time signature"))

(define-syntax-class key-signature
  #:attributes (key-signature)
  #:datum-literals (Fb F♭ Cb C♭ Gb G♭ Db D♭ Ab A♭ Eb E♭ Bb B♭
                       F C G D A E B
                       F# F♯ C# C♯ G# G♯ D# D♯ A# A♯ E# E♯ B# B♯
                       fb f♭ cb c♭ gb g♭ db d♭ ab a♭ eb e♭ bb b♭
                       f c g d a e b
                       f# f♯ c# c♯ g# g♯ d# d♯ a# a♯ e# e♯ b# b♯)
  (pattern (~and key (~or Fb F♭ Cb C♭ Gb G♭ Db D♭ Ab A♭ Eb E♭ Bb B♭
                          F C G D A E B
                          F# F♯ C# C♯ G# G♯ D# D♯ A# A♯ E# E♯ B# B♯
                          fb f♭ cb c♭ gb g♭ db d♭ ab a♭ eb e♭ bb b♭
                          f c g d a e b
                          f# f♯ c# c♯ g# g♯ d# d♯ a# a♯ e# e♯ b# b♯))
           #:attr key-signature (key-stx->key #'key)))

(define note-seq
  (list (music:pitch-class 'C 'none)
        (music:pitch-class 'C 'sharp)
        (music:pitch-class 'D 'none)
        (music:pitch-class 'D 'sharp)
        (music:pitch-class 'E 'none)
        (music:pitch-class 'F 'none)
        (music:pitch-class 'F 'sharp)
        (music:pitch-class 'G 'none)
        (music:pitch-class 'G 'sharp)
        (music:pitch-class 'A 'none)
        (music:pitch-class 'A 'sharp)
        (music:pitch-class 'B 'none)))

;; pitch-class -> PitchNumber
(define (pitch-class->PitchNumber pitch-class)
  (define base-index (index-of note-seq (music:pitch-class
                                         (music:pitch-class-name pitch-class)
                                         'none)))
  (+ base-index (accidental-semitones (music:pitch-class-accidental pitch-class))))

;; Note -> Number
(define (staff-index note-name)
  (index-of '(C D E F G A B) note-name))

;; Accidental -> Number
(define (accidental-semitones accidental)
  (match accidental
    ['none 0]
    ['natural 0]
    ['sharp 1]
    ['sharpsharp 2]
    ['flat -1]
    ['flatflat -2]))

;; pitch-class pitch-class -> Boolean
(define (pitch-equal? p1 p2)
  (equal? (pitch-class->PitchNumber p1) (pitch-class->PitchNumber p2)))

;; PitchNumber Integer -> PitchNumber
(define (pitch-add-semitones pitch-number num-semi)
  (modulo (+ pitch-number num-semi)
          (length note-seq)))

;; raw-note Integer -> raw-note
(define (note-add-semitones note num-semi)
  (define pitch (music:raw-note-pitch note))
  (music:raw-note (pitch-add-semitones pitch num-semi)
                  (+ (music:raw-note-octave note)
                     (floor (/ (+ pitch num-semi) 12)))))

;; [List-of A] Number -> [List-of A]
(define (rotate-left xs n)
  (append (list-tail xs n) (take xs n)))

;; [List-of A] Number -> [List-of A]
(define (rotate-right xs n)
  (rotate-left xs (- (length xs) n)))

;; key-signature -> Scale
(define (key-scale key)
  (define steps (match (music:key-signature-type key)
                  ['major '(0 2 4 5 7 9 11)]
                  ['minor '(0 2 3 5 7 8 10)]))
  (rotate-right
   (map (λ (semitones)
          (pitch-add-semitones (pitch-class->PitchNumber (music:key-signature-root key))
                               semitones))
        steps)
   (staff-index (music:pitch-class-name (music:key-signature-root key)))))

(define c-major-scale (key-scale (music:key-signature (music:pitch-class 'C 'none) 'major)))

;; note key-signature -> raw-note
;; converts a note relative to a key to a raw note
(define (note-in-key->raw-note note key)
  (define scale (key-scale key))
  (define base-pitch (list-ref scale (staff-index (music:note-name note))))
  (define raw-note-result (note-add-semitones (music:raw-note base-pitch (music:note-octave note))
                      (if (equal? (music:note-accidental note) 'natural)
                          (let ([scale-difference (map - c-major-scale scale)])
                            (list-ref scale-difference (staff-index (music:note-name note))))
                          (accidental-semitones (music:note-accidental note)))))

  (if (music:note-t? note)
      (music:raw-note-t (music:raw-note-pitch raw-note-result)
                        (music:raw-note-octave raw-note-result)
                        (music:note-t-stx note))
      raw-note-result))

;; [List-of voice] -> [List-of Chord]
(define (voices->chords voices)
  (define smallest-beat-size (apply max (map (λ (voice) (music:time-signature-size (music:voice-time voice)))
                                  voices)))

  (define (voice->note-seq voice)
    (define note-multiplier (/ smallest-beat-size (music:time-signature-size (music:voice-time voice))))
    (flatten (map (λ (measure)
           (map (λ (note)
                  (define raw-note (if (music:rest? note)
                                       note
                                       (note-in-key->raw-note note (music:voice-key voice))))
                  (make-list note-multiplier raw-note))
                (music:measure-notes measure)))
         (music:voice-measures voice))))

  (define voices-seq (map voice->note-seq voices))

  (define (notes->chords voices-seq)
    (define non-empty-seq (filter cons? voices-seq))
    (cond
      [(empty? non-empty-seq) empty]
      [else (cons (list->set (filter music:raw-note? (map first non-empty-seq)))
                  (notes->chords (map rest non-empty-seq)))]))
  
  (notes->chords voices-seq))