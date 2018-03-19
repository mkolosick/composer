#lang racket

(require (prefix-in music: "repr.rkt")
         syntax/parse)

(provide note
         time-denominator
         key-signature
         key-note->note)

;; String -> note | rest | #f
(define (note-string->note str)
  (match str
    ["rest" (music:rest)]
    [(regexp #rx"^([a-gA-G])(.*)" (list _ note-value rest))
     (define note-symbol (string->symbol (string-upcase note-value)))
     (match rest
       [(regexp #rx"^(##|#|♯♯|♯|bb|b|♭♭|♭|nat|♮)?(.*)" (list _ accidental-value rest))
        (define accidental-symbol (if accidental-value
                                      (accidental-string->symbol accidental-value)
                                      'none))
        (define note-part (music:pitch-class note-symbol accidental-symbol))
        (match rest
          [(regexp #rx"^([0-9]+)$" (list _ octave-string))
           (music:note note-part (string->number octave-string))]
          [_ #f])]
       [_ #f])]
    [_ #f]))

;; String -> Accidental
(define (accidental-string->symbol str)
  (match str
    [(or "#" "♯") 'sharp]
    [(or "b" "♭") 'flat]
    [(or "nat" "♮") 'natural]
    [(or "##" "♯♯") 'sharpsharp]
    [(or "bb" "♭♭") 'flatflat]
    [_ 'none]))
  
(define-syntax-class note
  #:opaque
  #:attributes (note)
  (pattern note-value:id
           #:do [(define note-string (symbol->string (syntax->datum #'note-value)))
                 (define note-result (note-string->note note-string))]
           #:fail-unless note-result (format "invalid note: ~a" note-string)
           #:attr note note-result))

#;(define (note->syntax stx note-struct)
    (if (music:rest? note-struct)
        #'(music:rest)
        (with-syntax ([note-name (datum->syntax #'stx (music:note-name note-struct))]
                      [note-accidental (datum->syntax #'stx (music:note-accidental note-struct))]
                      [note-octave (datum->syntax #'stx (music:note-octave note-struct))])
          #'(music:note (music:pitch-class 'note-name 'note-accidental)
                        note-octave))))

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
           #:attr key-signature (key-symbol->key (syntax->datum #'key))))

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

;; Note -> Number
(define (staff-index note-name)
  (index-of '(C D E F G A B C) note-name))

;; Accidental -> Number
(define (accidental-semitones accidental)
  (match accidental
    ['none 0]
    ['natural 0]
    ['sharp 1]
    ['sharpsharp 2]
    ['flat -1]
    ['flatflat -2]))

;; pitch-class -> pitch-class
(define (normalize-pitch pitch-class)
  (define base-index (index-of note-seq (music:pitch-class
                                         (music:pitch-class-name pitch-class)
                                         'none)))
  (define pitch-index (+ base-index
                         (accidental-semitones (music:pitch-class-accidental pitch-class))))
  (list-ref note-seq (modulo pitch-index (length note-seq))))

;; pitch-class pitch-class -> Boolean
(define (pitch-equal? p1 p2)
  (equal? (normalize-pitch p1) (normalize-pitch p2)))

;; pitch-class Number -> pitch-class
(define (pitch-add-semitones pitch-class num-semi)
  (define normalized (normalize-pitch pitch-class))
  (define pitch-index (index-of note-seq normalized))
  (list-ref note-seq
            (modulo (+ pitch-index num-semi)
                    (length note-seq))))

;; note Number -> note
(define (note-add-semitones note num-semi)
  (define base-index (index-of note-seq (music:pitch-class
                                         (music:note-name note)
                                         'none)))
  (define accidental-distance (accidental-semitones (music:note-accidental note)))
  (define start-octave
    (cond
      [(< (+ base-index accidental-distance) 0)
       (- (music:note-octave note) 1)]
      [(> (+ base-index accidental-distance) (length note-seq))
       (+ (music:note-octave note) 1)]
      [else (music:note-octave note)]))
  (define normalized-pitch (normalize-pitch (music:note-pitch-class note)))
  (define-values (octaves-up semitones-up) (quotient/remainder num-semi (length note-seq)))
  (music:note (pitch-add-semitones normalized-pitch semitones-up)
              (+ start-octave octaves-up)))

;; Symbol -> key-signature
(define (key-symbol->key key-sym)
  (define key-string (symbol->string key-sym))
  (define note-char (list-ref (string->list key-string) 0))
  (define key-type (if (char-upper-case? note-char)
                       'major
                       'minor))
  (define note-name (string->symbol (string-upcase (substring key-string 0 1))))
  (define accidental (accidental-string->symbol (substring key-string 1)))
  (define key-root (music:pitch-class note-name accidental))
  (music:key-signature key-root key-type))

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
  (rotate-right (map (λ (semitones)
                       (pitch-add-semitones (music:key-signature-root key) semitones))
                     steps)
                (staff-index (music:pitch-class-name (music:key-signature-root key)))))


;; note key-signature -> note
;; converts a note relative to a key to a raw note
(define (key-note->note note key)
  (define scale (key-scale key))
  (define base-pitch (list-ref scale (staff-index (music:note-name note))))
  (note-add-semitones (music:note base-pitch (music:note-octave note))
                      (accidental-semitones (music:note-accidental note))))
