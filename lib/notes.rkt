#lang racket

(require (for-syntax syntax/parse
                     racket
                     (prefix-in music: "repr.rkt"))
         (prefix-in music: "repr.rkt"))

(begin-for-syntax
  ;; returns a note or a rest or #f
  (define (note-string->note str)
    (match str
      ["rest" (music:rest)]
      [(regexp #rx"^([a-gA-G])(.*)" (list _ note-value rest))
       (define note-symbol (string->symbol note-value))
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

  (define (accidental-string->symbol str)
    (match str
      [(or "#" "♯") 'sharp]
      [(or "b" "♭") 'flat]
      [(or "nat" "♮") 'natural]
      [(or "##" "♯♯") 'sharpsharp]
      [(or "bb" "♭♭") 'flatflat]))
  
  (define-syntax-class note
    #:opaque
    #:attributes (note)
    (pattern note-value:id
             #:do [(define note-string (symbol->string (syntax->datum #'note-value)))
                   (define note-result (note-string->note note-string))]
             #:fail-unless note-result (format "invalid note: ~a" note-string)
             #:attr note note-result))

  (define (note->syntax stx note-struct)
    (if (music:rest? note-struct)
        (datum->syntax #'stx #'(music:rest))
        (with-syntax ([note-name (datum->syntax #'stx (music:note-name note-struct))]
                      [note-accidental (datum->syntax #'stx (music:note-accidental note-struct))]
                      [note-octave (datum->syntax #'stx (music:note-octave note-struct))])
          (datum->syntax #'stx
                         #'(music:note (music:pitch-class 'note-name 'note-accidental)
                                       note-octave))))))

(define-syntax bar
  (syntax-parser
    [(_ foo:note)
     #:with foo-syntax (note->syntax #'foo (attribute foo.note))
     #'foo-syntax]))
