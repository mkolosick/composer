#lang scribble/manual
 
@title{Music Lang}

@defmodulelang[music]{
 The @racketmodname[music] package provides tools for writing and analyzing Western music.
}

@section[#:tag "writing"]{Writing Music}

Let's write a song. We will start with a voice in C major and 4/4 time.

@racketblock[(voice C 4 4)]

At the moment this song has no notes so we'll expand it by adding a measure.

@racketblock[(voice C 4 4 (E5 D5 C5 D5))]

A measure is made up of a parenthesized list of notes. Each note is assumed to be of the time
signature's beat value. The syntax for notes is expanded in @secref{writing-syntax-note}.

This is still rather boring so let's expand to additional voices and a second measure.

@racketblock[
 (voice C 4 4 (E5 D5 C5 D5) (D5 E5 F♯5 G5))
 (voice C 4 4 (G4 F4 E4 A4) (G4 G4 A4  B4))
 (voice C 4 4 (C4 B3 C4 A3) (B3 C4 D4  D4))
 (voice C 4 4 (C3 D3 E3 F3) (G3 C3 D3  G3))]

This is the general form for a score in #lang music. The syntax of a score is expanded in @secref{writing-syntax}.

@subsection[#:tag "writing-syntax"]{Syntax}

A #lang music program represents a score made up of a number of @seclink["writing-syntax-voice"]{voices}.
Each voice can have distinct key and time signatures.

@subsubsection[#:tag "writing-syntax-voice"]{Voices}

@defform[(voice key time-numerator time-denominator measure ...)
         #:contracts ([key key-signature?]
                      [time-numerator positive?]
                      [time-denominator power-of-two?]
                      [measure (list/c note?)])]{
 This produces a voice in @racket[key] with time signature @racket[time-numerator/time-denominator].
 The notes in each measure are with respect to the key, so the note @racket[C7] in
 @racketblock[(voice D 1 4 (C7))]
 represents the raw note @racket[C♯7].
}

@subsubsection[#:tag "writing-syntax-key"]{Key Signature}

A key signature is written like a musical note (see @secref{writing-syntax-note}), but without an octave.
In addition, an upper case letters represent major keys while lower case represent the minor keys. So @racket[C♯]
is C♯ major while @racket[c♯] is C♯ minor.

@subsubsection[#:tag "writing-syntax-note"]{Notes}

A measure is made up of standard musical notes with their octave. For example: @racket[E5], @racket[F♯3],
@racket[c♮2], @racket[Cnat10]. Accidentals can be written as any of @code{♭}, @code{b}, @code{♯},
@racketidfont{#}, @code{♮}, or @code{nat}.

@section[#:tag "constraining"]{Defining Musical Forms}

#lang music provides forms to define constraints on music.

@defform[(chord-names (name figure) ...)]{
 @racket[chord-names] defines the notation and set of allowed chords. Each figure is written
 as [bass interval ...]. Each of these is a number that is the number of semitones from C.
 All chords are written as if in C major.
}

@defform[(progressions (chord chord-set) ...)
         #:contracts ([chord-set (or/c (list/c chord?) chord?)])]{
 @racket[progressions] defines the allowed set of chord progressions in any key.
 Each @racket[chord] is allowed to go to every chord in @racket[chord-set].
}

@defform[(pivots (chord chord-set) ...)
         #:contracts ([chord-set (or/c (list/c chord?) chord?)])]{
 @racket[pivots] defines the allowed set of pivots for modulating between keys.
 Each @racket[chord] is allowed to pivot to every chord in @racket[chord-set].
}

These forms must all be present and then the module can be used as a #lang line
for a user to write music fitting this form.

@#reader scribble/comment-reader
(racketmod "theory-homework-1.rkt"
(voice C 4 4 (E5 D5    C5 D5) (D5 E5 F#5 G5) (G5 F#5 F#5 F5)  (E5 D5  D5 C5))
(voice C 4 4 (G4 F4    E4 A4) (G4 G4 A4  B4) (B4 B4  A4  B4)  (C5 A4  B4 G4))
(voice C 4 4 (C4 B3    C4 A3) (B3 C4 D4  D4) (D4 D4  D4  D4)  (E4 D4  D4 E4))
(voice C 4 4 (C3 D3    E3 F3) (G3 C3 D3  G3) (G3 B2  D3  G3)  (C3 F3  G3 C3))
; C major     I  viio6 I6 ii6  V  I  V/V                 V7    I  ii6 V  I
; G major                            V   I    I  iii V   V7/IV
)