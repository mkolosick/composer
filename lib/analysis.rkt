#lang racket

(require (prefix-in music: "repr.rkt")
         "types.rkt"
         "notes.rkt"
         graph)

(provide (all-defined-out))

;; a MeasureChecker is a function measure-t time-signature key-signature -> Void

;; MeasureChecker
;; blames the measure when the measure is too long
(define (check-measure-length measure time-signature key-signature)
  (define measure-length (music:measure-length measure))
  (define num-beats (music:time-signature-beats time-signature))
  (define stx (music:measure-t-stx measure))
  
  (when (> measure-length num-beats) (blame stx "measure too long"))
  (when (< measure-length num-beats) (blame stx "measure too short")))

(define (verify-form phrases voices graph)
  (define
    remaining-measures
    (foldl (λ (phrase remaining-measures)
             (verify-phrase
              phrase
              remaining-measures
              graph))
           voices
           phrases))
  (unless (null? (music:voice-measures (first remaining-measures)))
    (blame (music:voice-t-stx (first voices)) "Too long for form constraints")))

(define (verify-phrase phrase voices graph)
  (define vertices (get-vertices graph))
  (define phrase-key (music:phrase-start-key phrase))
  (define maybe-start
    (filter
     (λ (vertex)
       (and
        (not (symbol? vertex))
        (equal?
         (music:chord-forest-node-key vertex)
         phrase-key)
        (equal?
         (music:chord-symbol-beat (music:chord-forest-node-symbol vertex))
         ;; this may cause trouble depending on how we implement ties over barines
         ;; also it makes me want to throw up, aesthetically speaking.
         (music:beat (first (music:measure-notes (first (music:voice-measures (first voices)))))))))
     vertices))
  (when (null? maybe-start)
    (blame (music:measure-t-stx (first (music:voice-measures (first voices))))
           (format "Incorrect start key. Expected: ~a ~a"
                   (music:pitch-class-name (music:key-signature-root phrase-key))
                   (music:key-signature-type phrase-key))))
  (define trimmed-voices (discard-measures voices (- (music:phrase-length phrase) 1)))
  ;; Gross oversimplification of cadences, we only handle cadences on the last beat of the specified measure.
  (define this-measure-last-beat
    (argmax music:note-beat
            (map
             (λ (voice)
               (last (music:measure-notes (first (music:voice-measures voice)))))
             trimmed-voices)))
  (define maybe-vertex
    (filter
     (λ (vertex)
       (and
        (not (symbol? vertex))
        (equal?
         (music:chord-forest-node-key vertex)
         (music:cadence-key (music:phrase-cadence phrase)))
        (equal?
         (music:chord-symbol-beat (music:chord-forest-node-symbol vertex))
         (music:note-beat this-measure-last-beat))))
     vertices))
  (unless
      (and
       (not (empty? maybe-vertex))
       (ormap
        (λ (cadence) (verify-cadence (reverse cadence) (music:cadence-key (music:phrase-cadence phrase)) (first maybe-vertex) graph))
        (music:cadence-progressions (music:phrase-cadence phrase))))
    (blame (music:note-t-stx this-measure-last-beat) "Cadence specified in form was not found."))
  (discard-measures trimmed-voices 1))

(define (verify-cadence cadence key vertex graph)
  (cond
    [(null? cadence) #t]
    [(not (equal?
           (music:chord-symbol-s (music:chord-forest-node-symbol vertex))
           (first cadence))) #f]
    [else
     (define vertices (foldl (λ (edge acc)
                               (if (equal? (second edge) vertex) (cons (first edge) acc) acc))
                             '() (get-edges graph)))
     (define maybe-next
       (filter
        (λ (vertex)
          (and
           (not (symbol? vertex))
           (equal?
            (music:chord-forest-node-key vertex)
            key)
           vertices))
        vertices))
     (or (null? (rest cadence))
         (and (not (null? maybe-next))
              (verify-cadence (rest cadence) key (first maybe-next) graph)))]))

(define (discard-measures voices n)
  (define measures (map music:voice-measures voices))
  (define (helper measures n)
    (cond
      [(zero? n) measures]
      [(null? measures)
       (blame
        (music:measure-t-stx
         (last (music:voice-measures (first voices)))
         (format "Missing ~a measures to satisfy form constraints." n)))]
      [else (helper (map rest measures) (sub1 n))]))
  (map
   (λ (voice ms)
     (music:voice (music:voice-key voice) (music:voice-time voice) ms))
   voices
   (helper measures n)))

;; chord-symbol chord-symbol
;; [dict Symbol [set Symbol]]
;; -> Boolean
(define (valid-transition? chord1 chord2 transitions)
  (or
   (set-member? (dict-ref transitions (music:chord-symbol-s chord1) (set))
                (music:chord-symbol-s chord2))
   (equal? chord1 chord2)))

;; [Listof (list key-signature [Listof chord-symbol-t])]
;; [dict Symbol [set Symbol]]
;; [dict Symbol [set Symbol]]
;; -> ChordForest
(define (build-chord-forest symbols-in-keys transitions pivots)
  (define g (weighted-graph/directed '()))
  (define-edge-property g transition-type)
  (add-vertex! g 'start)
  (add-vertex! g 'end)
  
  ;; key-signature [Listof chord-symbol-t]
  ;; [Listof (list key-signature [Listof chord-symbol-t])]
  ;; Number
  ;; -> Void
  ;; adds all of the symbols from symbols-in-key to g with transitions to
  ;; pivot chords in other keys and valid transitions in this key
  (define (add-transitions-from-key key symbols-list other-key-symbols index)
    (cond
      ;; if we are at the end and there is a chord symbol
      ;; then add an edge to 'end
      [(empty? (rest symbols-list))
       (when (music:chord-symbol-s (first symbols-list))
         (define node (music:chord-forest-node key (first symbols-list) index))
         (add-directed-edge! g node 'end 0))]
      [(cons? (rest symbols-list))
       (define chord1 (first symbols-list))
       (define chord2 (second symbols-list))

       (when (music:chord-symbol-s chord1)
         (define node1 (music:chord-forest-node key chord1 index))
         (add-vertex! g node1)
         (when (zero? index)
           (add-directed-edge! g 'start node1 0))
         (when (and (music:chord-symbol-s chord2) (valid-transition? chord1 chord2 transitions))
           (define node2 (music:chord-forest-node key chord2 (+ index 1)))
           (add-directed-edge! g node1 node2 1)
           (transition-type-set! node1 node2 'in-key))

         (add-pivots key (first symbols-list)
                     (map (λ (other-key-symbol)
                            (list (first other-key-symbol)
                                  (first (second other-key-symbol))))
                          other-key-symbols)
                     index))
       
       (add-transitions-from-key key (rest symbols-list)
                                 (map (λ (other-key-symbol)
                                        (list (first other-key-symbol)
                                              (rest (second other-key-symbol))))
                                      other-key-symbols)
                                 (+ index 1))]))

  ;; key-signature chord-symbol-t
  ;; [Listof (list key-signature chord-symbol-t)]
  ;; Number
  ;; -> Void
  ;; adds all vertical pivots
  (define (add-pivots key symbol other-key-symbols index)
    (when symbol
      (for ([other-key-symbol other-key-symbols])
        (match-define (list other-key other-symbol) other-key-symbol)
        (when (and other-symbol (valid-transition? symbol other-symbol pivots))
          (define node1 (music:chord-forest-node key symbol index))
          (define node2 (music:chord-forest-node other-key other-symbol index))
          (add-directed-edge! g node1 node2 0)
          (transition-type-set! node1 node2 'modulation)))))

  (for-each (λ (symbols-in-key)
              (add-transitions-from-key (first symbols-in-key) (second symbols-in-key)
                                        (remove symbols-in-key symbols-in-keys)
                                        0))
            symbols-in-keys)
  
  g)

;; [Listof Chord]
;; [dict Symbol [set Symbol]]
;; [dict Symbol [set Symbol]]
;; Universe
;; -> ChordForest
(define (chords->ChordForest chords transitions pivots universe)
  (define chord-symbols (make-chord-symbols-in-keys chords universe))
  (build-chord-forest chord-symbols transitions pivots))

;; [Listof Chord] Universe -> [Listof (list key-signature [Listof fic-t])]
(define (make-chord-symbols-in-keys chords universe)
  (define figures (map chord->figure (filter (compose not set-empty?) chords)))

  ;; 'major | 'minor -> [Listof (list key-signature [Listof fic-t])]
  (define (gen-numerals type)
    (build-list
     12
     (λ (n) (list
             (music:key-signature
              (PitchNumber->pitch-class n)
              type)
             (map (λ (figure)
                    (music:fic-t
                     (modulo (- (music:fic-bass figure) n) 12)
                     (music:fic-intervals figure)
                     (music:fic-beat figure)
                     (music:fic-t-stx figure)))
                  figures)))))

  (define proto-numerals
    (append (gen-numerals 'major) (gen-numerals 'minor)))

  (map (λ (numeral-list)
         (list (first numeral-list)
               (map (λ (proto-numeral)
                      (music:chord-symbol-t
                       (dict-ref
                        universe
                        (music:figure
                         (music:fic-bass proto-numeral)
                         (music:fic-intervals proto-numeral))
                        #f)
                       (music:fic-beat proto-numeral)
                       (music:fic-t-stx proto-numeral)))
                    (second numeral-list))))
       proto-numerals))

;; ChordForest -> Void
(define (verify-harmonic-progression chord-forest first-blame-stx)
  (define-values (distance-dict pred-dict)
    (dag-shortest-paths chord-forest 'start))
  (define distance-to-end (dict-ref distance-dict 'end))

  (when (infinite? distance-to-end)
    (define farthest-chord
      (argmax second
              (sequence->list
               (sequence-filter (λ (distance-pair)
                                  (not (infinite? (second distance-pair))))
                                (in-values-sequence (in-dict distance-dict))))))
    (define blame-stx (if (equal? (first farthest-chord) 'start)
                          first-blame-stx
                          (music:chord-symbol-t-stx
                           (music:chord-forest-node-symbol (first farthest-chord)))))
    (blame blame-stx "could not find progression to next chord")))