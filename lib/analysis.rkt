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
  (define measure-length (length (music:measure-notes measure)))
  (define num-beats (music:time-signature-beats time-signature))
  (define stx (music:measure-t-stx measure))
  
  (when (> measure-length num-beats) (blame stx "measure too long"))
  (when (< measure-length num-beats) (blame stx "measure too short")))

;; chord-symbol chord-symbol
;; [dict Symbol [set Symbol]]
;; -> Boolean
(define (valid-transition? chord1 chord2 transitions)
  (set-member? (dict-ref transitions (music:chord-symbol-s chord1) (set))
               (music:chord-symbol-s chord2)))

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

;; [Listof Chord] Universe -> [Listof (list key-signature [Listof figure-t])]
(define (make-chord-symbols-in-keys chords universe)
  (define figures (map chord->figure chords))

  ;; 'major | 'minor -> [Listof (list key-signature [Listof figure-t])]
  (define (gen-numerals type)
    (build-list
     12
     (λ (n) (list
             (music:key-signature
              (PitchNumber->pitch-class n)
              type)
             (map (λ (figure)
                    (music:figure-t
                     (modulo (- (music:figure-bass figure) n) 12)
                     (music:figure-intervals figure)
                     (music:figure-t-stx figure)))
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
                         (music:figure-bass proto-numeral)
                         (music:figure-intervals proto-numeral))
                        #f)
                       (music:figure-t-stx proto-numeral)))
                    (second numeral-list))))
       proto-numerals))

;; ChordForest -> Void
(define (verify-harmonic-progression chord-forest)
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
    (blame (music:chord-symbol-t-stx (music:chord-forest-node-symbol (first farthest-chord))) "could not find progression to next chord")))