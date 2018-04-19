#lang racket/base

(provide render-score-button)

(require racket/class
         racket/draw
         racket/runtime-path
         racket/lazy-require
         racket/path)
(lazy-require
 ["gui.rkt"
  (execute-callback-with-rep:after-expression
   message-box)])

;; render-score.png points to a small image for the button
(define-runtime-path render-score-icon.png "render-score-icon.png")

;; ------------------------------------------------------------------------

;; file-render-score : Path Score -> Void

;; file  : a path representing the *source* file that the score comes from
;; score : the score value provided by the module as `score`

;; Renders the `score` to a different file with a path relative to `file`.
(define (file-render-score file score)
  ;; The destination file is `music-notation/<name>.xml` relative to the
  ;; `file`.
  (define file-dir (path-only file))
  (define file-name (file-name-from-path file))
  (define destination
    (path-add-extension
     (build-path file-dir "music-notation" file-name)
     #".xml"))
  
  ;; TODO: render the score
  (void))

;; ------------------------------------------------------------------------

;; render-score-action : DrRacket:Unit:Frame -> Void
;; ASSUME this is called within the DrRacket eventspace
(define (render-score-action frame)
  ;; this "Render Score" button action is based on Scribble's
  ;; "Render Scribble" button actions, from
  ;; drracket/scribble/tools/drracket-buttons.rkt
  (define dtext (send frame get-definitions-text))
  (define filename (send dtext get-filename))
  (cond
    [filename
     (execute-callback-with-rep:after-expression
      frame
      (λ ()
        ;; This thunk will be evaluated in the namespace of
        ;; the module, just like the Repl.
        (define score
          (or
           (with-handlers ([exn:fail? (λ (x) #f)])
             (dynamic-require (eval #'(variable-reference->module-path-index
                                       (#%variable-reference)))
                              'score))
           (with-handlers ([exn:fail? (λ (x) #f)])
             (eval 'score))))
        ;; If (dynamic-require ... 'score) and (eval 'score) both go wrong,
        ;; then we assume that's because of an earlier failure, so we just
        ;; don't do anything.
        (when score
          (printf "rendering score\n")
          ;; Render the score here
          (file-render-score filename score))))]
    [else
     (message-box "Music" "Cannot render a music score without a filename")]))

;; ------------------------------------------------------------------------

;; A ButtonSpec is a:
#; (List String
         Bitmap  ;16x16 pixels
         (-> Drracket:Unit:Frame Any) ;called when button is clicked
         (Maybe Real)) ; passed as #:number arg to register-toolbar-button

(define render-score-button
  (list "Render Score"
        (read-bitmap render-score-icon.png)
        render-score-action
        #f))

;; ------------------------------------------------------------------------

