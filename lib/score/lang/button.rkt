#lang racket/base

(provide render-score-button)

(require racket/class
         racket/draw
         racket/runtime-path)

;; render-score.png points to a small image for the button
(define-runtime-path render-score-icon.png "render-score-icon.png")

;; ------------------------------------------------------------------------

;; render-score-action : DrRacket:Unit:Frame -> Void
;; ASSUME this is called within the DrRacket eventspace
(define (render-score-action frame)
  (void))

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

