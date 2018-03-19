#lang racket

(provide assign-type
         get-type
         type-of)

(define TYPE-TAG 'music-type)

;; assign-type : Stx Type -> Term
;; Creates a Term from a Stx by attaching the given Type.
(define (assign-type e t)
  (syntax-property (local-expand e 'expression null)
                   TYPE-TAG t))

;; get-type : Term -> Type
;; Returns the TYPE-TAG property from the given Term.
(define (get-type e)
  (syntax-property e TYPE-TAG))

 ; type-of : Stx -> [List Term Type]
 ; Computes a Type and Term from a given Stx.
(define (type-of e)
  (define e+ (local-expand e 'expression null))
  (list e+ (get-type e+)))

