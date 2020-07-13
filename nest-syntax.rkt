#lang racket

;; NEST:

(provide NEST)  ;; Language 

(require redex  pict racket/hash)

;; NEST's BNF grammar 
(define-language NEST
  (e ::= 
     (pattern e e)        ;; define a join pattern
     (reaction e e)       ;; define a reaction
     (actor e)            ;; define an actor
     (react-to e e)       ;; bind a particular reaction a to pattern
     (remove e e)         ;; remove a particular reaction from a pattern
     (remove-reactions e) ;; remove all reactions associated to a pattern
     (send e e)           ;; send a message to an actor 
     (when e e)           ;; sends a message 'm' to an actor 'x' and continues 
     (aop e e)            ;; arithmetic operations
     (lop e e)            ;; logic operations
     (cop e e)            ;; comparison operations
     (if e e e)
     (seq e e)
     (λ [e ...] e)       ;; define a lambda
     (let (x e) in e)
     (e ...)
     x
     v                   
  )

  (x  ::= variable-not-otherwise-mentioned)

  (aop ::= + - / *)       ;;  arithmetic operators
  (lop ::= and or not)    ;;  logic operators
  (cop ::= == >= <= > <)  ;;  comparison operators

  ;; primitive values
  (v ::= nil number string boolean atom m rf u) 
  (atom ::= (variable-prefix :))
  
)



;;(render-language NEST "syntax.ps")