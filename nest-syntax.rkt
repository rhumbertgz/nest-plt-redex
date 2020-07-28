#lang racket

;; NEST:

(provide NEST)  ;; Language 

(require redex)

;; Abstract syntax
;; NEST's BNF grammar 
(define-language NEST
  (e ::= 
     (pattern x pb)       ;; define a join pattern
     (pattern x pb g)   
     (reaction x x x e)   ;; define a reaction
     (new-actor e)        ;; define a new actor
     (react-to e e)       ;; bind a particular reaction a to pattern
     (remove e e)         ;; remove a particular reaction from a pattern
     (remove-reactions e) ;; remove all reactions associated to a pattern
     (send e e)           ;; send a message to an actor 
     (when e e)           ;; set a guard to a pattern expression
     (aop e e)            ;; arithmetic operations
     (lop e e)            ;; logic operations
     (cop e e)            ;; comparison operations
     (if e e e)
     (seq e e)
     (λ [e ...] e)        ;; define a lambda)
     (let (x e) in e)
     x
     v
     pb
     (e ...)          
  )  

  (x  ::= variable-not-otherwise-mentioned)

  (aop ::= + - / *)       ;;  arithmetic operators
  (lop ::= and or not)    ;;  logic operators
  (cop ::= == >= <= > <)  ;;  comparison operators

  (v ::= nil number integer string boolean atom rf)   ;; values
  (atom ::= (variable-prefix :))
 
  ;; Evaluation context
  (rf ::= (ref id))                 ;; An actor reference or ID
    
  (K ::= (a ... A a ...))           ;; evaluation context for K

  (A ::= 
     hole
     (actor id q pl rl pr E))

  (E ::= 
     hole
     (v ... E e ...)
     (let (x E) in e)
     (react-to E e)
     (react-to v E)
     (send E e)
     (send v E)
     (remove E e)
     (remove v E)
     (remove-reactions E)
     (aop E e) 
     (aop v E)
     (cop E e)
     (cop v E))

  
  (k ::= (a ...))                   ;; a configuration K consist of a set of actors
  (a ::= (actor id q pl rl pr e))   ;; An actor has an id, a message queue (q), a pattern list (pl), a reaction list (rl),
                                    ;; pattern-reaction registry (pr), and a currently executing expression.

  
  (q ::= (m ...))          ;; actor's message queue 
  (m ::= (id x (t v ...))) ;; a meta-message is composed by an id, timestamp (receiver arrival time), and a message object
  (t ::= atom)              ;; message type

  (s ::= (t att ...))       ;; pattern selector. Its first element is a constant value that represents the type of message it matches.       
  (att ::= x v)             ;; selector's attribute. An attribute can be a primitive value or a logic variable (x).
 
  (pl ::= ((x . (p  (m ...))) ...))    ;; pattern list
  (p ::= (pb g))                       ;; pattern representation
  (pb ::= ep (lop pb pb))              ;; pattern
  (ep ::= s (s po pt ...))             ;; elementary pattern
  
  (rl ::= ((x . (x x e)) ...))             ;; reaction list

  (pr ::= ((rf . (rf ...)) ...))         ;; pattern-reaction registry

  (g ::= nil (when e))                 ;; guard expression
 
  


 
  ;; ##### BEGIN Operators #####
  (po ::= (count integer)      ;; allow developers to accumulate "n" messages
          (every integer)      ;; allows developers to react every "nth" message
          ;(window: number u)   ;; allow developers to specify a time window for a set of messages
          ;(debounce: integer u) ;; allow developers to specify a debounce window
  )

  ;; time units valid for message-windowing 
  ;(u ::= secs mins hours days weeks)

  ;; ##### END pattern-sets #####


  ;; ##### BEGIN transformers #####

  (pt ::=  (fold e) (bind x)) 
 
  ;; ##### END transformers #####
  
 (id ::= variable-not-otherwise-mentioned)

  )


;;(render-language NEST "syntax.ps")