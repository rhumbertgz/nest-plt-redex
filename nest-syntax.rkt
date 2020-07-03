#lang racket

;; NEST:

(provide NEST)  ;; Language 

(require redex)
(require pict)
(require racket/hash)

;; NEST's grammar 
(define-language NEST
  (e ::= x
         nil
         (seq e e)
         (λ [e ...] e)
         (let (x e) in e)
         ;; a pattern is composed by a 'message-pattern', a list of pattern-sets, and a list of pattern-set transformers
         ;; a pattern can be composed by other patterns using joing operators
         (pattern mp po ... pt ...)
         (reaction x e)
         (actor e)
         ;; <react> adds a reaction a to pattern
         (react e e)
         ;; specifies a particular order of the join-pattern matching mechanism 
         (match e_!_1 ...)
         (remove e e)
         (removeAll e)
         ;; <send> sends a message 'm' to an actor 'x' and continues 
         (send e e)
         ;; not in paper
         v
         (aop e e)
         (jop e e e ...)
         (cop e e)
         (if e e e)
         (e ...)
         )


  ;; Message header. A header is a constant that represents the type of message. 
  ;; In Sparrow every the first element of message must be an atom which will be the header of that message
  (h ::= atom)
  (atom ::= (variable-prefix :))
  
  ;; primitive values
  (v ::= nil number string boolean atom m rf)  
 
  (att ::= x v)
  
  ;; message-pattern
  (mp ::= (h att ...))
  
  ;; message
  (m ::= (h v ...))

  ;; a meta-message has a consecutive id, a timestamp, and a message object
  (mm ::= (id x (h v ...)))
   

  ;; join-pattern
;  (p ::= (pattern mp)
;         (pattern mp α)
;         (pattern mp α τ)
;         (pattern mp α pst ...)
;         (pattern mp α τ pst ...)
;         (pattern (jop p_!_1 p_!_1) ...))

  (k ::= (a ...))                       ; a configuration K consist of a set of actors
  
  ;; Evaluation context
  (K ::= (a ... A a ...))               ; evaluation context for K.
  (A ::= hole
         (actor id q jpe rl E))
  (E ::= hole
         (v ... E e ...)
         (let (x E) in e)
         (react E e)
         (react v E)
         (match E)
         (send E e)
         (send v E)
         (remove E e)
         (remove v E)
         (removeAll E)
         (aop E e) 
         (aop v E)
         (cop E e)
         (cop v E))
  
  ; an actor has an id, a message queue, a pattern list, a reaction list,  pattern-reaction hashtable,and a currently executing expression.
  (a ::= (actor id q jpe rl e))                
  ;; message queue 
  (q ::= (mm ...))
  ;; pattern list
;  (pl ::= (p ...))
  (pl ::= ((p . (mm ...)) ...))
  ;; reaction list
  (rl ::= (r ...))
  ;; pattern-reaction hashtable
  (pr ::= ((e . (e ...)) ...))
  ;; matching order list
  (mo ::= (e ...))

  (p ::= (pattern id mp ... po ... pt ...))
  (ndm ::= (mm ...))                     ; list of messages
  (ndr ::= (id_!_1 ...))                 ; list of reaction ids
  (nd ::= (node p ndm ndr x_weight))
  (jpe ::= (nd ...))
  ;; a reaction has a name, a single argument (a list) and a body
  (r ::= (reaction id x e)) 

  ;An actor reference consists of a actor id
  (rf (ref x))

  (aop ::= + - / *)
  (jop ::= and or not andThen)
  (cop ::= == >= <= > <)
 
  ;; ##### BEGIN Operators #####
  (po ::= α τ ב ∂)
  ;; allow developers to accumulate "n" messages
  (α ::= (count n))

  ;; allows developers to react every "nth" message
  (ℶ ::= (every n))

  ;; allow developers to specify a time window for a set of messages
  (τ ::= (window n u))

  ;; allow developers to specify a time window for a set of messages
  (∂ ::= (debounce n u))

  ;; n is used for integer
  (n ::= integer)

  ;; time units valid for message-windowing 
  (u ::= secs mins hours days weeks)

  ;; ##### END pattern-sets #####


  ;; ##### BEGIN transformers #####

  (pt ::= f b)

  ;; fold
  (f ::= (fold e))

 ;; bind
  (b ::= (bind x))

  ;; when @TODO
  (g ::= (when (m ...) e))
 
  ;; ##### END transformers #####

  ;; join operators (sigma)
;  (σ ::= (and p_!_1 p_!_1 ...)
;         (or p_!_1 p_!_1 ...)
;         (not p_!_1 p_!_1 ...)
;         (andThen p_!_1 p_!_1 ...))
;  
;  ;; actor ref  (delta)
;  (δ ::= variable-not-otherwise-mentioned)
;  ;; pattern ref (rho)
;  (ρ ::= variable-not-otherwise-mentioned)
  (id variable-not-otherwise-mentioned)
  (x ::= variable-not-otherwise-mentioned))

;;(render-language NEST "syntax.ps")