#lang racket

;; NEST:

(provide NEST NEST-R NEST-T)  ;; Language 

(require redex)

;; Abstract syntax
;; NEST's BNF grammar 
(define-language NEST
  (pe ::= (pattern pn p))
  (re ::= (reaction rn e))
  ;(ae ::= (actor an (react-to pn rn) ...))
  (e  ::= nil
          x l i
          (λ [e ...] e)                            ;; define a lambda
          (let (x e) in e)
          (spawn an)
          (send e e)
          (react-to pn rn)                         ;; why here and in ae?
          (remove rn pn)
          (remove-reactions pn)
          (e ...) 
          (~ e ...)
          (aop e e)                                ;; arithmetic operations
          (lop e e)                                ;; logic operations
          (cop e e)                                ;; comparison operations
          v)
  
  (aop ::= + - / *)                                ;;  arithmetic operators
  (lop ::= and or)                                 ;;  logic operators
  (cop ::= == >= <= > <)                           ;;  comparison operators
  (v ::= nil number integer string boolean atom)   ;; values
  (atom ::= (variable-prefix :))
  (g ::= (when e))                                 ;; guard expression

  (q ::= (m ...))          ;; actor's message queue 
  (m ::= (x (t v ...))) ;; a meta-message is composed by an timestamp (receiver arrival time), and a message object

  (p ::= pb                                        ;; pattern representation
         (pb g))

  (pb ::= ep 
          (lop pb pb))                             ;; compoite pattern
  
  (ep ::= s                                        ;; elementary pattern
          (s po pt ...)                            
          (not s (window number u)))              ;; negation pattern

  (t ::= atom)                                     ;; message type

  (s ::= (t att ...))                              ;; pattern selector. Its first element is a constant value that represents the type of message it matches.       
  (att ::= x v)                                    ;; selector's attribute. An attribute can be a primitive value or a logic variable (x).

  (po ::= (count integer)                         ;; allows developers to accumulate "n" messages
          (every integer)                         ;; allows developers to react every "nth" message
          (window number u)                       ;; allows developers to specify a time window for a set of messages
          (debounce integer u)                    ;; allows developers to specify a debounce window
  )

  (pt ::=  (fold e) (bind x)) 

  ;; time units valid for message-windowing 
  (u ::= secs mins hours days weeks)

  (pl ::= ((pn . (p  (m ...))) ...))              ;; pattern list
  (rl ::= ((rn . (e)) ...))                       ;; reaction list

  (pr ::= ((pn . (rn ...)) ...))                 ;; pattern-reaction registry
  
  (ml ::= ((x . (x)) ...))                       ;; message list

  (x  ::= variable-not-otherwise-mentioned)
  (l  ::= variable-not-otherwise-mentioned)
  (i  ::= variable-not-otherwise-mentioned)
  (pn ::= variable-not-otherwise-mentioned)
  (rn ::= variable-not-otherwise-mentioned)
  (an ::= variable-not-otherwise-mentioned)
  (id ::= variable-not-otherwise-mentioned)
)


(define-extended-language NEST-R NEST
 (e ::= .... pe re)
 (g ::= .... nil)  
 (v ::= .... rf)

 (rf ::= (ref id))                 ;; actor reference or ID
              
 (K ::= (a ... A a ...))           ;; evaluation context for K,
  (A ::= 
     hole
     (pl rl (actor id q pr E)))

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

  
  (k ::= (a ...))                               ;; a configuration K consist of a set of actors
  (a ::= (pl rl (actor id q pr e))              ;; An actor has an id, a message queue (q), a pattern-reaction registry (pr), and a currently executing expression.
         ((pl rl (actor id q pr e))))                     

 )

(define-extended-language NEST-T NEST-R
  (m ::= (v (t v ...)))
  )

