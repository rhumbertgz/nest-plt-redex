#lang racket

(require redex "nest-syntax.rkt")
(provide (all-defined-out))


;; Add pattern
(define-metafunction NEST 
  NEW-PATTERN : (any ...) any any -> (any ...)
  [(NEW-PATTERN (any_1 ...) any_2  any_3)
   ,(new-pattern (term (any_1 ...)) (term any_2) (term any_3))])

(define (new-pattern l k v)
  (append l (list (cons k v)))
  ;  (cond
  ;   ;[(contains? l k) (error (string-append "Duplicated pattern identifier: " (symbol->string k)))]
  ;   [(contains? l k) l]
  ;   [else 
  ;    (append l (list (cons k v)))])
  )   

;; Add reaction
(define-metafunction NEST 
  NEW-REACTION : (any ...) any any -> (any ...)
  [(NEW-REACTION (any_1 ...) any_2  any_3)
   ,(new-reaction (term (any_1 ...)) (term any_2) (term any_3))])

(define (new-reaction l k v)
  (append l (list (cons k v)))
  ;  (cond
  ;   [(contains? l k) (error (string-append "Duplicated reaction identifier: " (symbol->string k)))]
  ;   ;[(contains? l k) l]
  ;   [else 
  ;    (append l (list (cons k v)))])
  )

;(define (contains? l k)
;  (let* ([h (make-hash l)])
;   (cond
;   [(hash-has-key? h k)  #true]
;   [else #false])
;  ))

;; Add reaction to a pattern
(define-metafunction NEST 
  ADD-REACTION : (any ...) any any -> (any ...)
  [(ADD-REACTION (any_1 ...) any_2  any_3)
   ,(add-reaction (term (any_1 ...)) (term any_2) (term any_3))])

(define (add-reaction l p r )
  (let* ([h (make-hash l)]
         [v (hash-ref h p (lambda () '()))]
         [rs (append v (list r))])
    (hash-set! h p rs)
    (hash->list h))
  )

;; Remove reaction from a pattern
(define-metafunction NEST
  REMOVE-REACTION : (any ...) any any -> (any ...)
  [(REMOVE-REACTION (any_1 ...) any_2 any_3)
   ,(remove-reaction (term (any_1 ...)) (term any_2) (term any_3))])

(define (remove-reaction l p r)
  (let* ([h (make-hash l)]
         [v (hash-ref h p (lambda () '()))]
         [rs (remove r v)])
    (hash-set! h p rs)
    (hash->list h)
    ))


;; Add message
(define-metafunction NEST 
  ADD-MESSAGE : (any ...) any -> (any ...)
  [(ADD-MESSAGE (any_1 ...) any_2 )
   ,(add-message (term (any_1 ...)) (term any_2))])

(define (add-message l m)
  (append l m)
  )


(define-metafunction NEST 
  PROCESS-MESSAGE : any any any any any any -> any
  [(PROCESS-MESSAGE any_id any_q any_p any_r any_pr any_e)
   ,(process-message (term any_id) (term any_q) (term any_p) (term any_r) (term any_pr) (term any_e))])

(define (process-message id q pl rl pr b )
  (displayln "process-message")
  (displayln id)
  (displayln q)
  (displayln pl)
  (displayln pr)
  (displayln b)
 
  )



;; Match message against pattern's selector
(define (message-match? msg ps)
  (let ([ns (make-base-namespace)])
    (parameterize ([current-namespace ns])
      (namespace-require 'racket/match)
      (namespace-set-variable-value! 'm msg ns))
    (eval (read (open-input-string (build-match-clause ps))) ns)))

(define (build-match-clause ps)
  (let* ([b (string-join (map symbol->string ps) " ")]
         [c (string-append "[(list " b ") #true]")]
         [m (string-append "(match m " c "[_ #false])")])
    (displayln ps)
    (displayln b)
    (displayln c)
    (displayln m)
    m))



;(subst (x_1 v_1 e_1)) substitutes variable x_1 for value v_1 in e1.
(define-metafunction NEST
  ;;[v/x]x = v
  [(subst (x_1 any_1 x_1)) any_1]
  ;;[v/x]x′ = x′
  [(subst (x_1 any_1 x_a)) x_a]
  ;;[v/x]nil = nil
  [(subst (x any nil)) nil]


  ;;[v/x]let x = e in e => let x = [v/x]e in [v/x]e
  [(subst (x any (let (x e_x) in e_body)))
   (let (x (subst (x any e_x))) in e_body)]

  ;;[v/x]let x′= e in e = let x′ = [v/x]e in [v/x]e
  [(subst (x any (let (x_a e_xa) in e_body)))
   (let (x_a  (subst (x any e_xa))) in (subst (x any e_body)))]

  [(subst (x any (react-to e_rf_1 e_rf_2)))
   (react-to (subst (x any e_rf_1)) (subst (x any e_rf_2)))]


  ;;[v/x]remove {x, e} = react{x, e}
  [(subst (x any (remove e_ref_1 e_ref_2)))
   (remove (subst (x any e_ref_1)) (subst (x any e_ref_2)))]

  ;;[v/x]removeAll {x, e} = react{x, e}
  [(subst (x any (remove-reactions e_ref_1))) 
   (remove-reactions (subst (x any e_ref_1)))]

  ;;[v/x]seq {x, e} = react{x, e}
  [(subst (x any (seq e_1 e_2)))
   (seq (subst (x any e_1)) (subst (x any e_2)))]

  ;;[v/x]send {x, e} = send{x, e}
  [(subst (x any (send e_ref e_msg))) 
   (send (subst (x any e_ref)) e_msg )]
 
  [(subst (x any (aop e_1 e_2)))
   (aop (subst (x any e_1)) (subst (x any e_2)))]


  [(subst (x any (lop e_1 e_2)))
   (aop (subst (x any e_1)) (subst (x any e_2)))]

  [(subst (x any (cop e_1 e_2)))
   (aop (subst (x any e_1)) (subst (x any e_2)))]

  [(subst (x any v_2)) v_2]
  [(subst (x any e_2)) e_2]

  )