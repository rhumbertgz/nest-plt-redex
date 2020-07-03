#lang racket

(require redex "nest-syntax.rkt")
(provide (all-defined-out))

;Joins two lists
(define-metafunction NEST 
  [(U (any_1 ...) (any_2 ...))
   (any_1 ... any_2 ...)])

;; Add pattern
(define-metafunction NEST 
  [(AP (any_1 ...) any_2)
   (any_1 ... ((node any_2 () () -1)) )])

;(define-metafunction Sparrow 
;  [(PM any_pl any_rl any_m)
;   ,(add-reaction (term any_1) (term any_2) (term any_3))])
;
;(define (process-message pl rl m)
;  (let* ([ph (make-hash pl)]
;         [rh (make-hash rl)]
;         [v (hash-ref ph p (lambda () '()))]
;         [rs (append v (make-list 1 m))])
;   (hash-set! h p rs)
;   (hash->list h))
;  )

;; Add reaction
(define-metafunction NEST 
  [(AR any_1 any_2 any_3)
   ,(add-reaction (term any_1) (term any_2) (term any_3))])

(define (add-reaction l p r)
  (let* ([h (make-hash l)]
         [v (hash-ref h p (lambda () '()))]
         [rs (append v (make-list 1 r))])
   (hash-set! h p rs)
   (hash->list h))
  )

;; Remove reaction
(define-metafunction NEST 
  [(RR any_1 any_2 any_3)
   ,(remove-reaction (term any_1) (term any_2) (term any_3))])

(define (remove-reaction l p r)
  (let* ([h (make-hash l)]
         [v (hash-ref h p (lambda () '()))]
         [rs (remv r v)])
   (hash-set! h p rs)
   (hash->list h)
  ))

;; Add reaction
(define-metafunction NEST 
  [(CR any_1 any_2 any_3)
   ,(contains-reaction (term any_1) (term any_2) (term any_3))])

(define (contains-reaction l p r)
  (let* ([h (make-hash l)]
         [v (hash-ref h p (lambda () '()))])
   (cond
   [(member r v)  #true]
   [else #false])
  ))

;; Remove all reaction
(define-metafunction NEST 
  [(RAR any_1 any_2)
   ,(remove-all-reactions (term any_1) (term any_2))])

(define (remove-all-reactions l p)
  (let* ([h (make-hash l)])
   (hash-remove! h p)
   (hash->list h)
  ))

;; Check for a pattern on the list of patterns
(define-metafunction NEST 
  [(CP any_1 any_2)
   ,(contains-pattern (term any_1) (term any_2))])

(define (contains-pattern l p)
  (let* ([h (make-hash l)])
   (cond
   [(hash-has-key? h p)  ':ok]
   [else ':no-reaction-to-remove])
  ))

;; Add message to the actor queue and check for possible matches
;(define-metafunction Sparrow 
;  [(AM any_m any_q any_p any_r any_pr)
;   ,(process-message (term any_m) (term any_q) (term any_p)  (term any_r)  (term any_pr))])
;
;(define (process-message m q p r pr)
;  (let* ([q_new (append q (make-list 1 m))])
;   q_new
;  ))


;; Match message against message-pattern
(define (message-match? msg mp)
    (let ([ns (make-base-namespace)])
      (parameterize ([current-namespace ns])
        (namespace-require 'racket/match)
        (namespace-set-variable-value! 'm msg ns))
      (eval (read (open-input-string (build-match-clause mp))) ns)))

(define (build-match-clause mp)
    (let* ([b (string-join (map symbol->string mp) " ")]
           [c (string-append "[(list " b ") #true]")]
           [m (string-append "(match m " c "[_ #false])")])
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

  ;;[v/x]actor{e} = actor{e}
  [(subst (x any (actor e ))) (actor e)]

  ;[v/x]pattern{ mp , ps , pst} = pattern{ mp , ps , pst}
  [(subst (x any (pattern mp po ... pt ...)))  (pattern mp po ... pt ...)]


   ;;[v/x]reaction{x, e} = reaction{x, e}
  [(subst (x any (reaction x_1 e))) (reaction x_1 e)]

  ;;[v/x]react {x, e} = react{x, e}
  [(subst (x any (react e_ref_1 e_ref_2)))
   (react (subst (x any e_ref_1)) (subst (x any e_ref_2)))]

  ;;[v/x]remove {x, e} = react{x, e}
  [(subst (x any (remove e_ref_1 e_ref_2)))
   (remove (subst (x any e_ref_1)) (subst (x any e_ref_2)))]

 ;;[v/x]removeAll {x, e} = react{x, e}
  [(subst (x any (removeAll e_ref_1))) 
   (removeAll (subst (x any e_ref_1)))]

  ;;[v/x]seq {x, e} = react{x, e}
  [(subst (x any (seq e_1 e_2)))
   (seq (subst (x any e_1)) (subst (x any e_2)))]

  ;;[v/x]send {x, e} = send{x, e}
  [(subst (x any (send e_ref e_msg))) 
   (send (subst (x any e_ref)) e_msg )]
 
  [(subst (x any (aop e_1 e_2)))
   (aop (subst (x any e_1)) (subst (x any e_2)))]
  [(subst (x any v_2)) v_2]

  )