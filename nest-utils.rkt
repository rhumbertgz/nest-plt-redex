#lang racket

(require redex "nest-syntax.rkt")

(provide (all-defined-out))


;; Add pattern
(define-metafunction NEST-R 
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
(define-metafunction NEST-R 
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
(define-metafunction NEST-R 
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
(define-metafunction NEST-R
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
(define-metafunction NEST-R
  ADD-MESSAGE : (any ...) any -> (any ...)
  [(ADD-MESSAGE (any_1 ...) any_2 )
   ,(add-message (term (any_1 ...)) (term any_2))])

(define (add-message l m)
  (append l m))


(define-metafunction NEST-R 
  PROCESS-MESSAGE : any any any any any any -> any
  [(PROCESS-MESSAGE any_id any_q any_p any_r any_pr any_e)
   ,(process-message (term any_id) (term any_q) (term any_p) (term any_r) (term any_pr) (term any_e))])

(define (process-message id q pl rl pr b )
  (let* 
      ([results (foldr (lambda (value acc)
                         (let 
                             ([p (car value)]
                              [matched (list-ref value 1)]
                              [msgs (list-ref value 2)]
                              [plist (car acc)]
                              [act (cadr acc)]
                              )         
                           (set! plist (append plist (list p)))
                           (cond
                             [(eq? matched #t) (set! act (append act (list (list (get-reactions (car p) pr) msgs))))]
                             [else 'continue])
                           (list plist act)
                           ))
                       (list empty empty) 
                       (foreach-pattern pl q))]
       [npl (car results)]
       [rout (eval-reactions rl (cadr results))])
    (list (list 'actor id empty npl rl pr rout))
    ))


;((r1 lm st Reaction #1))
;(((ref r1)) ((id_msg x_tstamp (:msg 1 2))))

(define (eval-reactions rl act)
  (let
      ([rh (make-hash rl)])

    (foldr (lambda (a acc)
             (foldl (lambda (r acc2)
                      (let*
                          ([k (cadr r)]
                           [v (hash-ref rh k 'nil)])
                        (cond
                          [(eq? v 'nil) acc2]
                          [else (append acc2 (list (list-ref v 3)))])
                        )
                      )
                    acc 
                    (car a))
             )
           '() 
           act)
    ))


(define (get-reactions pref rl)
  (let* 
      ([h (make-hash rl)])
    (hash-ref h (list 'ref pref) 'nil)
    ))  

(define (foreach-pattern pl ml)
  (foldr (lambda (p acc)
           (append acc (list (check-msgs p ml))))
         '()
         pl))

(define (check-msgs p ml) 
  (foldr (lambda (msg acc)
           (let*
               ([pattern (cadr (car acc))]
                [selector (get-selector pattern)]
                [operator (get-operator pattern)])   
             (cond
               [(message-match? (list-ref msg 2)  selector) (check-pattern acc operator msg) ]
               [else acc])
             ))
         (cons p (list #f empty)) ;; accumulator
         ml)
  )

;; composed selectors (using AND, OR...) are not handled
(define (get-selector pstruct)
  (let
      ([s (car (car pstruct))])
    (cond
      [ (list? s) s ]
      [else (car pstruct)])
    )
  
  )

;; only patterns with the count operator are handled
(define (get-operator pstruct)
  (cadr pstruct))

(define (selector? s)
  (string-prefix? (symbol->string s) ":"))


(define (check-pattern ptr opr msg ) 
  (cond
    [ (eq? opr 'nil) (cons (car ptr) (list #t (list msg) ))] ;; no Group operator
    [else
     (let
         ([buffer (append (cadr (cadr (car ptr))) (list msg))] ;; add new message to the pattern's buffer
          [group (cadr opr)]
          )
       (cond
         [ (eq? group (length buffer)) (cons (update-pattern (car ptr) empty) (list #t buffer ))] ;; accumulation commpleted
         [else (cons (update-pattern (car ptr) buffer) (cadr ptr)) ]
         )
       )
     ])
  )

(define (update-pattern ptr buffer)
  (cons (car ptr) (list (car (cadr ptr)) buffer ) )
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
    m))


;(subst (x_1 v_1 e_1)) substitutes variable x_1 for value v_1 in e1.
(define-metafunction NEST-R

  [(subst (x_1 any_1 x_1)) any_1]
 
  [(subst (x_1 any_1 x_a)) x_a]

  [(subst (x any nil)) nil]

  [(subst (x any (let (x e_x) in e_body)))
   (let (x (subst (x any e_x))) in e_body)]

  [(subst (x any (let (x_a e_xa) in e_body)))
   (let (x_a  (subst (x any e_xa))) in (subst (x any e_body)))]

  [(subst (x any (react-to e_rf_1 e_rf_2)))
   (react-to (subst (x any e_rf_1)) (subst (x any e_rf_2)))]

  [(subst (x any (remove e_ref_1 e_ref_2)))
   (remove (subst (x any e_ref_1)) (subst (x any e_ref_2)))]

  [(subst (x any (remove-reactions e_ref_1))) 
   (remove-reactions (subst (x any e_ref_1)))]

  [(subst (x any (seq e_1 e_2)))
   (seq (subst (x any e_1)) (subst (x any e_2)))]

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