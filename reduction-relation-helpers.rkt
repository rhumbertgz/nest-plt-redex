#lang racket

(require redex  "nest-syntax.rkt" "nest-utils.rkt")
(provide (all-defined-out))

;; Add pattern
(define-metafunction NEST-R 
  NEW-PATTERN : (any ...) any any -> (any ...)
  [(NEW-PATTERN (any_1 ...) any_2  any_3)
   ,(new-pattern (term (any_1 ...)) (term any_2) (term any_3))])


(define (new-pattern l k v)
  (log "new-pattern")
  (cond
    [(contains? l k) (error (string-append "Duplicated pattern identifier: " (symbol->string k)))]
    [else 
     (append l (list (cons k v)))]))   

;; Add reaction
(define-metafunction NEST-R 
  NEW-REACTION : (any ...) any any -> (any ...)
  [(NEW-REACTION (any_1 ...) any_2  any_3)
   ,(new-reaction (term (any_1 ...)) (term any_2) (term any_3))])

(define (new-reaction l k v)
  (log "new-reaction")
  (cond
    [(contains? l k) (error (string-append "Duplicated reaction identifier: " (symbol->string k)))]
    [else 
     (append l (list (cons k v)))])  )

(define (contains? l k)
  (let* ([h (make-hash l)])
    (cond
      [(hash-has-key? h k)  #true]
      [else #false])))
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
    (reverse (hash->list h))))

;; Remove reaction from a pattern
(define-metafunction NEST-R
  REMOVE-REACTION : (any ...) any any -> (any ...)
  [(REMOVE-REACTION (any_1 ...) any_2 any_3)
   ,(remove-reaction (term (any_1 ...)) (term any_2) (term any_3))])

(define (remove-reaction l p r)
  (let* ([h (make-hash l)]
         [v (hash-ref h p (lambda () '()))]
         [rs (remove r v)])
    (if (empty? rs) 
        (hash-remove! h p)  
        (hash-set! h p rs))
    (hash->list h)))

;; Remove reactions from a pattern
(define-metafunction NEST-R
  REMOVE-REACTIONS : (any ...) any -> (any ...)
  [(REMOVE-REACTIONS (any_1 ...) any_2 )
   ,(remove-reactions (term (any_1 ...)) (term any_2) )])

(define (remove-reactions l p)
  (let* ([h (make-hash l)])
    (hash-remove! h p) 
    (hash->list h)))

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
                         ;;value: ((p_name select_guard buffer) #t list_msg)     
                         ;;acc: (list_1 list_2)
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
                       (foreach-pattern pl (list (first q))))]
      
       [npl (car results)]
       [rout (eval-reactions rl (cadr results))])
    (list (list npl rl (list 'actor id (rest q) pr rout)))))

(define-metafunction NEST-R 
  PROCESS-RANDOM-MESSAGE : any any any any any any -> any
  [(PROCESS-RANDOM-MESSAGE any_id any_q any_p any_r any_pr any_e)
   ,(process-random-message (term any_id) (term any_q) (term any_p) (term any_r) (term any_pr) (term any_e))])

(define (process-random-message id q pl rl pr b )
  (let* 
      ([results (foldr (lambda (value acc)
                         ;;value: ((p_name select_guard buffer) #t list_msg)     
                         ;;acc: (list_1 list_2)
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
                       (foreach-pattern pl (list (first q))))]
      
       [npl (car results)]
       [rout (eval-reactions-with-random-msgs rl (cadr results))])
    (list (list npl rl (list 'actor id (rest q) pr rout)))))

;; rl : reactions hash
;; act : list of reactions to execute
(define (eval-reactions rl act)
  (let
      ([rh (make-hash rl)])
    (foldr (lambda (a acc)
             (inc-reaction-counter)
             (foldl (lambda (k acc2)
                      (let*
                          ([v (hash-ref rh k 'nil)])                                            
                        (cond
                          [(eq? v 'nil) acc2]
                          [else 
                           (append acc2 (list (list-ref v 0)))])
                        )
                      )
                    acc 
                    (car a)))
           '() 
           act)))


(define (eval-reactions-with-random-msgs rl act)
  (let
      ([rh (make-hash rl)])
    (foldr (lambda (a acc)
             (inc-reaction-counter)
             (foldl (lambda (k acc2)
                      (let*
                          ([f (hash-ref rh k 'nil)])                                            
                        (cond
                          [(eq? f 'nil) acc2]
                          [else 
                           (append acc2 (list (apply log-reaction (list k (cadr a)) )))]) ;; apply reaction function (car f)
                        )
                      )
                    acc 
                    (car a)))
           '() 
           act)))


(define (get-reactions pref rl)
  (let* 
      ([h (make-hash rl)])
    (hash-ref h pref 'nil)  ))  

(define (foreach-pattern pl ml)
  (foldr (lambda (p acc)
           (append acc (list (check-msgs p ml))))
         '()
         pl))

;; p: pattern
;; ml: message list
(define (check-msgs p ml) 
  (foldr (lambda (msg acc)
           ;;msg: (id tstamp value)
           ;;acc: (pattern msg_buffer) #f list)
           (let*
               ([pattern (cadr (car acc))]
                [selector (get-selector pattern)]
                [guard (get-guard pattern)]
                [operator (get-operator pattern)])
             (cond
               [(message-match? (list-ref msg 1)  selector guard) 
                (let ([r  (check-pattern acc operator msg) ])
                  (if (equal? operator 'nil)
                      r
                      (if (and (equal? (cadr r) #t) (equal? (car operator) 'every)) 
                          (let ([newer-msg (last (last r))])
                            (list-set r 2 (list newer-msg)) 
                            )                      
                          r))
                  )]
               [else acc])            
             ))
         (cons p (list #f empty)) ;; accumulator
         ml))



(define (selector? s)
  (string-prefix? (symbol->string s) ":"))


(define (check-pattern ptr opr msg ) 
  (cond
    [ (eq? opr 'nil) (cons (car ptr) (list #t (list msg) ))] ;; no Group operator
    [else
     (let
         ([buffer (append (last (car ptr)) (list msg))] ;; add new message to the pattern's buffer
          [group (cadr opr)]
          )
       (cond
         [ (eq? group (length buffer)) (cons (update-pattern (car ptr) empty) (list #t buffer ))] ;; accumulation commpleted
         [else (cons (update-pattern (car ptr) buffer) (rest ptr)) ]
         )) ])  )

(define (update-pattern ptr buffer)
  (cons (car ptr) (list (cadr ptr) buffer)))

;; Match message against pattern's selector
(define (message-match? msg ps guard)
  (if (equal? (first msg) (first ps))
      (let ([ns (make-base-namespace)])
        (parameterize ([current-namespace ns])
          (namespace-require 'racket/match)
          (namespace-set-variable-value! 'm msg ns))
        (let ([result (eval (read (open-input-string (build-match-clause ps guard))) ns)])
          (if result (register-matched-msg msg) "do-nothing")
          result
          ))
      #false))

(define (build-match-clause ps guard)
  (let ([b (string-join (map any->string ps) " ")])
    (if (equal? guard 'nil)
        (let* ([c (string-append "[(list " b ") #true]")]
               [m (string-append "(match m " c "[_ #false])")])
          m)

        (let* ([g (string-join (map any->string guard) " ")]
               [c (string-append "[(list " b ") #:when (" g ") #true]")]
               [m (string-append "(match m " c "[_ #false])")])
          m))))



(define (get-selector pstruct)
  (log  pstruct)
  (let ([so (car pstruct)])
    (if (list? (car so))
        (car so)
        so)))

(define (get-guard pstruct)
  (let ([expr (cadr pstruct)])
    (if (equal? expr 'nil)
        'nil
        (let ([first (car expr)])
          (if (list? first)
              (extract-guard expr)
              (if (equal? first 'when) (cadr expr) 'nil))))))

(define (extract-guard l)
  (findf (lambda (x) 
             (equal? (car x) 'when)) l))

;; only patterns with the count operator are handled
(define (get-operator pstruct)
  (log "get-operator")
  (log  pstruct)
  (let ([so (car pstruct)])
    (if (list? (car so))
        (extract-operator so)
        'nil)))

(define (extract-operator l)
  (findf   (lambda (x) 
             (or (equal? (car x) 'every) (equal? (car x) 'count))) l))

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
  [(subst (x any e_2)) e_2])