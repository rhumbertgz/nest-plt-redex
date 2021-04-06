#lang racket

(require redex "nest-syntax.rkt")

(provide (all-defined-out))

(define matched-msgs empty)

(define (register-matched-msg m)
  (set! matched-msgs (append matched-msgs (list m))))

(define (clear-matched-msgs)
  (set! matched-msgs empty))

(define reaction-counter 0)

(define (inc-reaction-counter)
  (set! reaction-counter (+ reaction-counter 1)))

(define (reset-reaction-counter)
  (set! reaction-counter 0))

(define consumed-msgs-counter 0)

(define (inc-consumed-msgs-counter)
  (set! consumed-msgs-counter (+ consumed-msgs-counter 1)))

(define (reset-consumed-msgs-counter)
  (set! consumed-msgs-counter 0))

(define consumed-msgs empty)

(define (register-consumed-msgs l)
  (set! consumed-msgs (append consumed-msgs (list l))))

(define (clear-consumed-msgs)
  (set! consumed-msgs empty))

(define (reset-statistics)
  (clear-matched-msgs)
  (reset-reaction-counter)
  (reset-consumed-msgs-counter)
  )


;; Add pattern
(define-metafunction NEST-R 
  NEW-PATTERN : (any ...) any any -> (any ...)
  [(NEW-PATTERN (any_1 ...) any_2  any_3)
   ,(new-pattern (term (any_1 ...)) (term any_2) (term any_3))])


(define (new-pattern l k v)
  (cond
    [(contains? l k) (error (string-append "Duplicated pattern identifier: " (symbol->string k)))]
    [else 
     (append l (list (cons k v)))])
  )   

;; Add reaction
(define-metafunction NEST-R 
  NEW-REACTION : (any ...) any any -> (any ...)
  [(NEW-REACTION (any_1 ...) any_2  any_3)
   ,(new-reaction (term (any_1 ...)) (term any_2) (term any_3))])

(define (new-reaction l k v)
  (cond
    [(contains? l k) (error (string-append "Duplicated reaction identifier: " (symbol->string k)))]
    [else 
     (append l (list (cons k v)))])
  )

(define (contains? l k)
  (let* ([h (make-hash l)])
    (cond
      [(hash-has-key? h k)  #true]
      [else #false])
    ))

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
    (reverse (hash->list h)))
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
    (if (empty? rs) 
        (hash-remove! h p)  
        (hash-set! h p rs))
    (hash->list h)
    ))

;; Remove reactions from a pattern
(define-metafunction NEST-R
  REMOVE-REACTIONS : (any ...) any -> (any ...)
  [(REMOVE-REACTIONS (any_1 ...) any_2 )
   ,(remove-reactions (term (any_1 ...)) (term any_2) )])

(define (remove-reactions l p)
  (let* ([h (make-hash l)])
    (hash-remove! h p) 
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
    (list (list npl rl (list 'actor id (rest q) pr rout)))
    ))


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
    (list (list npl rl (list 'actor id (rest q) pr rout)))
    ))



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
                    (car a))
             )
           '() 
           act)
    ))


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
           act)
    ))


(define (get-reactions pref rl)
  (let* 
      ([h (make-hash rl)])
    (hash-ref h pref 'nil)    
    ))  

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


(define (get-guard pstruct)
  (let ([expr (cadr pstruct)])
    (if (equal? expr 'nil)
        'nil
        (let ([first (car expr)])
          (if (list? first)
              (extract-guard expr)
              (if (equal? first 'when) (cadr expr) 'nil)
              )
          ))))

(define (extract-guard l)
  (findf   (lambda (x) 
             (equal? (car x) 'when)) l))



;; composed selectors (using AND, OR...) are not handled
(define (get-selector pstruct)
  (let ([so (car pstruct)])
    (if (list? (car so))
        (car so)
        so
        )))

;; only patterns with the count operator are handled
(define (get-operator pstruct)
  (let ([so (car pstruct)])
    (if (list? (car so))
        (extract-operator so)
        'nil)
    ))

(define (extract-operator l)
  (findf   (lambda (x) 
             (or (equal? (car x) 'every) (equal? (car x) 'count))) l))

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
         )
       )
     ])
  )

(define (update-pattern ptr buffer)
  (cons (car ptr) (list (cadr ptr) buffer ) )
  )

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
          )
        )
      #false
      ))

(define (build-match-clause ps guard)
  (let ([b (string-join (map any->string ps) " ")])
    (if (equal? guard 'nil)
        (let* ([c (string-append "[(list " b ") #true]")]
               [m (string-append "(match m " c "[_ #false])")])
          m)

        (let* ([g (string-join (map any->string guard) " ")]
               [c (string-append "[(list " b ") #:when (" g ") #true]")]
               [m (string-append "(match m " c "[_ #false])")])
          m)
        )
    )
  )

(define (any->string x)
  (call-with-output-string
   (lambda (out)
     (display x out))))

;(define (any->string attr)
;  (cond
;    [(symbol? attr) (symbol->string attr)]
;    [(number? attr) (number->string attr)]
;    [(boolean? attr) attr]
;    [else attr]
;    )
; )

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


;; Random testing

;; #:log-output -> 'none 'basic 'advanced
(define (pattern-check  r #:pattern p #:n-messages n #:attr-type [type (list 'all 'random)] #:log-output [log-level 'none] #:trace [trace #false])
  (reset-statistics)
  (let* ([terms (apply-reduction-relation* r (term  ((() () (actor root_master () () (new-actor (~ ,p (reaction r1 log-reaction) (react-to ,(second p) r1))))))))]
         [env (second (first terms))]
         [selector (get-selector-exp p)]
         [msgs (random-messages selector n type)]
         [actor (list-set (third env) 2 msgs)]
         [exp (list-set env 2 actor)]
         [new_m_actor (first (first (apply-reduction-relation* r (term  (,exp)))))]
         [mailbox (third (third new_m_actor))]
         [pbuffer (last (first (first new_m_actor)))]) 
    
    (show-output p n msgs mailbox pbuffer log-level)
    (if (equal? trace #true) (traces r (term  (,exp)))  (displayln ""))
    ))

(define (show-output pt nm msgs mailbox pbuffer level)
  (cond
    [(equal? level 'none) (displayln "")]
    [(equal? level 'basic) (show-basic-output pt nm mailbox pbuffer)]
    [(equal? level 'advanced) (show-advanced-output pt nm msgs mailbox pbuffer)]
    )
  )

(define (show-basic-output pt nm mailbox pbuffer)
  (displayln "==========================================================")    
  (display "pattern-check: ")
  (displayln pt)
  (displayln "")
  (display "  - random messages: ")
  (displayln nm)
  (display "  - matched messages: ")
  (displayln (length matched-msgs))
  (display "  - total of fired reactions: ")
  (displayln reaction-counter)
  ;  (display "    consumed messages: ")
  ;  (displayln consumed-msgs-counter)
  (display "  - actor' mailbox: ")
  (displayln mailbox) 
 (display "  - pattern' buffer: ")
 (displayln (length pbuffer))  
  (displayln "==========================================================")
  )

(define (show-advanced-output pt nm msgs mailbox pbuffer)
  (displayln "==========================================================")    
  (display "pattern-check: ")
  (displayln pt)
  (displayln "")
  (display "  - random messages: ")
  (displayln (map (lambda (m) (any->string (last m))) msgs))
  (display "    Total:")
  (displayln nm)
  
  
  (displayln "")
  (display "  - matched messages: ")
  (displayln (map (lambda (m) (any->string m)) matched-msgs))
  (display "    Total:")
  (displayln (length matched-msgs))
  (displayln "")

  (display "  - total of fired reactions: ")
  (displayln reaction-counter)
  ;  (display "    consumed messages: ")
  ;  (displayln consumed-msgs-counter)
  (cond
    [(> consumed-msgs-counter 0) 
     (displayln "    consumed messages ")
     (for-each (lambda (l) (displayln (string-append "    " (any->string l)))) consumed-msgs)]
    [else (display "")])

  (displayln "")
  (display "  - pattern' buffer: ")
  (displayln (map (lambda (m) (any->string (last m))) pbuffer)) 
  (display "    Size: ")
  (displayln (length pbuffer)) 

  (displayln "")
  (display "  - actor' mailbox: ")
  (displayln mailbox) 
  (display "    Size: ")
  (displayln (length mailbox))     
  (displayln "==========================================================")
  )


(define (get-selector-exp p) 
  (let ([e (third p)])
    (if (list? (first e))
        (first e)
        e
        )
    ))
(define (random-messages selector n attr-type )
  (let ([type (first selector)]
        [attrs (- (length selector) 1) ])
    (generate-messages type attrs n attr-type)
    ))

(define (generate-messages type attrs n attr-type [acc empty])
  (if (= n 1)
      (append acc (list (term (,(current-milliseconds) ,(random-msg type attrs attr-type)))))
      (generate-messages type attrs (- n 1) attr-type (append acc (list (term (,(current-milliseconds) ,(random-msg type attrs attr-type))))))))
 
(define (random-msg type attrs attr-type)
  (flatten (append (list (random-type type)) (random-attrs attrs attr-type)) )
  )

(define (random-type type)
  (let ([n (random 2)]
        [tl (list ':temperature ':motion ':humidity ':light ':smoke ':carbon_dioxide)])
    (cond
      [(equal? n 0) (list-ref tl (random 6))]
      [else type]
      )
    ))

(define (random-attrs attrs type)
  (generate-attrs attrs type)
  )

(define (generate-attrs n type [acc empty])
  (if (= n 1)
      (append acc (list (random-attr type)))
      (generate-attrs (- n 1) type (append acc (list (random-attr type))))))

(define (random-attr type)
  (let ([sl (list "open" "closed" "on" "off" "livingroom" "bedroom" "bathroom")]
        [bl (list #true #false )])
    (cond
      [(equal? (car type) 'number) (random (cadr type))]
      [(equal? (car type) 'string) (list-ref (cadr type) (random (length (cadr type))))]
      [else
       (let ([n (random 3)])
         (cond
           [(equal? n 0) (list-ref sl (random 7))]
           [(equal? n 1) (list-ref bl (random 2))]
           [else (random 100)]
           )
         )
       ]
      )))


(define (log-reaction r l)
 
  (let* ([header (string-append (any->string r) " --> ")]
         [msgs (map (lambda (m) 
                      (inc-consumed-msgs-counter)
                      (any->string (last m))) l)])
    (register-consumed-msgs msgs)
    (string-append header (any->string msgs))
    ))