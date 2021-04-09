#lang racket

(require redex "nest-syntax.rkt")

(define logging #true)

(define (log v #:level [level "TRACE"])
  (if (equal? logging #true) (displayln (string-append level ": " (any->string v))) (display ""))  
  )

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

(define pattern-test-counter 0)
(define failure-test-counter 0)

(define (inc-pattern-test-counter)
  (set! pattern-test-counter (+ pattern-test-counter 1)))

(define (reset-test-counters)
  (set! failure-test-counter 0)
  (set! pattern-test-counter 0)
  )

(define (pattern-test-results)
  (if (equal? failure-test-counter 0 )
      (displayln (string-append "All " (any->string pattern-test-counter) " pattern tests passed."))
      (displayln (string-append (any->string failure-test-counter) " of the " (any->string pattern-test-counter) " pattern tests failed."))
      )
  (reset-test-counters)
  )

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
     (append l (list (cons k v)))])
  )   

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
  (log "add-reaction")
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
  (log "remove-reaction")
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
  (log "remove-reactions")
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
  (log "add-message")
  (append l m))


(define-metafunction NEST-R 
  PROCESS-MESSAGE : any any any any any any -> any
  [(PROCESS-MESSAGE any_id any_q any_p any_r any_pr any_e)
   ,(process-message (term any_id) (term any_q) (term any_p) (term any_r) (term any_pr) (term any_e))])

(define (process-message id q pl rl pr b )
  (log "process-message")
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
  (log "process-random-message")
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
  (log "eval-reactions")
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

(define (pattern-test r #:pattern p #:max-msgs [mm 100] #:log-output [log-level 'none] #:trace [trace #false])
  (reset-statistics)
  (inc-pattern-test-counter)
  (let* ([terms (apply-reduction-relation* r (term  ((() () (actor root_master () () (new-actor (~ ,p (reaction r1 log-reaction) (react-to ,(second p) r1))))))))]
         [env (second (first terms))]
         [selector (get-selectors (third p))]
;         [guard (get-guard-exp p)]
;         [operator (get-operator-exp p)]
;         [msgs (generate-random-messages selector guard operator mm )]
         ;         [actor (list-set (third env) 2 msgs)]
         ;         [exp (list-set env 2 actor)]
         ;         [new_m_actor (first (first (apply-reduction-relation* r (term  (,exp)))))]
         ;         [mailbox (third (third new_m_actor))]
         ;         [pbuffer (last (first (first new_m_actor)))]
         ) 
    (log "HI")
    (log selector)
    ;    (show-output p (length msgs) msgs mailbox pbuffer log-level)
    ;    (if (equal? trace #true) (traces r (term  (,exp)))  (display ""))
    )  
  )

(define (generate-random-messages s g o mm #:pollute [pollute #false])
  (let ([type (first s)]
        [attrs (list-tail s 1)]
        [n (get-total-msgs o mm)])
    (valid-random-msgs type attrs g n)
    ))

(define (get-total-msgs op maxm)
  (cond
    [(equal? op 'nil) maxm]
    [(< maxm (second op)) (error (string-append "The pattern requires at least " (any->string (second op)) " messages to have a match. Increase the value of the #:max-mgs argument."))]
    [else (let* ([type (first op)]
                 [val (second op)]
                 [base (exact-floor (/ maxm val))])
            (if (or (equal? type 'count) (equal? type 'every)) 
                (* base val)
                maxm)
            )]
    )
  )

(define (valid-random-msgs type attrs guard n [acc empty])
  (if (= n 1)
      (append acc (list (term (,(current-milliseconds) ,(valid-random-msg type attrs guard)))))
      (valid-random-msgs type attrs guard  (- n 1)  (append acc (list (term (,(current-milliseconds) ,(valid-random-msg type attrs guard))))))))

(define (valid-random-msg type attrs guard)
  (let* ([attr_vals (map (lambda (attr)
                           (valid-value attr guard))
                         attrs)]
         [final_attrs (fix-val-duplicated-attrs attr_vals attrs guard)])
    (flatten (append (list type) final_attrs))
    )
  
  )

(define (fix-val-duplicated-attrs attr_vals attrs guard)
  (let ([log_vars (filter symbol? (set->list (list->set attrs)))])
    (foldl (lambda (lv result)
             (let ([pos (indexes-of attrs lv)])
               (if (empty? pos)
                   result
                   (let ([new_val (valid-value lv guard)])
                     (foldl (lambda (i acc) (list-set acc i new_val)) result pos)
                     ))
               ))
           attr_vals
           log_vars
           )
    ))

(define (valid-value attr guard)
  (if (symbol? attr)
      (if (equal? (member attr (flatten guard)) #f) 
          (random-value)
          (let* ([pred (last guard)]
                 [op (first pred)]
                 [att_pos (index-of pred attr)])
            (cond
              [(and (equal? op '>) (equal? att_pos 1)) (random 100 200)]
              [(and (equal? op '>) (equal? att_pos 2)) (random 0 99)]
              [(and (equal? op '<) (equal? att_pos 1)) (random 0 99)]
              [(and (equal? op '<) (equal? att_pos 2)) (random 100 200)]
              [else (random 100)]
              )
            )
          )
      attr)
  )

(define (random-value)
  (let ([sl (list "open" "closed" "on" "off" "livingroom" "bedroom" "bathroom")]
        [bl (list #true #false )]
        [n (random 3)])
    (cond
      [(equal? n 0) (list-ref sl (random 7))]
      [(equal? n 1) (list-ref bl (random 2))]
      [else (random 100)]
      )))


;(define (generate-messages type attrs n attr-type [acc empty])
;  (if (= n 1)
;      (append acc (list (term (,(current-milliseconds) ,(random-msg type attrs attr-type)))))
;      (generate-messages type attrs (- n 1) attr-type (append acc (list (term (,(current-milliseconds) ,(random-msg type attrs attr-type))))))))

;; #:log-output -> 'none 'basic 'advanced
(define (pattern-check  r #:pattern p #:n-messages n #:attr-type [type (list 'all 'random)] #:log-output [log-level 'none] #:trace [trace #false])
  (reset-statistics)
  (let* ([terms (apply-reduction-relation* r (term  ((() () (actor root_master () () (new-actor (~ ,p (reaction r1 log-reaction) (react-to ,(second p) r1))))))))]
         [env (second (first terms))]
         [selector (get-selectors (third p))]
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


;TRACE: : ((:msg1 a b) (or ((:msg3 b c) (count 2)) ((:msg2 c d) (every 6))))
;TRACE: (:msg1 a b)

(define (get-selectors expr [selectors empty] [guard 'nil] [operators (make-immutable-hash)])
  (log "get-selectors")
  (log  expr)
  (if (empty? expr)
      (make-hash (list (list 'selectors selectors) (list 'guard guard) (list 'operators operators)))

      (let* ([f (first expr)])
        (log  f)
        (cond
          ;; expr = (and (:msg1 a b) (:msg1 c d))
          [(join? f) (log 1) (get-selectors (rest expr) selectors guard operators)]
          [else 
           (cond
             [(and (join? (get-first f)) (guard? expr)) (log 1.5) (get-selectors (rest f) selectors (second expr) operators)]
             ;;(or ((:msg3 b c) (count 2)) ((:msg2 c d) (every 6)))
             [(join? (get-first f)) (log 1.7) (get-selectors (rest f) selectors guard operators)]
             ;; expr = ((:msgq x y) (when (< x y)))
             [(and (basic-selector? f) (guard? expr)) (log 2) (get-selectors empty (add-selector f selectors) (second expr) operators) ]
             ;;  expr = ((:msgq x y) (count 7)) ;; or any other operator
             [(and (list? f) (operator? expr)) (log 3)  (get-selectors empty (add-selector f selectors) guard (add-operator (first f) (second expr) operators) ) ]
             ;; expr = (((:msgq x y) (count 2)) (when (> x y)))
             [(and (and (first-list? f) (operator? f)) (guard? expr)) (log 4)  (get-selectors empty (add-selector (first f) selectors) (second expr) (add-operator (first (first f)) (second f) operators) ) ]
             ;; expr = (((:msg1 a b) (count 2)) (:msg2 c d))
             [(and (first-list? f) (operator? f))  (log 4.5)  (get-selectors (rest expr) (add-selector (first f) selectors) guard (add-operator (first (first f)) (second f) operators) ) ]
             ;; expr = ((:msg1 a b) (:msg1 c d)) 
             [(first-list? f) (log 5)  (get-selectors (rest expr) (add-selector f selectors) guard operators) ]
             ;; expr = ((and (:msg1 a b) (:msg1 c d)) (when (> a c)))
             [(and (and (list? f) (join? (first f))) (guard? expr)) (log 6)  (get-selectors (rest expr) selectors (second expr) operators) ]
             ;;expr = ((:msg2 c d))
             [(list? f) (log 6.5)  (get-selectors (rest expr) (add-selector f selectors) guard operators) ]
             ;; expr = (:msgq x x)
             [else (log 7)  (get-selectors empty (add-selector expr selectors) guard operators)]
             )
           ]
          )
        ))
  )

;; s - selector
;; l - selectors' list
(define (add-selector s l)
  (append l (list s))
  )

;; n - pattern name
;; o - operator
;; h - operators' hash
(define (add-operator n o h)
  (hash-set h n o)
  )

;; TODO rename to selector?
(define (basic-selector? expr)
  (log "basic-selector?")
  (if (list? expr)
      (if (list? (first expr))
          #false
          #true)
      #false))

;; n - operator's name
(define (operator? expr)
  (log "operator?")
  (if (> (length expr) 1) (member-of? (first (second expr)) (list 'count 'every 'window)) #false))

(define (get-first expr)
  (log "get-first")
  (if (list? expr) (first expr) expr))

;; j - boolean join operator
(define (join? j)
  (log "join?")
  (member-of? j (list 'and 'andThen 'or)))

(define (member-of? i l)
  (log "member-of?")
  (log i)
  (log l)
  (if (list? (member i l)) #true #false)
  )

(define (first-list? expr)
  (log "first-list?")
  (log expr)
  (if (list? expr) (list? (first expr)) #false))

(define (guard? expr)
  (log "guard?")
  (log expr)
  (if (> (length expr) 1) (equal? (first (second expr)) 'when) #false))


(define (get-selector-exp p) 
  (log "get-selector-exp")
  (log  p)
  (let* ([s (third p)]
         [h (first s)])
    (if (list? h)
        (if (list? (first h))  (first h) (first s))       
        s
        )
    ))

(define (get-operator-exp p) 
  (log "get-operator-exp")
  (log  p)
  (let* ([e (third p)]
         [h (first e)])
    (if (list? h)
        (if (list? (first h))  
            (last h) 
            (if (equal? (first (last e)) 'when) 'nil (last e)))       
        'nil
        )
    ))

(define (get-guard-exp p) 
  (log "get-operator-exp")
  (log  p)
  (let* ([e (third p)]
         [t (last e)])
    (if (list? t)
        (if (equal? (first t) 'when)  t 'nil)       
        'nil
        )
    ))

(define (get-guard pstruct)
  (log "get-guard")
  (log  pstruct)
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
  (log "get-selector")
  (log  pstruct)
  (let ([so (car pstruct)])
    (if (list? (car so))
        (car so)
        so
        )))

;; only patterns with the count operator are handled
(define (get-operator pstruct)
  (log "get-operator")
  (log  pstruct)
  (let ([so (car pstruct)])
    (if (list? (car so))
        (extract-operator so)
        'nil)
    ))

(define (extract-operator l)
  (findf   (lambda (x) 
             (or (equal? (car x) 'every) (equal? (car x) 'count))) l))


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