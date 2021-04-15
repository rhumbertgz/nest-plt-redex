#lang racket


(require redex  "nest-utils.rkt")

(provide (all-defined-out))

(define (pattern-test-results)
  (if (equal? failure-test-counter 0 )
      (displayln (string-append "All " (any->string pattern-test-counter) " pattern tests passed."))
      (displayln (string-append (any->string failure-test-counter) " of the " (any->string pattern-test-counter) " pattern tests failed."))
      )
  (reset-test-counters))

(define (pattern-test r #:pattern p #:iterations [iterations 100] #:polluted-msgs [pollute #false] #:log-output [log-level 'none])
  (log "pattern-test"  #:label "call")
  (reset-statistics)
  (inc-pattern-test-counter)
  (let* ([terms (apply-reduction-relation* r (term  ((() () (actor root_master () () (new-actor (~ ,p (reaction r1 log-reaction) (react-to ,(second p) r1))))))))]
         [env (second (first terms))]
         [pattern (decompose-pattern (third p))]
         ;         [guard (get-guard-exp p)]
         ;         [operator (get-operator-exp p)]
         [msgs (generate-message-sequences pattern iterations pollute)]
         ;         [actor (list-set (third env) 2 msgs)]
         ;         [exp (list-set env 2 actor)]
         ;         [new_m_actor (first (first (apply-reduction-relation* r (term  (,exp)))))]
         ;         [mailbox (third (third new_m_actor))]
         ;         [pbuffer (last (first (first new_m_actor)))]
         ) 
    (log "done!")
    (log pattern)
    (log "Messages")
    (for-each (lambda (l) (displayln l)) msgs)

    ;    (show-output p (length msgs) msgs mailbox pbuffer log-level)
    ;    (if (equal? trace #true) (traces r (term  (,exp)))  (display ""))
    ))

(define (decompose-pattern expr [selectors empty] [guard 'nil] [operators (make-immutable-hash)] [members empty])
  (if (empty? expr)
      (let ([result (make-hash)])
        (hash-set! result 'selectors selectors)
        (hash-set! result 'guard guard)
        (hash-set! result 'operators operators)
        (hash-set! result 'members (set-member-constraints members))
        result
        )
     

      (let* ([f (first expr)])
        (log  f)
        (cond
          ;; expr = (and (:msg1 a b) (:msg1 c d))
          [(join? f) (log 1) (decompose-pattern (rest expr) selectors guard operators (add-member f members))]
          [else 
           (cond
             [(and (join? (get-first f)) (guard? expr)) (log 1.5) (decompose-pattern (rest f) selectors (second expr) operators (add-member (get-first f) members))]
             ;;(or ((:msg3 b c) (count 2)) ((:msg2 c d) (every 6)))
             [(join? (get-first f)) (log 1.7) (decompose-pattern (rest f) selectors guard operators (add-member (get-first f) members))]
             ;; expr = ((:msgq x y) (when (< x y)))
             [(and (basic-selector? f) (guard? expr)) (log 2) (decompose-pattern empty (add-selector f selectors) (second expr) operators (add-member (get-first f) members)) ]
             ;;  expr = ((:msgq x y) (count 7)) ;; or any other operator
             [(and (list? f) (operator? expr)) (log 3)  (decompose-pattern empty (add-selector f selectors) guard (add-operator (first f) (second expr) operators) (add-member (get-first f) members) ) ]
             ;; expr = (((:msgq x y) (count 2)) (when (> x y)))
             [(and (and (first-list? f) (operator? f)) (guard? expr)) (log 4)  (decompose-pattern empty (add-selector (first f) selectors) (second expr) (add-operator (first (first f)) (second f) operators)  (add-member (first (get-first f)) members)) ]
             ;; expr = (((:msg1 a b) (count 2)) (:msg2 c d))
             [(and (first-list? f) (operator? f))  (log 4.5)  (decompose-pattern (rest expr) (add-selector (first f) selectors) guard (add-operator (first (first f)) (second f) operators) (add-member (first (get-first f)) members) ) ]
             ;; expr = ((:msg1 a b) (:msg1 c d)) 
             [(first-list? f) (log 5)  (decompose-pattern (rest expr) (add-selector f selectors) guard operators (add-member (get-first f) members)) ]
             ;; expr = ((and (:msg1 a b) (:msg1 c d)) (when (> a c)))
             [(and (and (list? f) (join? (first f))) (guard? expr)) (log 6)  (decompose-pattern (rest expr) selectors (second expr) operators (add-member (get-first f) members)) ]
             ;;expr = ((:msg2 c d))
             [(list? f) (log 6.5)  (decompose-pattern (rest expr) (add-selector f selectors) guard operators (add-member (get-first f) members)) ]
             ;; expr = (:msgq x x)
             [else (log 7)  (decompose-pattern empty (add-selector expr selectors) guard operators (add-member (get-first f) members))]
             )
           ]
          )
        ))
  )

;; p - pattern  #hash((guard . nil) (members . #hash((:msgq . #hash((index . 0) (parent . nil))))) (operators . #hash()) (selectors . ((:msgq x y))))
;; n - number of targeted activations
(define (generate-message-sequences p n pollute?)
  (log "generate-message-sequences"  #:label "call")

  (let* ([selectors (hash-ref p 'selectors)]
         [context (hash-ref p 'members)]
         [operators (hash-ref p 'operators)]
         [guard (if (equal? (hash-ref p 'guard) 'nil) empty (second (hash-ref p 'guard)))]
         [logic_vars (set->list (get-logic-vars selectors))]
         )
    (cond
      [pollute? (generate-invalid-sequences n logic_vars selectors guard context operators)]
      [else (generate-valid-sequences n logic_vars selectors guard context operators)])
    ))

(define (generate-valid-sequences n logic_vars selectors guard context operators)
  (log "generate-valid-sequences"  #:label "call")
  (log logic_vars #:label "logic vars")
  (log selectors #:label "selectors")
  (log context #:label "context")
  (log operators #:label "operators")

  (cond
    [(equal? (length selectors) 1)
     (for/fold ([acc empty])
               ([x (in-range n)])      
       (append acc (list (new-selector-messages (first selectors) logic_vars guard operators))))       
     ]
    [(sequencing-pattern? (hash-values context)) (log "sequencing") empty]
    [else 
     (for/fold ([acc empty])
               ([x (in-range (/ n 2))])  ;; the other half will randomly generated by shuffling the sequences generated 
       (append acc (list (new-selector-messages (first selectors) logic_vars guard operators)))) ]))

;; v - context values
(define (sequencing-pattern? v)
  (cond
    [(empty? v) #false] 
    [else
     (let ([x (first v)])
       (cond
         [(and (hash-has-key? x 'type) (equal? (hash-ref x 'type) 'andThen)) #true ]
         [else  (sequencing-pattern? (rest v))])
       )]))

;; s - selector
;; h - logic variables values
;; 0 - operators
(define (new-selector-messages s lvars g o)
  (log "new-selector-messages"  #:label "call")
  (cond
    [(equal? (hash-has-key? o (first s)) #false) 
     (list (new-message s lvars g))]
    [else 
     (let* ([op_exp (hash-ref o (first s))]
            [op (first op_exp)])
       (cond
         [(member-of? op (list 'count 'every)) 
          (for/fold ([acc empty])
                    ([x (in-range (second op_exp))])      
            (append acc (list (new-message s lvars g)))) ]
         [(equal? op 'window) 
          (first (let* ([n 2] ; (random 1 11)
                        [t (first-tstamp-and-tick n (second op_exp) (third op_exp))]
                        [fts (first t)]
                        [tick (second t)])
                   (for/fold ([acc (list empty fts)])
                             ([x (in-range n)])
                     (let* ([lm (first acc)]
                            [ts (second acc)]
                            [m (new-message s lvars g ts)]
                            [nlm (append lm (list m))])
                       (list nlm (+ ts tick))
                       ))))]
         [else (error (string-append "Invalid pattern operator " (any->string op)))]))]))

(define (new-message s logic_vars guard [ts (current-milliseconds)])
  (log "new-message" #:label "call")
  (let ([h (random-value-generator logic_vars guard)])
    (list ts (foldl (lambda (x acc)
                      (cond
                        [(and (symbol? x) (hash-has-key? h x)) (append acc (list (hash-ref h x)))]
                        [else (append acc (list x))])
                      ) empty s))))

;; TODO PENDING
;; s - selector
;; h - logic variables values
(define (new-messagesss sl h)
  (log "new-message" #:label "call")
  (log sl)
  (log h)
  (foldl (lambda (s acc)
           (log "selector")
           (log s)
           (let ([m (foldl (lambda (x acc2)
                             (cond
                               [(and (symbol? x) (hash-has-key? h x)) (append acc2 (list (hash-ref h x)))]
                               [else (append acc2 (list x))])
                             ) empty s)])
             (append acc (list m)))
           ) empty sl))



(define (get-logic-vars selectors)
  (log "get-logic-vars"  #:label "call")
  (let ([vars (set)])
    (foldl (lambda (s acc)
             (foldl (lambda (lv acc_i)
                      (cond
                        [(symbol? lv) (set-add acc_i lv)]
                        [else acc_i]))
                    acc
                    (rest s)))
           vars
           selectors)))

(define (random-value-generator vars guard)
  (log "random-value-generator" #:label "call")
  (let* ([constrains (foldl (lambda (var acc)
                              (hash-set! acc var (get-constrains var guard) )
                              acc)
                            (make-hash)
                            vars)]
         [vars_with-sc (vars-with-static-constrains constrains)]
         [ord_constrains (map (lambda (k) (hash-set! constrains 
                                                     k 
                                                     (sort-constrains (hash-ref constrains k))) 
                                k) vars_with-sc)]
         [ord-vars (foldl (lambda (k acc)                                  
                            (if (member-of? k acc) acc (append acc (list k)))) ord_constrains (hash-keys constrains))]
         [values (make-immutable-hash )]
         )
    (foldl (lambda (lvar acc) (get-random-value lvar constrains acc)) values ord-vars)))


(define (get-random-value lvar all_constrains values)
  (log "get-random-value" #:label "call")
  (log lvar)
  (log values)
  (let ([constrains (var-constrains lvar all_constrains)])
    (log constrains)
    (log (empty? constrains))
    (cond
      [(or (empty? constrains) (empty? (first constrains)) ) 
       (cond
         [(hash-has-key? values lvar) values]
         [else (hash-set values lvar (random-value))])]
      [else 
       (foldl (lambda (c acc) (new-random-value lvar c acc)) values constrains)])))

(define (new-random-value blvar c values)
  (log "new-random-value" #:label "call")
  (let ([op (first c)]
        [n (second c)])
    (cond
      [(and (value? n) (equal? op '>)) (hash-set values blvar (random (+ n 30) (+ n 100)))]
      [(and (value? n) (equal? op '<)) (hash-set values blvar (random 1 n))]
      [(and (value? n) (equal? op '>=)) (hash-set values blvar (random n (+ n 100)))]
      [(and (value? n) (equal? op '<=)) (hash-set values blvar (random (+ n 1)))]
      [(and (value? n) (equal? op '=)) (hash-set values blvar n)]
      [(and (symbol? n) (equal? op '<)) 
       (cond
         [(hash-has-key? values n) 
          (let ([blvar_value (hash-ref values n )])
            (hash-set values blvar (- (random blvar_value) 1)))]
         [else 
          (cond
            [(hash-has-key? values blvar)
             (let ([blvar_value (hash-ref values blvar )])
               (hash-set values n (random (+ blvar_value 1) (+ blvar_value 100))))]
            [else 
             (let ([blvar_value (random 50 200)])
               (hash-set  (hash-set values blvar blvar_value) n (random (+ blvar_value 1) (+ blvar_value 100))))])]
         )]

      [(and (symbol? n) (equal? op '<=))
       (cond
         [(hash-has-key? values blvar) 
          (let ([blvar_value (hash-ref values blvar )])
            (hash-set values n (random blvar_value (+ blvar_value 100))))]
         [else 
          (let ([blvar_value (random 50 200)])
            (hash-set  (hash-set values blvar blvar_value) n (random blvar_value (+ blvar_value 100))) )]
         )])))

(define (generate-invalid-sequences n logic_vars selectors guard members operators)
  (let* ([var_hash (invalid-random-value-generator selectors guard)])
    (log logic_vars)))


(define (invalid-random-value-generator vars guard)
  (let* ([constrains (foldl (lambda (var acc)
                              (hash-set! acc var (get-constrains var guard) )
                              acc)
                            (make-hash)
                            vars)]
         [vars_with-sc (vars-with-static-constrains constrains)]
         [ord_constrains (map (lambda (k) (hash-set! constrains 
                                                     k 
                                                     (sort-constrains (hash-ref constrains k))) 
                                k) vars_with-sc)]
         [ord-vars (foldl (lambda (k acc)                                  
                            (if (member-of? k acc) acc (append acc (list k)))) ord_constrains (hash-keys constrains))]
         [values (make-immutable-hash )])
    (foldl (lambda (lvar acc) (get-invalid-random-value lvar constrains acc)) values ord-vars)))

(define (get-invalid-random-value lvar all_constrains values)
  (let ([constrains (var-constrains lvar all_constrains)])
    (log constrains)
    (log (empty? constrains))
    
    (cond
      [(or (empty? constrains) (empty? (first constrains)) )
       (cond
         [(hash-has-key? values lvar) values]
         [else (hash-set values lvar (random-value))])]
      [else 
       (foldl (lambda (c acc) (new-invalid-random-value lvar c acc)) values constrains)
       ])
    ))
(define (new-invalid-random-value blvar c values)
  (let ([op (first c)]
        [n (second c)])
    (cond
      [(and (value? n) (equal? op '>)) (hash-set values blvar (random n))]
      [(and (value? n) (equal? op '<)) (hash-set values blvar (random (+ n 1) (+ n 100)))]
      [(and (value? n) (equal? op '>=)) (hash-set values blvar (random n))]
      [(and (value? n) (equal? op '<=)) (hash-set values blvar (random (+ n 2) (+ n 100)))]
      [(and (value? n) (equal? op '=)) (hash-set values blvar (random (+ n 1) (+ n 100)))]
      [(and (symbol? n) (equal? op '<)) 
       (cond
         [(hash-has-key? values n) 
          (let ([blvar_value (hash-ref values n )])
            (hash-set values blvar (random (+ blvar_value 1) (+ blvar_value 100))))]
         [else 
          (cond
            [(hash-has-key? values blvar) 
             (let ([blvar_value (hash-ref values blvar )])
               (hash-set values n (random  blvar_value) )
               )]
            [else 
             (let ([blvar_value (random 50 200)])
               (hash-set  (hash-set values blvar blvar_value) n (random  blvar_value)) )])
          ])]
      [(and (symbol? n) (equal? op '<=)) 
       (cond
         [(hash-has-key? values blvar) 
          (let ([blvar_value (hash-ref values blvar )])
            (hash-set values n (random  blvar_value)))]
         [else 
          (let ([blvar_value (random 50 200)])
            (hash-set  (hash-set values blvar blvar_value) n (random  blvar_value)) )]
         )])))

(define (var-constrains var hash )
  (log "var-constrains" #:label "call")
  (log var)
  (let ([constrains (hash-ref hash var)])
    (cond
      [(empty? constrains) constrains]
      [(list? (first constrains)) constrains]
      [else (list constrains)])))

(define (sort-constrains l)
  (log "sort-constrains" #:label "call")
  (cond
    [(empty? l) l]
    [(equal? (length l) 1) l]
    [else 
     (sort l 
           (lambda (x y) 
             (let* ([x_b (second x)]
                    [y_b (second y)]
                    [x_i (if (symbol? x_b) -1 x_b)]
                    [y_i (if (symbol? y_b) -1 y_b)])
               (> x_i y_i))
             ))
     ])
  )

(define (vars-with-static-constrains hash)
  (log "var-constrains" #:label "call")
  (filter (lambda (k) 
            (contains-values? (hash-ref hash k))) 
          (hash-keys hash)))

(define (contains-values? v)
  (let ([results (memf (lambda (i)
                         (value? i))
                       (flatten v))])
    (cond
      [(list? results) #true]
      [else #false])))


(define (get-constrains var guard)
  (log "get-constrains" #:label "call")
  (let ([constrains empty]
        [comp_op (list '> '< '>= '<= '=)]
        [bool_op (list 'and 'or)])
    (cond
      [(member-of? (get-first guard) comp_op) 
       (if (member-of? var guard)
           (get-constraint var guard)
           empty)
       ]
      [else
       (foldl (lambda (i acc) 
                (cond
                  [(member-of? i bool_op) acc] 
                  [(and (and (list? i) (member-of? (first i) comp_op)) (member-of? var i))  (add-constrain acc (get-constraint var i)) ]
                  [(member-of? (first i) bool_op) (append acc (get-constrains var (rest i))) ]
                  [else acc ]))
              constrains
              guard)])))

(define (add-constrain l c)
  (log "add-constrain" #:label "call")
  (cond
    [(empty? c) l]
    [else (append l (list c))]))

(define (get-constraint var guard)
  (let ([op (first guard)]
        [comp_op_l (list '< '<= '=)]
        [comp_op_g (list '> '>= '=)]
        [comp_op (list '> '< '>= '<= '=)]
        [x  (second guard)]
        [y  (third guard)])
    (cond
      [(and (and (equal? x var) (symbol? y)) (member-of? op comp_op_l)) (list op y)]
      [(and (and (equal? y var) (symbol? x)) (member-of? op comp_op_g))  (list (oposite-op op) x)]   
      [(and (and (equal? x var) (value? y)) (member-of? op comp_op))   (list op y)]
      [(and (and (equal? y var) (value? x)) (member-of? op comp_op))   (list op x)]
      [else empty])))

(define (value? v)
  (if (symbol? v) #false #true))

(define (oposite-op o)
  (cond
    [(equal? o '>) '<]
    [(equal? o '<) '>]
    [(equal? o '>=) '<=]
    [(equal? o '<=) '>=]
    [else o]))

(define (random-value)
  (let ([sl (list "open" "closed" "on" "off" "livingroom" "bedroom" "bathroom")]
        [bl (list #true #false )]
        [n (random 3)])
    (cond
      [(equal? n 0) (list-ref sl (random 7))]
      [(equal? n 1) (list-ref bl (random 2))]
      [else (random 100)]
      )))


(define (set-member-constraints members  )
  (let* ([output (make-immutable-hash)]
         [init (hash-set (hash-set (make-immutable-hash) 'output output ) 'counter 0)]
         [results (foldl (lambda (m acc)
                           (cond
                             [(join? m) (register-join-operator m acc)] 
                             [else (register-selector m acc)]))
                         init
                         members
                         )])
    (hash-ref results 'output)))

(define (register-selector type hash)
  (let* ([counter (hash-ref hash 'counter)]
         [output (hash-ref hash 'output)]
         [parent (get-parent counter output)])
    (hash-set hash 'output (register-children type parent output))))

(define (register-children child parent hash)
  (if (equal? parent 'nil)
      (hash-set hash child (hash-set (hash-set (make-immutable-hash) 'parent 'nil) 'index 0) )
      (let* ([children (hash-ref (hash-ref hash parent) 'children)]
             [hash_children (hash-set hash parent (hash-set (hash-ref hash parent) 'children (append children (list child))))])
        (hash-set hash_children child (hash-set (hash-set (make-immutable-hash) 'parent parent) 'index (get-index children))))))

(define (get-index children)
  (let ([n (length children)])
    (cond
      [(equal? n 0) 0]
      [else 1])))

(define (get-parent counter hash)
  (if (equal? counter 0)
      'nil
      (let ([children (length (hash-ref (hash-ref hash counter) 'children))])
        (cond
          [(< children 2) counter]
          [else (get-parent (- counter 1) hash)]))))

(define (register-join-operator m hash)
  (let* ([counter (hash-ref hash 'counter)]
         [output (hash-ref hash 'output)])
    (cond
      [(equal? counter 0)
       (let* ([output_j (hash-set output (+ counter 1) (new-join m))]
              [hash_c (inc-join-counter output  counter)]) 
         (hash-set hash_c 'output output_j))]
      [else
       (let* ([output_j (hash-set output (+ counter 1) (new-join m counter))]
              [hash_c (inc-join-counter output  counter)]) 
         (hash-set hash_c 'output output_j))])))

(define (inc-join-counter hash counter)
  (hash-set hash  'counter (+ counter 1)))

(define (new-join operator [parent 0] [children empty])
  (hash-set (hash-set (hash-set (make-immutable-hash)  'type operator) 'parent parent) 'children children))

;; s - selector
;; l - selectors' list
(define (add-selector s l)
  (append l (list s)))

(define (add-member s l)
  (append l (list s)))

;; n - pattern name
;; o - operator
;; h - operators' hash
(define (add-operator n o h)
  (hash-set h n o))

;; TODO rename to selector?
(define (basic-selector? expr)
  (log "basic-selector?" #:label "call")
  (if (list? expr)
      (if (list? (first expr))
          #false
          #true)
      #false))

;; n - operator's name
(define (operator? expr)
  (log "operator?" #:label "call")
  (if (> (length expr) 1) (member-of? (first (second expr)) (list 'count 'every 'window)) #false))

(define (get-first expr)
  (log "operator?" #:label "call")
  (cond
    [(and (list? expr) (empty? expr)) expr]
    [(list? expr) (first expr)]
    [else expr]))

;; j - boolean join operator
(define (join? j)
  (member-of? j (list 'and 'andThen 'or)))

(define (member-of? i l)
  (if (list? (member i l)) #true #false))

(define (first-list? expr)
  (log "first-list?" #:label "call")
  (if (list? expr) (list? (first expr)) #false))

(define (guard? expr)
  (if (> (length expr) 1) (equal? (first (second expr)) 'when) #false))

;; TODO check ussage
;; composed selectors (using AND, OR...) are not handled




;; ts - timestamp in milliseconds
(define (tstamp->string ts)
  (let ([msdate (seconds->date (/ ts 1000))])
    (string-append (f (date-day msdate)) "-" (f (date-month msdate)) "-" (f (date-year msdate)) " " (f (date-hour msdate)) ":" (f (date-minute msdate)) ":" (f (date-second msdate)))
    ))

(define (f v)
  (cond
    [(< v 10) (string-append "0" (format "~v" v))]
    [else (format "~v" v)]))


(define (first-tstamp-and-tick n t u)
  (let* ([ctms (current-milliseconds)]
         [stms (cond
                 [(equal? u 'secs) (- ctms (* 1000 t))]
                 [(equal? u 'mins) (- ctms (* 60000 t))]
                 [(equal? u 'hours) (- ctms (* 3600000 t))]
                 [(equal? u 'days) (- ctms (* 86400000 t))]
                 [(equal? u 'weeks) (- ctms (* 604800000 t))])]
         [tick (/ (- ctms stms) (- n 0.5))])
    (list stms tick)))


;(define (get-selector-exp p) 
;  (let* ([s (third p)]
;         [h (first s)])
;    (if (list? h)
;        (if (list? (first h))  (first h) (first s))       
;        s)))
;
;(define (get-operator-exp p) 
;  (let* ([e (third p)]
;         [h (first e)])
;    (if (list? h)
;        (if (list? (first h))  
;            (last h) 
;            (if (equal? (first (last e)) 'when) 'nil (last e)))       
;        'nil)))
;
;(define (get-guard-exp p) 
;  (let* ([e (third p)]
;         [t (last e)])
;    (if (list? t)
;        (if (equal? (first t) 'when)  t 'nil)       
;        'nil)))
;



(define (show-output pt nm msgs mailbox pbuffer level)
  (cond
    [(equal? level 'none) (displayln "")]
    [(equal? level 'basic) (show-basic-output pt nm mailbox pbuffer)]
    [(equal? level 'advanced) (show-advanced-output pt nm msgs mailbox pbuffer)]))

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
  (displayln "=========================================================="))


;(define guard (list 'and (list '> 'x 'y) (list 'and (list '> 'z 'y) (list '> 'y 5))))
;(invalid-random-value-generator (list 'x 'y 'z) guard)

;(define guard (list 'and (list '> 'x 'y) (list 'and (list '> 'z 'y) (list '< 'y 5))))
;(random-value-generator (list 'x 'y 'z) guard)
;
;(define guard (list 'and (list '> 'x 'y) (list 'and (list '< 'z 'y) (list '< 'y 5))))
;(random-value-generator (list 'x 'y 'z) guard)


;(define guard (list 'and (list '> 'x 'y) (list '> 'z 'y)))
;(random-value-generator (list 'x 'y 'z) guard)

;(define guard (list 'and (list '> 'x 'y) (list '> 'z 20)))
;(invalid-random-value-generator (list 'x 'y 'z) guard)

;(define guard (list '< 'x 'y) )
;(invalid-random-value-generator (list 'x 'y) guard)

;(define guard (list '> 'x 'y) )
;(invalid-random-value-generator (list 'x 'y) guard)

;(log guard)


