#lang racket

(require redex  "nest-reductions.rkt" "nest-utils.rkt")

;;; #:pattern #:n-messages  #:attr-type 
;(pattern-check NEST-T-Reductions 
;                 #:pattern (term (pattern p1 (:msg x y))) 
;                 #:n-messages 3)
;
;
;;; pattern matching by message's type
;(pattern-check NEST-T-Reductions 
;                 #:pattern (term (pattern p1 (:msg x y))) 
;                 #:n-messages 10 
;                 #:log-output 'advanced)
;
;
;;; pattern matching example with logic variables
;;; #:attr-type ->optional arg to restrict the type of attributes to generate. Possible values: ('number  int-value) or ('string list)
;(pattern-check NEST-T-Reductions 
;                 #:pattern (term (pattern p1 (:msg x x))) 
;                 #:n-messages 10 
;                 #:attr-type (list 'number 3))
;
;
;;; pattern matching with inline filter
;(pattern-check NEST-T-Reductions 
;                 #:pattern (term (pattern p1 (:msg 2)))                          
;                 #:n-messages 10   
;                 #:attr-type (list 'number 3))
;
;;; pattern with guard filter 
;(pattern-check NEST-T-Reductions                                   
;                 #:pattern (term (pattern p1 ((:msgq x y) (when (> x y)))))    
;                 #:n-messages 5   
;                 #:attr-type (list 'number 100)
;                 #:log-output 'advanced
;                 #:trace #true)
               
;
;;;pattern executed every n messages
;(pattern-check NEST-T-Reductions                                   
;                 #:pattern (term (pattern p1 ((:msgq x y) (every 3))))    
;                 #:n-messages 5   
;                 #:attr-type (list 'number 100)
;                 #:log-output 'advanced)

;;;accumulation pattern example
;(pattern-check NEST-T-Reductions                                   
;                 #:pattern (term (pattern p1 ((:msgq x y) (count 2))))    
;                 #:n-messages 20   
;                 #:attr-type (list 'number 100)
;                 #:log-output 'advanced
;                 #:trace #true)



(pattern-test  NEST-T-Reductions            
               ;#:pattern (term (pattern p5 ((and (:msg1 a b) (or ((:msg3 b c) (count 2)) ((:msg2 c d) (every 6)))) (when (> a c)))))
               ;#:pattern (term (pattern p5 ((and ((:msg1 a b) (count 2)) ((:msg2 c d) (every 6))) (when (> a c)))))
               ;#:pattern (term (pattern p5 (and ((:msg1 a b) (count 2)) ((:msg2 c d) (every 6)))))
               ;#:pattern (term (pattern p5 (and ((:msg1 a b) (count 2)) (:msg2 c d))))
               ;#:pattern (term (pattern p5 ((and (:msg1 a b) (:msg1 c d)) (when (> a c)))))
               ;#:pattern (term (pattern p5 (and (:msg1 a b) (:msg2 c d))))
               ;#:pattern (term (pattern p1 (((:msgq x y) (count 2)) (when (> x y)) ))) 
               ;#:pattern (term (pattern p1 ((:msgq x y) (count 7)))) 
               ;#:pattern (term (pattern p1 ((:msgq x y) (when (< x y)))))    
               ;#:pattern (term (pattern p1 ((:msgq x y) (when (> x y))))) 
               ;#:pattern (term (pattern p1 (:msgq x x))) 
               #:pattern (term (pattern p1 (:msgq x y)))
               #:log-output 'advanced 
               #:max-msgs 1)



(pattern-test-results)