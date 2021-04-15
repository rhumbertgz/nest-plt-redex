#lang racket

(require redex  "nest-reductions.rkt" "pattern-tests-suite.rkt")

(pattern-test  NEST-T-Reductions            
               ;#:pattern (term (pattern p5 ((and (:msg1 a b) (or ((:msg2 b c) (count 2)) ((:msg3 c d) (every 6)))) (when (> a c)))))
               ;#:pattern (term (pattern p5 ((and ((:msg1 a b) (count 2)) ((:msg2 c d) (every 6))) (when (> a c)))))
               ;#:pattern (term (pattern p5 (and ((:msg1 a b) (count 2)) ((:msg2 c d) (every 6)))))
               ;#:pattern (term (pattern p5 (and ((:msg1 a b) (count 2)) (:msg2 c d))))
               ;#:pattern (term (pattern p5 ((and (:msg1 a b) (:msg2 c b)) (when (> a c)))))
               ;#:pattern (term (pattern p5 (andThen (:msg1 a b) (:msg2 a d))))
               #:pattern (term (pattern p5 (and (:msg1 a b) (:msg2 a d))))
               ;#:pattern (term (pattern p1 (((:msgq x y) (count 2)) (when (> x y)) ))) 
              ; #:pattern (term (pattern p1 ((:msgq x y) (every 7)))) 
               ;#:pattern (term (pattern p1 ((:msgq x y) (window 10 mins)))) 
               ;#:pattern (term (pattern p1 ((:msgq x y) (count 7)))) 
               ;#:pattern (term (pattern p1 ((:msgq x y) (when (< x y)))))    
               ;#:pattern (term (pattern p1 ((:msgq x y) (when (> x y))))) 
               ;#:pattern (term (pattern p1 (:msgq x x))) 
               ;#:pattern (term (pattern p1 (:msgq x y)))
               #:log-output 'advanced 
               #:iterations 1)


(pattern-test-results)