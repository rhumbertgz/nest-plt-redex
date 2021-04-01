#lang racket

(require redex  "nest-reductions.rkt" "nest-utils.rkt")

;; ##### BEGIN Basic Syntax Tests #####
(define (test-basic-syntax)
  (test-->> NEST-Reductions
            (term  ((() ()  (actor id () ()  (+ 2 3)))))
            (term  ((() ()  (actor id () ()  5)))))

  (test-->> NEST-Reductions
            (term  ((() () (actor id () () (- 6 3)))))
            (term  ((() () (actor id () () 3)))))

  (test-->> NEST-Reductions
            (term  ((() () (actor id () () (* 2 3)))))
            (term  ((() () (actor id () () 6)))))

  (test-->> NEST-Reductions
            (term  ((() () (actor id () () (/ 15 3)))))
            (term  ((() () (actor id () () 5)))))

  )
; ##### END Basic Syntax Tests #####


;; ##### BEGIN Substitution Tests #####
(define (test-substitution)
  (test-->> NEST-Reductions
            (term  ((() ()(actor id_01 () () (let (x (+ 2 4)) in x)))))
            (term  ((() ()(actor id_01 () () 6)))))

  (test-->> NEST-Reductions
            (term  ((() ()(actor id_02 () () (let (x (* 2 4)) in x))) ))
            (term  ((() () (actor id_02 () () 8)))))

  (test-->> NEST-Reductions
            (term  ((() () (actor id_03 () () (let (x (let (x (+ 2 4)) in x)) in  (let (y 0) in  (+ y (+ x 3) )))))))
            (term  ((() () (actor id_03 () () 9)))))

  (test-->> NEST-Reductions
            (term  ((() () (actor id_03 () () (let (x (let (x (+ 2 4)) in x)) in  (let (y (+ x 2)) in  (+ y (+ x 3) )))))))
            (term  ((() () (actor id_03 () () 17)))))

  ;  (test-->> NEST-Reductions
  ;            (term  ((() () (actor id_04 () () (seq (+ 2 4) (+ 2 3) )))))
  ;            (term  ((() () (actor id_04 () () 5)))))

  (test-->> NEST-Reductions
            (term  ((() () (actor id_05 () () (let (x (+ 2 4)) in x)))))
            (term  ((() () (actor id_05 () () 6)))))
  )
;; ##### END Substitution Tests #####


;; ##### BEGIN Pattern Tests #####
(define (test-patterns)

  ;; Elementary Pattern
  (test-->> NEST-Reductions
            (term  ((() () (actor id_10 () () (pattern p1 (:msg x y))))))
            (term  ((((p1 . (((:msg x y) nil) ()))) () (actor id_10 () () ())))))

  ;; Elementary Pattern with a guard expression
  (test-->> NEST-Reductions
            (term  ((() () (actor id_11 () () (pattern p2 ((:msg x y) (when (> x y))))))))
            (term  ((((p2 . (((:msg x y) (when (> x y))) ()))) () (actor id_11 () () ())))))

  ;; Elementary Pattern using the extensional sequencing operator
  (test-->> NEST-Reductions
            (term  ((() () (actor id_12 () () (pattern p3 ((:msg x y) (every 3)))))))
            (term  ((((p3 . ((((:msg x y) (every 3)) nil) ()))) () (actor id_12 () () ())))))

  ;; Composite Pattern
  (test-->> NEST-Reductions
            (term  ((() () (actor id_13 () () (pattern p4 (and (:msg1 a b) (:msg1 c d)))))))
            (term  ((((p4 . (((and (:msg1 a b) (:msg1 c d)) nil) ()))) () (actor id_13 () () ())))))

  ;; Composite Pattern with a guard expression
  (test-->> NEST-Reductions
            (term  ((() () (actor id_14 () () (pattern p5 ((and (:msg1 a b) (:msg1 c d)) (when (> a c))))))))
            (term  ((((p5 . (((and (:msg1 a b) (:msg1 c d)) (when (> a c))) ()))) () (actor id_14 () () ())))))

  ;; Quantified Accumulation Pattern 
  (test-->> NEST-Reductions
            (term  ((() () (actor id_15 () () (pattern p6 ((:msg x y) (count 3)))))))
            (term  ((((p6 . ((((:msg x y) (count 3)) nil) ()))) () (actor id_15 () () ())))))
           

  ;; Quantified Accumulation Pattern with a guard expression
  (test-->> NEST-Reductions
            (term  ((() () (actor id_16 () () (pattern p7 (((:msg x y) (count 3)) (when (> x y))) )))))
            (term  ((((p7 . ((((:msg x y) (count 3)) (when (> x y))) ()))) () (actor id_16 () () ())))))

  ;; Composite/Quantified Accumulation Pattern
  (test-->> NEST-Reductions
            (term  ((() () (actor id_17 () () (pattern p8 (and ((:msg1 a b) (every 3)) ((:msg1 c d) (every 3))))))))
            (term  ((((p8 . (((and ((:msg1 a b) (every 3)) ((:msg1 c d) (every 3))) nil) ()))) () (actor id_17 () () ())))))


  ;; Add multiple patterns to an actor
  (test-->> NEST-Reductions
            (term  ((((p1 . (((:msg x y) nil) ()))) () (actor id_18 () () (pattern p2 (:temp x y z))))))
            (term  ((((p1 . (((:msg x y) nil) ())) (p2 . (((:temp x y z) nil) ()))) () (actor id_18 () () ())))))


  )
;; ##### END Pattern Tests #####


;; ##### BEGIN Reactions Tests #####

(define (test-reactions)
  ;; Reaction definition
  (test-->> NEST-Reductions
            (term  ((() () (actor id_20 () () (reaction r1 "Firing reaction 1")))))
            (term  ((() ((r1 . ("Firing reaction 1"))) (actor id_20 () () ())))))

  ;; Multiple reaction definitions
  (test-->> NEST-Reductions
            (term  ((() ((r1 . ("Firing reaction 1"))) (actor id_30 () () (reaction r2 "Firing reaction 2")))))
            (term  ((() ((r1 . ("Firing reaction 1")) (r2 . ("Firing reaction 2"))) (actor id_30 () () ())))))
  ;

  ;; Add a reaction to pattern
  (test-->> NEST-Reductions
            (term  ((((p1 . (((:msg x y) nil) ()))) ((r1 . ("Firing reaction 1"))) (actor id_40 () () (react-to p1 r1)))))
            (term  ((((p1 . (((:msg x y) nil) ()))) ((r1 . ("Firing reaction 1"))) (actor id_40 () ((p1 . (r1))) ())))))


  ;; Add a multiple reactions to pattern
  (test-->> NEST-Reductions
            (term  ((((p1 . (((:msg x y) nil) ()))) ((r1 . ("Firing reaction 1")) (r2 . ("Firing reaction 2"))) (actor id_50 () ((p1 . (r1))) (react-to p1 r2)))))
            (term  ((((p1 . (((:msg x y) nil) ()))) ((r1 . ("Firing reaction 1")) (r2 . ("Firing reaction 2"))) (actor id_50 () ((p1 . (r1 r2))) ())))))


  ;; Remove a reaction from a pattern
  (test-->> NEST-Reductions
            (term  ((((p1 . (((:msg x y) nil) ()))) ((r1 . ("Firing reaction 1")) (r2 . ("Firing reaction 2"))) (actor id_60 () ((p1 . (r1 r2))) (remove r2 p1)))))
            (term  ((((p1 . (((:msg x y) nil) ()))) ((r1 . ("Firing reaction 1")) (r2 . ("Firing reaction 2"))) (actor id_60 () ((p1 . (r1))) ())))))


  ;; Remove all reaction from a pattern
  (test-->> NEST-Reductions
            (term  ((((p1 . (((:msg x y) nil) ()))) ((r1 . ("Firing reaction 1")) (r2 . ("Firing reaction 2"))) (actor id_60 () ((p1 . (r1 r2))) (remove-reactions p1)))))
            (term  ((((p1 . (((:msg x y) nil) ()))) ((r1 . ("Firing reaction 1")) (r2 . ("Firing reaction 2"))) (actor id_60 () () ())))))
  
  )


(define (test-actors)

  ;; Declare a simple actor with a pattern
  (test-->> NEST-Reductions
            (term  ((() () (actor root_1 () () (new-actor (pattern p1 (:msg x y)))))))
            (term  ((() () (actor root_1 () () (ref id_new)))  
                    (((p1 . (((:msg x y) nil) ()))) () (actor id_new () () ())))))

  ;; Declare a simple actor with a reaction
  (test-->> NEST-Reductions
            (term  ((() () (actor root_2 () () (new-actor (reaction r1 "Firing reaction 1"))))))
            (term  ((() () (actor root_2 () () (ref id_new)))  
                    (() ((r1 . ("Firing reaction 1"))) (actor id_new () () ())))))

  ;; Declare an actor with a single pattern and reaction
  (test-->> NEST-Reductions
            (term  ((() () (actor root_3 () () (new-actor (~ (pattern p1 (:msg x y)) (reaction r1 "Firing reaction 1")))))))
            
            (term  ((() () (actor root_3 () () (ref id_new)))  
                    (((p1 . (((:msg x y) nil) ()))) ((r1 . ("Firing reaction 1"))) (actor id_new () () ())))))

  ;; Declare an actor with a multiple patterns and reactions
  (test-->> NEST-Reductions
            (term  ((() () (actor root_4 () () (new-actor (~ (pattern p1 (:msg x y)) (pattern p2 (:msg_x x y)) (reaction r1 "Firing reaction 1") (reaction r2 "Firing reaction 2")))))))
            
            (term  ((() () (actor root_4 () () (ref id_new)))  
                    (((p1 . (((:msg x y) nil) ())) (p2 . (((:msg_x x y) nil) ()))) ((r1 . ("Firing reaction 1")) (r2 . ("Firing reaction 2"))) (actor id_new () () ())))))


  ;; Declare an actor with a binding between its pattern and reaction
  (test-->> NEST-Reductions
            (term  ((() () (actor root_5 () () (new-actor (~ (pattern p1 (:msg x y)) (reaction r1 "Firing reaction 1") (react-to p1 r1)))))))
            
            (term  ((() () (actor root_5 () () (ref id_new)))  
                    (((p1 . (((:msg x y) nil) ()))) ((r1 . ("Firing reaction 1"))) (actor id_new () ((p1 . (r1))) ())))))


  ;; Declare an actor with bindings between its pattern and reactions
  (test-->> NEST-Reductions
            (term  ((() () (actor root_6 () () (new-actor (~ (pattern p1 (:msg x y)) (reaction r1 "Firing reaction 1") (reaction r2 "Firing reaction 2") (react-to p1 r1) (react-to p1 r2)))))))
            
            (term  ((() () (actor root_6 () () (ref id_new)))  
                    (((p1 . (((:msg x y) nil) ()))) ((r1 . ("Firing reaction 1")) (r2 . ("Firing reaction 2"))) (actor id_new () ((p1 . (r1 r2))) ())))))


  ;; Declare an actor with a binding between its patterns and reactions
  (test-->> NEST-Reductions
            (term  ((() () (actor root_7 () () (new-actor (~ (pattern p1 (:msg x y)) (pattern p2 (:msg_x x y)) (reaction r1 "Firing reaction 1") (reaction r2 "Firing reaction 2") (react-to p1 r1) (react-to p2 r2)))))))
            
            (term  ((() () (actor root_7 () () (ref id_new)))  
                    (((p1 . (((:msg x y) nil) ())) (p2 . (((:msg_x x y) nil) ()))) ((r1 . ("Firing reaction 1")) (r2 . ("Firing reaction 2")))  (actor id_new () ((p1 . (r1)) (p2 . (r2))) ())))))


  ;; Declare an actor and remove a previous binding between a pattern and a reaction
  (test-->> NEST-Reductions
            (term  ((() () (actor root_8 () () (new-actor (~ (pattern p1 (:msg x y)) (reaction r1 "Firing reaction 1") (react-to p1 r1) (remove r1 p1)))))))
            
            (term  ((() () (actor root_8 () () (ref id_new)))  
                    (((p1 . (((:msg x y) nil) ()))) ((r1 . ("Firing reaction 1"))) (actor id_new () () ())))))

  ;; Declare an actor and remove a previous binding between a pattern and its reaction
  (test-->> NEST-Reductions
            (term  ((() () (actor root_9 () () (new-actor (~ (pattern p1 (:msg x y)) (pattern p2 (:msg_x x y)) (reaction r1 "Firing reaction 1") (reaction r2 "Firing reaction 2") (react-to p1 r1) (react-to p2 r2) (remove-reactions p1)))))))
            
            (term  ((() () (actor root_9 () () (ref id_new)))  
                    (((p1 . (((:msg x y) nil) ())) (p2 . (((:msg_x x y) nil) ()))) ((r1 . ("Firing reaction 1")) (r2 . ("Firing reaction 2")))  (actor id_new () ((p2 . (r2))) ())))))



  ;; Send a message to an actor without processing it
  (test-->> NEST-Reductions
            (term  ((() () (actor root_10 () () (let (newActor (new-actor (~ (pattern p1 (:msg x y)) (reaction r1 "Firing reaction 1") (react-to p1 r1)))) in (send newActor (:msg2 1)))))))
            (term  ((() () (actor root_10 () () ()))  
                    (((p1 . (((:msg x y) nil) ()))) ((r1 . ("Firing reaction 1"))) (actor id_new ((x_tstamp (:msg2 1)))  ((p1 . (r1))) ())))))


  ;  ;; Process-message by a pattern with one reaction
  (test-->> NEST-E-Reductions
            (term  ((((p1 . (((:msg x y) nil) ()))) ((r1 . ("Firing reaction 1") )) (actor id_new ((x_tstamp (:msg 1 2)) )  ((p1 . (r1))) ()))))
            (term  ((((p1 . (((:msg x y) nil) ()))) ((r1 . ("Firing reaction 1") )) (actor id_new () ((p1 . (r1))) ("Firing reaction 1"))))))
                     
  ;  ;; Process-message by a pattern with two reactions
  (test-->> NEST-E-Reductions
            (term  ((((p1 . (((:msg x y) nil) ()))) ((r1 . ("Firing reaction 1")) (r2 . ("Firing reaction 2"))) (actor id_new ((x_tstamp (:msg 1 2)) )  ((p1 . (r1 r2))) ()))))
            (term  ((((p1 . (((:msg x y) nil) ()))) ((r1 . ("Firing reaction 1")) (r2 . ("Firing reaction 2"))) (actor id_new () ((p1 . (r1 r2))) ("Firing reaction 1" "Firing reaction 2"))))))


  )

;; Execute tests
(test-basic-syntax)
(test-substitution)
(test-patterns)
(test-reactions)
(test-actors)


;; create a new actor and send him a message
(traces NEST-E-Reductions
  (term  ((() () (actor root_10 () () (let (newActor (new-actor (~ (pattern p1 (:msg x y)) (reaction r1 "Firing reaction 1") (react-to p1 r1)))) in (send newActor (:msg2 1)))))))
)


;; NOTE: Do not use `pattern-check` and `pattern-traces` to avoid missleiding statistics in their terminal output. 
;; If you want to get a visual representation of the reductions and a textual summary of it use  `pattern-traces` 

;  (pattern-check NEST-T-Reductions 
;                 (term (pattern p1 (:msg x y))) 
;                 3
;                 )

(pattern-traces NEST-T-Reductions 
                 (term (pattern p1 (:msg x y))) 
                 10
                 )



  (module+ test
    (test-results))