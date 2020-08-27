#lang racket

(require redex "nest-reductions.rkt")

;; ##### BEGIN Basic Syntax Tests #####
(define (test-basic-syntax)
  (test-->> NEST-Reductions
            (term  ((actor id () () () ()  (+ 2 3))))
            (term  ((actor id () () () ()  5))))

  (test-->> NEST-Reductions
            (term  ((actor id () () () () (- 6 3))))
            (term  ((actor id () () () () 3))))

  (test-->> NEST-Reductions
            (term  ((actor id () () () () (* 2 3))))
            (term  ((actor id () () () () 6))))

  (test-->> NEST-Reductions
            (term  ((actor id () () () () (/ 15 3))))
            (term  ((actor id () () () () 5))))

  )
; ##### END Basic Syntax Tests #####


;; ##### BEGIN Substitution Tests #####
(define (test-substitution)
  (test-->> NEST-Reductions
            (term  ((actor id_01 () () () () (let (x (+ 2 4)) in x))))
            (term  ((actor id_01 () () () () 6))))

  (test-->> NEST-Reductions
            (term  ((actor id_02 () () () () (let (x (* 2 4)) in x))) )
            (term  ((actor id_02 () () () () 8))))

  (test-->> NEST-Reductions
            (term  ((actor id_03 () () () () (let (x (let (x (+ 2 4)) in x)) in  (let (y 0) in  (+ y (+ x 3) ))))))
            (term  ((actor id_03 () () () () 9))))

  (test-->> NEST-Reductions
            (term  ((actor id_03 () () () () (let (x (let (x (+ 2 4)) in x)) in  (let (y (+ x 2)) in  (+ y (+ x 3) ))))))
            (term  ((actor id_03 () () () () 17))))

  (test-->> NEST-Reductions
            (term  ((actor id_04 () () () () (seq ((+ 2 4)) (+ 2 3) ))))
            (term  ((actor id_04 () () () () 5))))

  (test-->> NEST-Reductions
            (term  ((actor id_01 () () () () (let (x (+ 2 4)) in x))))
            (term  ((actor id_01 () () () () 6))))
  )
;; ##### END Substitution Tests #####


;; ##### BEGIN Pattern Tests #####
(define (test-patterns)

  ;; Elementary Pattern
  (test-->> NEST-Reductions
            (term  ((actor id_1 () () () () (pattern p1 (:msg x y)))))
            (term  ((actor id_1 () ((p1 . (((:msg x y) nil) ()))) () () (ref p1)))))

  ;; Elementary Pattern with a guard expression
  (test-->> NEST-Reductions
            (term  ((actor id_2 () () () () (pattern p2 (:msg x y) (when (> x y))))))
            (term  ((actor id_2 () ((p2 . (((:msg x y) (when (> x y))) ()))) () () (ref p2)))))

  ;; Elementary Pattern using the extensional sequencing operator
  (test-->> NEST-Reductions
            (term  ((actor id_3 () () () () (pattern p3 ((:msg x y) (every 3))))))
            (term  ((actor id_3 () ((p3 . ((((:msg x y) (every 3)) nil) ()))) () () (ref p3)))))

  ;; Composite Pattern
  (test-->> NEST-Reductions
            (term  ((actor id_4 () () () () (pattern p4 (and (:msg1 a b) (:msg1 c d))))))
            (term  ((actor id_4 () ((p4 . (((and (:msg1 a b) (:msg1 c d)) nil) ()))) () () (ref p4)))))

  ;; Composite Pattern with a guard expression
  (test-->> NEST-Reductions
            (term  ((actor id_5 () () () () (pattern p5 (and (:msg1 a b) (:msg1 c d)) (when (> a c))))))
            (term  ((actor id_5 () ((p5 . (((and (:msg1 a b) (:msg1 c d)) (when (> a c))) ()))) () () (ref p5)))))

  ;; Quantified Accumulation Pattern 
  (test-->> NEST-Reductions
            (term  ((actor id_6 () () () () (pattern p6 ((:msg x y) (count 3))))))
            (term  ((actor id_6 () ((p6 . ((((:msg x y) (count 3)) nil) ()))) () () (ref p6)))))
           

  ;; Quantified Accumulation Pattern with a guard expression
  (test-->> NEST-Reductions
            (term  ((actor id_7 () () () () (pattern p7 ((:msg x y) (count 3)) (when (> x y)) ))))
            (term  ((actor id_7 () ((p7 . ((((:msg x y) (count 3)) (when (> x y))) ()))) () () (ref p7)))))

  ;; Composite/Quantified Accumulation Pattern
  (test-->> NEST-Reductions
            (term  ((actor id_8 () () () () (pattern p8 (and ((:msg1 a b) (every 3)) ((:msg1 c d) (every 3)))))))
            (term  ((actor id_8 () ((p8 . (((and ((:msg1 a b) (every 3)) ((:msg1 c d) (every 3))) nil) ()))) () () (ref p8)))))


  ;; Add multiple patterns to an actor
  (test-->> NEST-Reductions
            (term  ((actor id_1 () () () () ((pattern p1 (:msg x y)) (pattern p2 (:temp x y z))))))
            (term  ((actor id_1 () ((p1 . (((:msg x y) nil) ())) (p2 . (((:temp x y z) nil) ()))) () () ((ref p1) (ref p2))))))


  
  )
;; ##### END Pattern Tests #####


;; ##### BEGIN Reactions Tests #####

(define (test-reactions)
  ;; Reaction definition
  (test-->> NEST-Reductions
            (term  ((actor id_00 () () () () (reaction r1 lm ir st #true))))
            (term  ((actor id_00 () () ((r1 . (lm ir st #true))) () (ref r1)))))

  ;; Multiple reaction definitions
  (test-->> NEST-Reductions
            (term  ((actor id_10 () () () () ((reaction r1 lm ir st #true) (reaction r2 lm ir st #true)))))
            (term  ((actor id_10 () () ((r1 . (lm ir st #true)) (r2 . (lm ir st #true))) () ((ref r1) (ref r2))))))

  ;; Reaction definition in a let expression 
  (test-->> NEST-Reductions
            (term  ((actor id_20 () () () () (let (x (reaction r1 lm ir st #true)) in x))) )
            (term  ((actor id_20 () () ((r1 . (lm ir st #true))) () (ref r1)))))

  ;; Add a reaction to pattern
  (test-->> NEST-Reductions
            (term  ((actor id_30 () () () () (let (prf (let (prf (pattern p1 (:msg x y))) in prf)) 
                                               in  (let (rrf (reaction r1 lm ir st #true)) 
                                                     in  (react-to prf rrf))))))
            (term  ((actor id_30 () ((p1 . (((:msg x y) nil) ()))) ((r1 . (lm ir st #true))) (((ref p1) . ((ref r1)))) ()))))


  ;; Add a multiple reactions to pattern
  (test-->> NEST-Reductions
            (term  ((actor id_40 () ((p1 . (((:msg x y) nil) ()))) ((r1 . (lm ir st #true)) (r2 . (lm ir st #true))) (((ref p1) . ((ref r1))))  (react-to (ref p1) (ref r2))))) 
            (term  ((actor id_40 () ((p1 . (((:msg x y) nil) ()))) ((r1 . (lm ir st #true)) (r2 . (lm ir st #true))) (((ref p1) . ((ref r1) (ref r2)))) ()))))

  ;; Remove a reaction from a pattern
  (test-->> NEST-Reductions
            (term  ((actor id_50 () ((p1 . (((:msg x y) nil) ()))) ((r1 . (lm ir st #true)) (r2 . (lm ir st #true))) (((ref p1) . ((ref r1) (ref r2)))) (remove (ref p1) (ref r2)))))
            (term  ((actor id_50 () ((p1 . (((:msg x y) nil) ()))) ((r1 . (lm ir st #true)) (r2 . (lm ir st #true))) (((ref p1) . ((ref r1)))) ()))))


  ;; Remove all reaction from a pattern
  (test-->> NEST-Reductions
            (term  ((actor id_60 () ((p1 . (((:msg x y) nil) ()))) ((r1 . (lm ir st #true)) (r2 . (lm ir st #true))) (((ref p1) . ((ref r1) (ref r2)))) (remove-reactions (ref p1)))))
            (term  ((actor id_60 () ((p1 . (((:msg x y) nil) ()))) ((r1 . (lm ir st #true)) (r2 . (lm ir st #true))) () ()))))
  
  )


(define (test-actors)

  ;; Declare a simple actor
  (test-->> NEST-Reductions
            (term  ((actor root () () () () (new-actor (let (x (/ 8 4)) in x)))))
            (term  ((actor root () () () () (ref id_new))  
                    (actor id_new () () () () 2))))
  
  ;; Declare a simple actor
  (test-->> NEST-Reductions
            (term  ((actor root () () () () (new-actor (pattern p1 (:msg x y))))))
            (term  ((actor root () () () () (ref id_new))  
                    (actor id_new () ((p1 . (((:msg x y) nil) ()))) () () (ref p1)))))

  ;; Declare an actor with a single pattern and reaction
  (test-->> NEST-Reductions
            (term  ((actor root () () () () (new-actor (let (prf (pattern p1 (:msg x y))) 
                                                         in  (let (rrf (reaction r1 lm ir st #true)) 
                                                               in  (react-to prf rrf)))))))
            (term  ((actor root () () () () (ref id_new))  
                    (actor id_new () ((p1 . (((:msg x y) nil) ()))) ((r1 . (lm ir st #true))) (((ref p1) . ((ref r1)))) ()))))



  ;; Send a message (no-match) to an actor
  (test-->> NEST-Reductions
            (term  ((actor root () () () () (let (newActor (new-actor (let (prf (pattern p1 (:msg x y)))
                                                                        in  (let (rrf (reaction r1 lm ir st #true)) 
                                                                              in  (react-to prf rrf)))))
                                              in (send newActor (:msg2 1))))))
            (term  ((actor root () () () () ())  
                    (actor id_new ((id_msg x_tstamp (:msg2 1))) ((p1 . (((:msg x y) nil) ()))) ((r1 . (lm ir st #true))) (((ref p1) . ((ref r1)))) ()))))


  ;; Process-message by a pattern with one reaction
  (test-->> NEST-Reductions
            (term  ((actor id_new ((id_msg x_tstamp (:msg 1 2)) ) ((p1 . (((:msg x y) nil) ()))) ((r1 . (lm ir st "Reaction #1") )) (((ref p1) . ((ref r1)))) nil)))
            (term  ((actor id_new () ((p1 . (((:msg x y) nil) ()))) ((r1 . (lm ir st "Reaction #1"))) (((ref p1) . ((ref r1)))) ("Reaction #1")))))

  ;; Process-message by a pattern with two reactions
  (test-->> NEST-Reductions
            (term  ((actor id_new ((id_msg x_tstamp (:msg 1 2)) ) ((p1 . (((:msg x y) nil) ()))) ((r1 . (lm ir st "Reaction #1") ) (r2 . (lm ir st "Reaction #2") )) (((ref p1) . ((ref r1) (ref r2)))) nil)))
            (term  ((actor id_new () ((p1 . (((:msg x y) nil) ()))) ((r1 . (lm ir st "Reaction #1")) (r2 . (lm ir st "Reaction #2") )) (((ref p1) . ((ref r1) (ref r2)))) ("Reaction #1" "Reaction #2")))))


  )

;
;(traces NEST-Reductions
;       (term  ((actor id_new () () () () nil)))
;        )


(test-basic-syntax)
(test-substitution)
(test-patterns)
(test-reactions)
(test-actors)


(module+ test
  (test-results))