#lang racket

(require redex "nest-syntax.rkt")

;; Handwritten tests
(module+ test

   ;; ################################## BEGIN NEST Expressions Tests ##################################
   (define valid-e-exp? (redex-match? NEST e))

   ;; ##### BEGIN Actor Operations Tets #####
   (define spawn (term (spawn an_1)))
   (define spawn2 (term (spawn 2)))
   (define send (term (send an_1 1)))
   (define send2 (term (send "an_1")))
   (define react-to (term (react-to pn_1 rn_2)))
   (define react-to2 (term (react-to a)))
   (define remove (term (remove rn_1 pn_1)))
   (define remove2 (term (remove rn_1 "pn_!")))
   (define remove-reactions (term (remove-reactions pn_1)))

   (test-equal (valid-e-exp? spawn) #true)
   (test-equal (valid-e-exp? spawn2) #false)
   (test-equal (valid-e-exp? send) #true)
   (test-equal (valid-e-exp? send2) #false)
   (test-equal (valid-e-exp? react-to) #true)
   (test-equal (valid-e-exp? remove) #true)
   (test-equal (valid-e-exp? remove2) #false)
   (test-equal (valid-e-exp? remove-reactions) #true)

  ;;; #### END Actor Operations Tests #####

  ;; ##### BEGIN Arithmetic Operations Tets #####
  (define sum (term (+ 3 4)))
  (define mult (term (* 4 5)))
  (define div (term (/ 3 5)))
  (define rest (term (- 5 2)))

  (test-equal (valid-e-exp? sum) #true)
  (test-equal (valid-e-exp? rest) #true)
  (test-equal (valid-e-exp? mult) #true)
  (test-equal (valid-e-exp? div) #true)
  ;;; #### END Arithmetic Operations Tests #####


  ;; ##### BEGIN Comparison Operations Tets #####
  (define equals (term (== 5 2)))
  (define goe (term (>= 5 2)))
  (define loe (term (<= 5 2)))

  (test-equal (valid-e-exp? equals) #true)
  (test-equal (valid-e-exp? goe) #true)
  (test-equal (valid-e-exp? loe) #true)
  ;;; #### END Arithmetic Operations Tests #####


  ;;; ##### BEGIN Lambda Tets #####
  (define fn1 (term (λ (x) (+ x x))))
  (define fn2 (term (λ (x y) (* x y))))
  (define fn3 (term (λ () #true)))
  (define fn4 (term (λ x #true)))
 
  (test-equal (valid-e-exp? fn1) #true)
  (test-equal (valid-e-exp? fn2) #true)
  (test-equal (valid-e-exp? fn3) #true)
  (test-equal (valid-e-exp? fn4) #false) 
  ;;; #### END Lambda Tests #####

  (println "Randomized tests for expressions")
  ;; Randomized tests
  (redex-check
    NEST
    e_i
    (redex-match? NEST e (term e_i))
    #:attempts 1000)
  ;; ################################## END NEST Expressions Tests ##################################

  ;; ################################## BEGIN Patern Tests         ##################################
  (define valid-pe-exp? (redex-match? NEST pe))

  (define s01 (term (:msg1 3)))
  (define s02 (term ((:msg2 4) (every 4))))
  (define s03 (term ((:msg1 a b) (count 4))))
  (define s04 (term ((:msg1 a b) (count 4) (fold ,fn1))))
  (define s05 (term ((:msg1 a b) (count 4) (fold ,fn1) (bind x))))
  (define s06 (term (and (:msg1 a b) (:msg2 c d))))
  (define g06 (term (when (> a c))))
  (define s07 (term (not (:msg2 1) (window 5 secs))))
 

  (define p1 (term (pattern pn_1 ,s01)))
  (define p2 (term (pattern pn_2 ,s02)))
  (define p3 (term (pattern pn_3 ,s03)))
  (define p4 (term (pattern pn_4 ,s04)))
  (define p5 (term (pattern pn_5 ,s05)))
  (define p6 (term (pattern pn_6 ,s06)))
  (define p6a (term (pattern pn_6a (,s06 ,g06))))
  (define p7 (term (pattern pn_7 ,s07)))

  (test-equal (valid-pe-exp? p1) #true)
  (test-equal (valid-pe-exp? p2) #true)
  (test-equal (valid-pe-exp? p3) #true)
  (test-equal (valid-pe-exp? p4) #true)
  (test-equal (valid-pe-exp? p5) #true)
  (test-equal (valid-pe-exp? p6) #true)
  (test-equal (valid-pe-exp? p6a) #true)
  (test-equal (valid-pe-exp? p7) #true)
  
  (println "Randomized tests for patterns")
  ;; Randomized tests
  (redex-check
    NEST
    pe_i
    (redex-match? NEST pe (term pe_i))
    #:attempts 1000)

  ;; ################################## END Patern Tests        ##################################


  ;; ################################## BEGIN Reactions Tests     ##################################
  (define valid-re-exp? (redex-match? NEST re))

  (define r1 (term (reaction rn_1 #true)))
  (define r2 (term (reaction rn_1)))
  (define r3 (term (reaction rn_2  "firing reaction 3")))
  
  (test-equal (valid-re-exp? r1) #true)
  (test-equal (valid-re-exp? r2) #false)
  (test-equal (valid-re-exp? r3) #true)
  
  (println "Randomized tests for reactions")
  ;; Randomized tests
  (redex-check
    NEST
    re_i
    (redex-match? NEST re (term re_i))
    #:attempts 1000)
  ;; ################################## END Reactions Tests ##################################

 
  

  (test-results)
)




