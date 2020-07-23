#lang racket

(require redex "nest-syntax.rkt")

(module+ test

  (define valid-syntax? (redex-match? NEST e))

   ;;; #### BEGIN Selector Type Tests #####
  (define st1 (term :msg_a))

  (test-equal (valid-syntax? st1) #true)
  ;;; #### END Selector Type Tests #####

  ;;; #### BEGIN Message Selectors Tests #####
  (define sl1 (term (:msg1 x y)))
  (define sl2 (term (:msg2 x x)))
  (define sl3 (term (:msg3 x x 3 #true)))

  (test-equal (valid-syntax? sl1) #true)
  (test-equal (valid-syntax? sl1) #true)
  (test-equal (valid-syntax? sl2) #true)
  (test-equal (valid-syntax? sl3) #true)
  ;;; #### END Message Selectors Tests #####



  ;; ##### BEGIN Arithmetic Operations Tets #####
  (define sum (term (+ 3 4)))
  (define mult (term (* 4 5)))
  (define div (term (/ 3 5)))
  (define rest (term (- 5 2)))
  (define equals (term (== 5 2)))
  (define goe (term (>= 5 2)))
  (define loe (term (<= 5 2)))

  (test-equal (valid-syntax? sum) #true)
  (test-equal (valid-syntax? rest) #true)
  (test-equal (valid-syntax? mult) #true)
  (test-equal (valid-syntax? div) #true)
  (test-equal (valid-syntax? goe) #true)
  (test-equal (valid-syntax? loe) #true)

  ;;; #### END Arithmetic Operations Tests #####


  ;;; ##### BEGIN Lambda Tets #####
  (define fn1 (term (λ (x) (+ x x))))
  (define fn2 (term (λ (x y) (* x y))))
  (define fn3 (term (λ () #true)))
  (define fn4 (term (λ x #true)))
 
  (test-equal (valid-syntax? fn1) #true)
  (test-equal (valid-syntax? fn2) #true)
  (test-equal (valid-syntax? fn3) #true)
  (test-equal (valid-syntax? fn4) #false) 
  ;;; #### END Lambda Tests #####


  ;;; ##### BEGIN Patterns Tests #####
  (define s01 (term (:msg1 a b)))
  (define s02 (term ((:msg1 a b) (every 4))))
  (define s03 (term ((:msg1 a b) (count 4))))
  (define s04 (term ((:msg1 a b) (count 4) (fold ,fn1))))
  (define s05 (term ((:msg1 a b) (count 4) (fold ,fn1) (bind x))))
  (define s06 (term (and (:msg1 a b) (:msg2 c d))))
  (define g06 (term (when (> a c))))
 

  (define p1 (term (pattern np1 ,s01)))
  (define p2 (term (pattern np1 ,s02)))
  (define p3 (term (pattern np1 ,s03)))
  (define p4 (term (pattern np1 ,s04)))
  (define p5 (term (pattern np1 ,s05)))
  (define p6 (term (pattern np1 ,s06)))
  (define p7 (term (pattern np1 ,s06 ,g06)))

  (test-equal (valid-syntax? p1) #true)
  (test-equal (valid-syntax? p2) #true)
  (test-equal (valid-syntax? p3) #true)
  (test-equal (valid-syntax? p4) #true)
  (test-equal (valid-syntax? p5) #true)
  (test-equal (valid-syntax? p6) #true)
  (test-equal (valid-syntax? p7) #true)

  ;;; ##### END Patterns Tests #####

  ;;; ##### BEGIN Reaction Tests #####
  
  (define r1 (term (reaction r1 lm st #true)))
  (define r2 (term (reaction r1)))
  
  (test-equal (valid-syntax? r1) #true)
  (test-equal (valid-syntax? r2) #false)

  ;;; ##### END Reaction Tests #####


  ;;; ##### BEGIN Reaction Binding Tests #####
  (define rb1 (term (react-to x ,r1)))
  (define rb2 (term (react-to x)))
  (define rb3 (term (react-to ,r1)))

  (test-equal (valid-syntax? rb1) #true)
  (test-equal (valid-syntax? rb2) #false)
  (test-equal (valid-syntax? rb3) #false)
  ;;; ##### END Reaction Tests #####


  ;;; ##### BEGIN Send Tests #####
  (define m1 (term (:msg1 2 6)))
  (define s1 (term (send x ,m1)))
  (define s2 (term (send x)))
  (define s3 (term (send ,fn1)))

  (test-equal (valid-syntax? s1) #true)
  (test-equal (valid-syntax? s2) #false)
  (test-equal (valid-syntax? s3) #false)
  ;;; ##### END Reaction Tests #####

  (test-results)

)