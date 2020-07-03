#lang racket

(require redex "nest-syntax.rkt" "nest-utils.rkt")

;;; #### BEGIN Message Selectors Tests #####
(module+ test
  (define mp1 (term (:header1 x y)))
  (define mp2 (term (:header2 x x)))
  (define mp3 (term (:header3 x x 3 #true)))
  (define mp4 (term (header2 x x)))

  (define valid-selector? (redex-match? Sparrow mp))

  (test-equal (valid-selector? mp1) #true)
  (test-equal (valid-selector? mp2) #true)
  (test-equal (valid-selector? mp3) #true)
  (test-equal (valid-selector? mp4) #false)
)
;;; #### END Message Selectors Tests #####


;;; #### BEGIN Selector Type Tests #####
(module+ test
  (define st1 (term :msg_a))
  (define st2 (term msg_a))

  (define valid-type? (redex-match? Sparrow h))

  (test-equal (valid-type? st1) #true)
  (test-equal (valid-type? st2) #false)
)
;;; #### END Selector Type Tests #####



;;; #### BEGIN Message Tests #####
(module+ test
  (define m1 (term (:msg_a 2 1)))
  (define m2 (term (:msg_b 3 #true 3.3 "hi")))
  (define m3 (term ("msg_a" x "hi")))
  (define m4 (term (msg_d x x)))

  (define valid-message? (redex-match? Sparrow m))

  (test-equal (valid-message? m1) #true)
  (test-equal (valid-message? m2) #true)
  (test-equal (valid-message? m3) #false)
  (test-equal (valid-message? m4) #false)
)
;;; #### END Message Tests #####


;;; #### BEGIN Message Match Tests #####
(module+ test
 (test-equal  (message-match? m1 mp1) #true)
 (test-equal  (message-match? m2 mp2) #false)
 (test-equal  (message-match? m2 mp1) #false)
)
;;; #### END Message-To-MessagePattern-Match Tests #####


;;; #### BEGIN Operators Tests #####
;; Accumulation 
(module+ test
  (define a1 (term (count 3)))
  (define a2 (term (count 3.3)))
  (define a3 (term (count "2")))

  (define valid-accumulation? (redex-match? Sparrow α))

  (test-equal (valid-accumulation? a1) #true)
  (test-equal (valid-accumulation? a2) #false)
  (test-equal (valid-accumulation? a3) #false)
)

;; Time-units
(module+ test
  (define tu1 (term secs ))
  (define tu2 (term mins ))
  (define tu3 (term hours ))
  (define tu4 (term days ))
  (define tu5 (term weeks ))
  (define tu6 (term months ))
  (define tu7 (term 2 ))
  (define tu8 (term "months" ))

  (define valid-time-unit? (redex-match? Sparrow u))

  (test-equal (valid-time-unit? tu1) #true)
  (test-equal (valid-time-unit? tu2) #true)
  (test-equal (valid-time-unit? tu3) #true)
  (test-equal (valid-time-unit? tu4) #true)
  (test-equal (valid-time-unit? tu5) #true)
  (test-equal (valid-time-unit? tu6) #false)
  (test-equal (valid-time-unit? tu7) #false)
  (test-equal (valid-time-unit? tu8) #false)
)

;; Windows
(module+ test
  (define w1 (term (window 3 secs)))
  (define w2 (term (window 4 mins)))
  (define w3 (term (window 1 hours)))
  (define w4 (term (window 2 days)))
  (define w5 (term (window 3 weeks)))
  (define w6 (term (window 3.3 mins)))
  (define w7 (term (window "2" hours)))

  (define valid-window? (redex-match? Sparrow τ))

  (test-equal (valid-window? w1) #true)
  (test-equal (valid-window? w2) #true)
  (test-equal (valid-window? w3) #true)
  (test-equal (valid-window? w4) #true)
  (test-equal (valid-window? w5) #true)
  (test-equal (valid-window? w6) #false)
  (test-equal (valid-window? w7) #false)
)

;; Debounce
(module+ test
  (define d1 (term (debounce 3 secs)))
  (define d2 (term (debounce 4 mins)))
  (define d3 (term (debounce 1 hours)))
  (define d4 (term (debounce 2 days)))
  (define d5 (term (debounce 3 weeks)))
  (define d6 (term (debounce 3.3 mins)))
  (define d7 (term (debounce "2" hours)))

  (define valid-debounce? (redex-match? Sparrow ∂))

  (test-equal (valid-debounce? d1) #true)
  (test-equal (valid-debounce? d2) #true)
  (test-equal (valid-debounce? d3) #true)
  (test-equal (valid-debounce? d4) #true)
  (test-equal (valid-debounce? d5) #true)
  (test-equal (valid-debounce? d6) #false)
  (test-equal (valid-debounce? d7) #false)
)


;; Every
(module+ test
  (define e1 (term (every 3)))
  (define e2 (term (every 4 mins)))
  (define e3 (term (every hour)))
  (define e4 (term (every "3")))
  (define e5 (term (every 3.3)))

  (define valid-every? (redex-match? Sparrow ℶ))

  (test-equal (valid-every? e1) #true)
  (test-equal (valid-every? e2) #false)
  (test-equal (valid-every? e3) #false)
  (test-equal (valid-every? e4) #false)
  (test-equal (valid-every? e5) #false)
)


; #### END Operators Tests #####


;; ##### BEGIN Arithmetic Operations Tets #####
(module+ test
  (define sum (term (+ 3 4)))
  (define mult (term (* 4 5)))
  (define div (term (/ 3 5)))
  (define rest (term (- 5 2)))
  (define equals (term (== 5 2)))
  (define goe (term (>= 5 2)))
  (define loe (term (<= 5 2)))

  (define math-op? (redex-match? Sparrow e))

  (test-equal (math-op? sum) #true)
  (test-equal (math-op? rest) #true)
  (test-equal (math-op? mult) #true)
  (test-equal (math-op? div) #true)
  (test-equal (math-op? goe) #true)
  (test-equal (math-op? loe) #true)

) 

;; #### END Arithmetic Operations Tests #####


;; ##### BEGIN Lambda Tets #####
(module+ test
  (define fn1 (term (λ (x) (+ x x))))
  (define fn2 (term (λ (x y) (* x y))))
  (define fn3 (term (λ () #true)))
  (define fn4 (term (λ x #true)))
 
  (define lambda? (redex-match? Sparrow e))

  (test-equal (lambda? fn1) #true)
  (test-equal (lambda? fn2) #true)
  (test-equal (lambda? fn3) #true)
  (test-equal (lambda? fn4) #false)) 
;; #### END Lambda Tests #####


;; ##### BEGIN Transformer Tests #####
;; fold transformer
(module+ test
  (define fold1 (term (fold ,fn1)))
  (define fold2 (term (fold () ,fn1)))

  (define valid_fold? (redex-match? Sparrow f))

  (test-equal (valid_fold? fold1) #true)
  (test-equal (valid_fold? fold2) #false))

;; bind
(module+ test  
  (define bind1 (term (bind x)))
  (define bind2 (term (bind ())))
 
  (define valid_bind? (redex-match? Sparrow b))

  (test-equal (valid_bind? bind1) #true)
  (test-equal (valid_bind? bind2) #false))
;; ##### END Transformers Tests #####


;;; ##### BEGIN Patterns Tests #####
;;; patterns
;(module+ test
;  (define p0 (term (pattern ,mp1 ,a1 ,w1 )))
;  (define p1 (term (pattern ,mp1 ,a1 ,a1 ,w1 )))
;  (define p2 (term (pattern ,mp1 ,a1 ,foldr1)))
;  (define p3 (term (pattern ,mp1 ,a1 ,w1 ,map1 ,map1  ,foldr1)))
;  (define p4 (term (pattern () )))
;  (define p5 (term (pattern ,mp1)))
;  
;  (define pattern? (redex-match? Sparrow e))
;  
;  (test-equal (pattern? p0) #true)
;  (test-equal (pattern? p1) #false)
;  (test-equal (pattern? p2) #true)
;  (test-equal (pattern? p3) #true)
;  (test-equal (pattern? p4) #false)
;  (test-equal (pattern? p5) #true))
;;; ##### END Patterns Tests #####


;; ##### BEGIN Reaction Tests #####
(module+ test
  (define r1 (term (react x ,fn1)))
  (define r2 (term (react x)))
  (define r3 (term (react ,fn1)))
  
  (define reaction? (redex-match? Sparrow e))

  (test-equal (reaction? r1) #true)
  (test-equal (reaction? r2) #false)
  (test-equal (reaction? r3) #false))
;; ##### END Reaction Tests #####


;; ##### BEGIN Send Tests #####
(module+ test
  (define s1 (term (send x ,m1)))
  (define s2 (term (send x)))
  (define s3 (term (send ,fn1)))
  
  (define send? (redex-match? Sparrow e))

  (test-equal (send? s1) #true)
  (test-equal (send? s2) #false)
  (test-equal (send? s3) #false))
;; ##### END Reaction Tests #####

;;; ##### BEGIN Match Tests #####
;(module+ test
;  (define match1 (term (match ,p0 ,p2 ,p3)))
;  (define match2 (term (match ,p0 ,p2 ,p2)))
;   
;  (define match? (redex-match? Sparrow e))
;
;  (test-equal (match? match1) #true)
;  (test-equal (match? match2) #false))
;;; ##### END Match Tests #####


(module+ test
  (test-results))