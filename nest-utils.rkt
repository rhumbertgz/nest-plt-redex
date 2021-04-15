#lang racket

(require redex "nest-syntax.rkt")
(provide (all-defined-out))

(define logging #true)

(define (log v #:level [level "TRACE"] #:label [label ""])
  (let ([l (if (equal? label "") "" (string-append label ": "))])
    (if (equal? logging #true) (displayln (string-append level " -> " l (any->string v))) (display ""))  
    ))
  


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

(define (any->string x)
  (call-with-output-string
   (lambda (out)
     (display x out))))

(define (log-reaction r l) 
  (let* ([header (string-append (any->string r) " --> ")]
         [msgs (map (lambda (m) 
                      (inc-consumed-msgs-counter)
                      (any->string (last m))) l)])
    (register-consumed-msgs msgs)
    (string-append header (any->string msgs))))