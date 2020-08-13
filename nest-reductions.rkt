#lang racket

(require redex "nest-syntax.rkt" "nest-utils.rkt")
(provide NEST-Reductions)

(define NEST-Reductions
  (reduction-relation
   NEST             ;; language
   #:domain k       ;; specifies a contract, in this case declaring that red relates terms matching the pattern p.

   ;;  Arithmetic operations
   (--> 
    ;;in-hole says K must be filled with (/ ......)
    (in-hole K (/ number_1 0))         ;; pattern
    ,(error 'DivisionByZeroError)      ;; expression
    "error")
   
   (--> 
    (in-hole K (+ number_1 number_2))
    (in-hole K ,(+ (term number_1) (term number_2)))
    "add")
   
   (-->
    (in-hole K (* number_1 number_2))
    (in-hole K ,(* (term number_1) (term number_2)))
    "multiply")
   
   (-->
    (in-hole K (/ number_1 number_2))
    (in-hole K ,(/ (term number_1) (term number_2)))
    "divide")
   
   (--> 
    (in-hole K (- number_1 number_2))
    (in-hole K ,(- (term number_1) (term number_2)))
    "subtract")

   (--> 
    (in-hole K  (actor id q pl rl pr (in-hole E (seq any_1 any_2)))  )
    ,(term-let ((x_1 (variable-not-in (term any_2) (term var))))
               (term (in-hole K (actor id q pl rl pr (in-hole E (let (x_1 any_1) in any_2))))))
    "syntax-expansion-seq")

   (-->
    (in-hole K (let (x_1 e_1) in e_2))
    (in-hole K (subst (x_1 e_1 e_2))) 
    "let")

   (-->
    (in-hole K (actor id q pl rl pr (in-hole E (pattern x pb))))
    ,(term-let ([p-item (term ((pb nil) ()))])
               (term 
                (in-hole K (actor id q (NEW-PATTERN pl x p-item) rl pr (in-hole E (ref x))))))
    "add-pattern")

   (-->
    (in-hole K (actor id q pl rl pr (in-hole E (pattern x pb g))))
    ,(term-let ([p-item (term ((pb g) ()))])
               (term 
                (in-hole K (actor id q (NEW-PATTERN pl x p-item) rl pr (in-hole E (ref x))))))
    "add-pattern-with-guard")

   (-->
    (in-hole K (actor id q pl rl pr (in-hole E (reaction x_1 x_2 x_3 x_4 e))))
    ,(term-let ([r-item (term (x_2 x_3 x_4 e))])
               (term 
                (in-hole K (actor id q pl (NEW-REACTION rl x_1 r-item) pr (in-hole E (ref x_1))))))
    "add-reaction")

   (-->
    (in-hole K (actor id q pl rl pr (in-hole E (react-to rf_1 rf_2))))
    ,(term-let ([ptr (term rf_1)]
                [rct (term rf_2)])
               (term 
                (in-hole K (actor id q pl rl (ADD-REACTION pr ptr rct) ()))))
    "react-to")

   (-->
    (in-hole K (actor id q pl rl pr (in-hole E (remove rf_1 rf_2))))
    ,(term-let ([ptr (term rf_1)]
                [rct (term rf_2)])
               (term 
                (in-hole K (actor id q pl rl (REMOVE-REACTION pr ptr rct) ()))))
    "remove-reaction")

   (-->
    (in-hole K (actor id q pl rl pr (in-hole E (remove-reactions rf_1))))
    ,(term-let ([ptr (term rf_1)])
               (term 
                (in-hole K (actor id q pl rl () ()))))
    "remove-reactions")

   (--> 
    ((actor id_0 q_0 pl_0 rl_0 pr_0 any_0) ...  
     (actor id_a q_a pl_a rl_a pr_a (in-hole E (new-actor e)))   
     (actor id_n q_n pl_n rl_n pr_n any_n) ...)
        
    ((actor id_0 q_0 pl_0 rl_0 pr_0 any_0) ...  
     (actor id_a  q_a pl_a rl_a pr_a (in-hole E (ref id_new)))
     (actor id_new ()  ()  ()  ()   e)
     (actor id_n q_n pl_n rl_n pr_n any_n) ...)
    (fresh id_new)
    "new-actor")

   (-->
    ((actor id_0 q_0 pl_0 rl_0 pr_0 any_0) ...  
     (actor id_1 q_1 pl_1 rl_1 pr_1 (in-hole E (send (ref id_3) e_msg)))   
     (actor id_2 q_2 pl_2 rl_2 pr_2  any_2) ... 
     (actor id_3 q_3 pl_3 rl_3 pr_3  any_3)
     (actor id_n q_n pl_n rl_n pr_n  any_n) ...)

    ((actor id_0 q_0 pl_0 rl_0 pr_0 any_0) ...  
     (actor id_1 q_1 pl_1 rl_1 pr_1 ())   
     (actor id_2 q_2 pl_2 rl_2 pr_2  any_2) ... 
     (actor id_3 (ADD-MESSAGE q_3 ((id_msg x_tstamp e_msg))) pl_3 rl_3 pr_3  any_3)
     (actor id_n q_n pl_n rl_n pr_n  any_n) ...)
    (fresh id_msg)
    (fresh x_tstamp)
    "send-message")


   (-->
    (in-hole K  (actor id_a (m_a ... (id_msg x_tstamp e_msg)) pl_a rl_a pr_a  v_a))
    (PROCESS-MESSAGE id_a (m_a ... (id_msg x_tstamp e_msg)) pl_a rl_a pr_a  v_a)
    "process-message")

   ))