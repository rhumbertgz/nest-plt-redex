#lang racket

(require redex "nest-syntax.rkt" "nest-utils.rkt")
(provide NEST-Reductions NEST-E-Reductions NEST-T-Reductions)


(define NEST-Reductions
  (reduction-relation
   NEST-R             ;; language
   #:domain k       ;; specifies a contract

   ;;  Arithmetic operations
   [ --> 
     ;;in-hole says K must be filled with (/ ......)
     (in-hole K (/ number_1 0))         ;; pattern
     ,(error 'DivisionByZeroError)      ;; expression
     "error"]
   
   [ --> 
     (in-hole K (+ number_1 number_2))
     (in-hole K ,(+ (term number_1) (term number_2)))
     "add"]
   
   [ -->
     (in-hole K (* number_1 number_2))
     (in-hole K ,(* (term number_1) (term number_2)))
     "multiply"] 
   
   [ -->
     (in-hole K (/ number_1 number_2))
     (in-hole K ,(/ (term number_1) (term number_2)))
     "divide"]
   
   [ --> 
     (in-hole K (- number_1 number_2))
     (in-hole K ,(- (term number_1) (term number_2)))
     "subtract"]

      (--> 
       (in-hole K  (pl rl (actor id q pr (in-hole E (seq any_1 any_2))))  )
       ,(term-let ((x_1 (variable-not-in (term any_2) (term var))))
                  (term (in-hole K (pl rl (actor id q pr (in-hole E (let (x_1 any_1) in any_2)))))))
       "syntax-expansion-seq")

   [ -->
     (in-hole K (let (x_1 e_1) in e_2))
     (in-hole K (subst (x_1 e_1 e_2))) 
     "let"]
   
   [ -->
     (in-hole K (pl rl (actor id q pr (in-hole E (~ (pattern x pb) e_i e_x e_y ...)))))
     ,(term-let ([p-item (term ((pb nil) ()))])
                (term 
                 (in-hole K ((NEW-PATTERN pl x p-item) rl (actor id q pr (in-hole E (~ e_i e_x e_y ...)))))))
     "add-patterns-from-list"]

   [ -->
     (in-hole K (pl rl (actor id q pr (in-hole E (~ (pattern x pb) e_i)))))
     ,(term-let ([p-item (term ((pb nil) ()))])
                (term 
                 (in-hole K ((NEW-PATTERN pl x p-item) rl (actor id q pr (in-hole E e_i))))))
     "add-patterns-from-pair"]
 
   [-->
    (in-hole K (pl rl (actor id q pr (in-hole E (pattern x pb)))))
    ,(term-let ([p-item (term ((pb nil) ()))])
               (term 
                (in-hole K ((NEW-PATTERN pl x p-item) rl (actor id q pr ())))))
    "add-pattern"]

  

   [-->
    (in-hole K (pl rl (actor id q pr (in-hole E (pattern x (pb g)) ))))
    ,(term-let ([p-item (term ((pb g) ()))])
               (term 
                (in-hole K ((NEW-PATTERN pl x p-item) rl (actor id q pr ())))))
    "add-pattern-with-guard"]

    [-->
    (in-hole K (pl rl (actor id q pr (in-hole E (~ (pattern x (pb g)) e_i e_x e_y ...)))))
    ,(term-let ([p-item (term ((pb g) ()))])
               (term 
                (in-hole K ((NEW-PATTERN pl x p-item) rl (actor id q pr (in-hole E (~ e_i e_x e_y ...)))))))
    "add-pattern-with-guard-from-list"]

     [-->
    (in-hole K (pl rl (actor id q pr (in-hole E (~ (pattern x (pb g))  e_i)))))
    ,(term-let ([p-item (term ((pb g) ()))])
               (term 
                (in-hole K ((NEW-PATTERN pl x p-item) rl (actor id q pr (in-hole E e_i))))))
    "add-pattern-with-guard-from-pair"]

   [-->
    (in-hole K (pl rl (actor id q pr (in-hole E (~ (reaction rn e) e_i e_x e_y ...)))))
    ,(term-let ([r-item (term (e))])
               (term 
                (in-hole K (pl (NEW-REACTION rl rn r-item) (actor id q pr (in-hole E (~ e_i e_x e_y ...)))))))
    "add-reactions-from-list"]

   [ -->
     (in-hole K (pl rl (actor id q pr (in-hole E (~ (reaction rn e) e_i)))))
     ,(term-let ([r-item (term (e))])
                (term 
                 (in-hole K (pl (NEW-REACTION rl rn r-item) (actor id q pr (in-hole E e_i))))))
     "add-reactions-from-pair"]



   [-->
    (in-hole K (pl rl (actor id q pr (in-hole E (reaction rn e)))))
    ,(term-let ([r-item (term (e))])
               (term 
                (in-hole K (pl (NEW-REACTION rl rn r-item) (actor id q pr ())))))
    "add-reaction"]

   [-->
    (in-hole K (pl rl (actor id q pr (in-hole E (~ (react-to pn_a rn_b) e_i e_x e_y ...)))))
    ,(term-let ([ptr (term pn_a)]
                [rct (term rn_b)])
               (term 
                (in-hole K (pl rl (actor id q (ADD-REACTION pr ptr rct) (in-hole E (~ e_i e_x e_y ...)))))))
    "react-to-from-list"]

   [-->
    (in-hole K (pl rl (actor id q pr (in-hole E (~ (react-to pn_a rn_b) e_i)))))
    ,(term-let ([ptr (term pn_a)]
                [rct (term rn_b)])
               (term 
                (in-hole K (pl rl (actor id q (ADD-REACTION pr ptr rct) (in-hole E e_i))))))
    "react-to-from-pair"]


   [ -->
     (in-hole K (pl rl (actor id q pr (in-hole E (react-to pn_a rn_b)))))
     ,(term-let ([ptr (term pn_a)]
                 [rct (term rn_b)])
                (term 
                 (in-hole K (pl rl (actor id q (ADD-REACTION pr ptr rct) ())))))
     "react-to"]

   [-->
    (in-hole K (pl rl (actor id q pr (in-hole E (~ (remove rn_a pn_b) e_i e_x e_y ...)))))
    ,(term-let ([ptr (term pn_b)]
                [rct (term rn_a)])
               (term 
                (in-hole K (pl rl (actor id q (REMOVE-REACTION pr ptr rct) (in-hole E (~ e_i e_x e_y ...)))))))
    "remove-reaction-from-list"]

   [-->
    (in-hole K (pl rl (actor id q pr (in-hole E (~ (remove rn_a pn_b) e_i)))))
    ,(term-let ([ptr (term pn_b)]
                [rct (term rn_a)])
               (term 
                (in-hole K (pl rl (actor id q (REMOVE-REACTION pr ptr rct) (in-hole E e_i))))))
    "remove-reaction-from-pair"]

   [-->
    (in-hole K (pl rl (actor id q pr (in-hole E (remove rn_a pn_b)))))
    ,(term-let ([ptr (term pn_b)]
                [rct (term rn_a)])
               (term 
                (in-hole K (pl rl (actor id q (REMOVE-REACTION pr ptr rct) ())))))
    "remove-reaction"]

   [-->
    (in-hole K (pl rl (actor id q pr (in-hole E (~ (remove-reactions rn_a) e_i e_x e_y ...)))))
    ,(term-let ([ptr (term rn_a)])
               (term 
                (in-hole K (pl rl (actor id q (REMOVE-REACTIONS pr ptr) (in-hole E (~ e_i e_x e_y ...)))))))
    "remove-reactions-from-list"]

   [-->
    (in-hole K (pl rl (actor id q pr (in-hole E (~ (remove-reactions rn_a) e_i)))))
    ,(term-let ([ptr (term rn_a)])
               (term 
                (in-hole K (pl rl (actor id q (REMOVE-REACTIONS pr ptr) (in-hole E e_i))))))
    "remove-reactions-from-pair"]

   [-->
    (in-hole K (pl rl (actor id q pr (in-hole E (remove-reactions rn_a)))))
    ,(term-let ([ptr (term rn_a)])
               (term 
                (in-hole K (pl rl (actor id q (REMOVE-REACTIONS pr ptr) ())))))
    "remove-reactions"]


   [--> 
    ((pl_0 rl_0 (actor id_0 q_0 pr_0 any_0)) ...  
     (pl_a rl_a (actor id_a q_a pr_a (in-hole E (new-actor e))))   
     (pl_n rl_n (actor id_n q_n pr_n any_n)) ...)
        
    ((pl_0 rl_0 (actor id_0 q_0 pr_0 any_0)) ...  
     (pl_a rl_a (actor id_a  q_a pr_a (in-hole E (ref id_new))))
     (() () (actor id_new () ()   e))
     (pl_n rl_n (actor id_n q_n pr_n any_n)) ...)
    (fresh id_new)
    "new-actor"]


   [-->
    ((pl_0 rl_0 (actor id_0 q_0 pr_0 any_0)) ...
     (pl_1 rl_1 (actor id_1 q_1 pr_1 (in-hole E (send (ref id_3) e_msg))))   
     (pl_2 rl_2 (actor id_2 q_2 pr_2  any_2)) ... 
     (pl_3 rl_3 (actor id_3 q_3 pr_3  any_3))
     (pl_n rl_n (actor id_n q_n  pr_n  any_n)) ...)

    ((pl_0 rl_0 (actor id_0 q_0 pr_0 any_0)) ...
     (pl_1 rl_1 (actor id_1 q_1 pr_1 ()))    
     (pl_2 rl_2 (actor id_2 q_2 pr_2  any_2)) ... 
     (pl_3 rl_3 (actor id_3 (ADD-MESSAGE q_3 ((x_tstamp e_msg))) pr_3  any_3))
     (pl_n rl_n (actor id_n q_n  pr_n  any_n)) ...)
    (fresh id_msg)
    (fresh x_tstamp)
    "send-message"]

   ))

(define NEST-E-Reductions
  (extend-reduction-relation 
   NEST-Reductions
   NEST-R             ;; language
   #:domain k       ;; specifies a contract

   [ -->
     (in-hole K  (pl_a rl_a (actor id_a (m_a ... (x_tstamp e_msg)) pr_a any_a)))
     (PROCESS-MESSAGE id_a (m_a ... (x_tstamp e_msg)) pl_a rl_a pr_a any_a)
     "process-message"]
   ))

(define NEST-T-Reductions
  (extend-reduction-relation 
   NEST-Reductions
   NEST-T             ;; language
   #:domain k       ;; specifies a contract

   [ -->
     (in-hole K  (pl_a rl_a (actor id_a (m_a ... (v_tstamp e_msg)) pr_a any_a)))
     (PROCESS-RANDOM-MESSAGE id_a (m_a ... (v_tstamp e_msg)) pl_a rl_a pr_a any_a)
     "process-random-message"]
   ))