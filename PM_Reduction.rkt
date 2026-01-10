#lang racket

(require redex)
(require "PM_Language.rkt")


(define PM
  (reduction-relation
   ParsingMachineLanguage
   #:domain (R Program Input natural natural Stack SPR M)
   
    ;; Char instruction - success case
     (--> (suc ((I_1 ...) ((Char natural) I_2 ...)) 
          ((natural_1 ...) (natural natural_2 ...)) 
          natural_pc 
          natural_i 
          Stack
          SPR
          M)  

          (suc ((I_1 ... (Char natural)) (I_2 ...)) 
          ((natural_1 ... natural) (natural_2 ...)) 
          ,(+ (term natural_pc) 1) 
          ,(+ (term natural_i) 1) 
          Stack
          SPR
          M) 
     "char-match")

     ;; Char instruction - fail case
     (--> (suc ((I_1 ...) ((Char (name var1 natural_!_)) I_2 ...)) 
          ((natural_1 ...) ((name var2 natural_!_) natural_2 ...)) 
          natural_pc 
          natural_i 
          Stack
          SPR
          M)    

          (fail ((I_1 ...) ((Char var1) I_2 ...)) 
          ((natural_1 ...) (var2 natural_2 ...)) 
          natural_pc 
          natural_i 
          Stack
          SPR
          M) 
     "char-fail")

     ;; Char instruction - fail empty case
     (--> (suc ((I_1 ...) ((Char natural) I_2 ...)) 
          ((natural_1 ...) ( )) 
          natural_pc 
          natural_i 
          Stack
          SPR
          M) 

          (fail ((I_1 ...) ((Char natural) I_2 ...)) 
          ((natural_1 ... ) ( )) 
          natural_pc 
          natural_i 
          Stack
          SPR
          M) 
     "char-fail-empty")

     ;; Any instruction - success case
     (--> (suc ((I_1 ...) (Any I_2 ...)) ((natural_1 ...) (natural_2 natural_3 ...)) natural_pc natural_i Stack SPR M)
          (suc ((I_1 ... Any) (I_2 ...)) ((natural_1 ... natural_2) (natural_3 ...)) ,(+ (term natural_pc) 1) ,(+ (term natural_i) 1) Stack SPR M)
     "any-match")

     ;; Any instruction - fail case
     (--> (suc ((I_1 ...) (Any I_2 ...)) ((natural_1 ...) ( )) natural_pc natural_i Stack SPR M)
          (fail ((I_1 ...) (Any I_2 ...)) ((natural_1 ... ) ( )) natural_pc natural_i Stack SPR M)
     "any-fail-empty")

     ;; Choice instruction
     (--> (suc ((I_1 ...) ((Choice integer) I_2 ...)) Input natural_pc natural_i (StackEntry ...) SPR M)
          (suc
           ((I_1 ... (Choice integer)) (I_2 ...))
           Input
           ,(+ (term natural_pc) 1)
           natural_i
           ((,(+ (term natural_pc) (term integer)) natural_i ) StackEntry ...)
           SPR
           M)
     "choice-match")

     ;; Jump instruction
     (--> (suc ((I_1 ...) ((Jump integer) I_2 ...)) Input natural_pc natural_i Stack SPR M)
          (suc (moveProgram ((I_1 ...) ((Jump integer) I_2 ...)) integer) Input ,(+ (term natural_pc) (term integer)) natural_i Stack SPR M)
     "jump-match")

     ;; Call instruction
     (--> (suc ((I_1 ...) ((Call integer) I_2 ...)) Input natural_pc natural_i (StackEntry ...) SPR M)
          (suc
           (moveProgram((I_1 ...) ((Call integer) I_2 ...)) integer)
           Input
           ,(+ (term natural_pc) (term integer))
           natural_i
           (,(+ (term natural_pc) 1) SPR StackEntry ...)
            ,(length (term M))
            M)
     "call-match")

     ;; Return instruction
     (--> (suc ((I_1 ...) ((Return natural_n) I_2 ...)) Input natural_pc0 natural_i (StackEntry ...) SPR M)
          (suc
           (moveProgram ((I_1 ...) ((Return natural_n) I_2 ...)) ,(- (term natural_pc1) (term natural_pc0)))
           Input natural_pc1 natural_i
           (StackEntry_1 ... StackEntry_3 ...)
           SPR_1
           M)
          (where ((StackEntry_1 ...) (natural_pc1 SPR_1 StackEntry_3 ...)) (splitStack natural_n (StackEntry ...)))
     "return-match")

     ;; Commit instruction
     (--> (suc ((I_1 ...) ((Commit integer) I_2 ...)) Input natural_pc natural_i (StackEntry_head StackEntry ...) SPR M)
          (suc (moveProgram ((I_1 ...) ((Commit integer) I_2 ...)) integer) Input ,(+ (term natural_pc) (term integer)) natural_i (StackEntry ...) SPR M)
     "commit-match")

     ;; Fail instruction
     (--> (suc ((I_1 ...) (Fail I_2 ...)) Input natural_pc natural_i Stack SPR M)
          (fail ((I_1 ... Fail) (I_2 ...)) Input ,(+ (term natural_pc) 1) natural_i Stack SPR M)
     "fail-instruction")

     ;; Fail
     (--> (fail Program Input natural_pc natural_i (natural StackEntry ...) SPR M)
          (fail Program Input natural_pc natural_i (StackEntry ...) SPR M)
     "fail")

     ;; Fail restore instruction
     (--> (fail Program Input natural_pc natural_i ((natural_newPC natural_newI) StackEntry ...) SPR M)
          (suc (moveProgram Program ,(- (term natural_newPC) (term natural_pc))) (moveInput Input ,(- (term natural_newI) (term natural_i))) natural_newPC natural_newI (StackEntry ...) SPR M)
     "fail-restore")

     ;;Load
      (--> (suc ((I_1 ...) ((Load natural) I_2 ...)) 
          Input 
          natural_pc 
          natural_i 
          (StackEntry ...)
          SPR
          M) 

          (suc ((I_1 ... (Load natural)) ( I_2 ...)) 
          Input 
          ,(+ (term natural_pc) 1) 
          natural_i 
          ((readMem ,(+ (term natural) (term SPR)) M) StackEntry ...)
          SPR
          M) 
     "load")

      ;; Store
      (--> (suc ((I_1 ...) ((Store natural) I_2 ...)) 
          Input 
          natural_pc 
          natural_i 
          (Value_1 StackEntry ...)
          SPR
          M) 

          (suc ((I_1 ... (Store natural)) ( I_2 ...)) 
          Input 
          ,(+ (term natural_pc) 1) 
          natural_i 
          (StackEntry ...)
          SPR
          (writeMem ,(+ (term natural) (term SPR)) Value_1 M)) 
     "store")

      ;; Push
      (--> (suc ((I_1 ...) ((Push Value_1) I_2 ...)) 
          Input 
          natural_pc 
          natural_i 
          (StackEntry ...)
          SPR
          M) 

          (suc ((I_1 ... (Push Value_1)) ( I_2 ...)) 
          Input 
          ,(+ (term natural_pc) 1) 
          natural_i 
          (Value_1 StackEntry ...)
          SPR
          M) 
     "push")

      ;; Pop
      (--> (suc ((I_1 ...) (Pop I_2 ...)) 
          Input 
          natural_pc 
          natural_i 
          (Value_1 StackEntry ...)
          SPR
          M) 

          (suc ((I_1 ... Pop ) ( I_2 ...)) 
          Input 
          ,(+ (term natural_pc) 1) 
          natural_i 
          (StackEntry ...)
          SPR
          M) 
     "pop")

      ;; Add
      (--> (suc ((I_1 ...) (Add I_2 ...)) 
          Input 
          natural_pc 
          natural_i 
          (natural_1 natural_2 StackEntry ...)
          SPR
          M) 

          (suc ((I_1 ... Add ) ( I_2 ...)) 
          Input 
          ,(+ (term natural_pc) 1) 
          natural_i 
          (,(+ (term natural_1) (term natural_2)) StackEntry ...)
          SPR
          M) 
     "add")

      ;; Sub
      (--> (suc ((I_1 ...) (Sub I_2 ...)) 
          Input 
          natural_pc 
          natural_i 
          (natural_1 natural_2 StackEntry ...)
          SPR
          M) 

          (suc ((I_1 ... Sub ) ( I_2 ...)) 
          Input 
          ,(+ (term natural_pc) 1) 
          natural_i 
          (,(- (term natural_2) (term natural_1)) StackEntry ...)
          SPR
          M) 
     "sub")

      ;; Mult
      (--> (suc ((I_1 ...) (Mult I_2 ...)) 
          Input 
          natural_pc 
          natural_i 
          (natural_1 natural_2 StackEntry ...)
          SPR
          M) 

          (suc ((I_1 ... Mult ) ( I_2 ...)) 
          Input 
          ,(+ (term natural_pc) 1) 
          natural_i 
          (,(* (term natural_2) (term natural_1)) StackEntry ...)
          SPR
          M) 
     "mult")

      ;; Div
      (--> (suc ((I_1 ...) (Div I_2 ...)) 
          Input 
          natural_pc 
          natural_i 
          (natural_1 natural_2 StackEntry ...)
          SPR
          M) 

          (suc ((I_1 ... Div ) ( I_2 ...)) 
          Input 
          ,(+ (term natural_pc) 1) 
          natural_i 
          (,(/ (term natural_2) (term natural_1)) StackEntry ...)
          SPR
          M) 
     "div")

      ;; And
      (--> (suc ((I_1 ...) (And I_2 ...)) 
          Input 
          natural_pc 
          natural_i 
          (boolean_1  boolean_2 StackEntry ...)
          SPR
          M) 

          (suc ((I_1 ... And) ( I_2 ...)) 
          Input 
          ,(+ (term natural_pc) 1) 
          natural_i 
          (,(and  (term  boolean_1) (term  boolean_2)) StackEntry ...)
          SPR
          M) 
     "and")

      ;; Or
      (--> (suc ((I_1 ...) (Or I_2 ...)) 
          Input 
          natural_pc 
          natural_i 
          (boolean_1  boolean_2 StackEntry ...)
          SPR
          M) 

          (suc ((I_1 ... Or) ( I_2 ...)) 
          Input 
          ,(+ (term natural_pc) 1) 
          natural_i 
          (,(or  (term  boolean_1) (term  boolean_2)) StackEntry ...)
          SPR
          M) 
     "or")

      ;; Not
      (--> (suc ((I_1 ...) (Not I_2 ...)) 
          Input 
          natural_pc 
          natural_i 
          (boolean_1 StackEntry ...)
          SPR
          M) 

          (suc ((I_1 ... Not) ( I_2 ...)) 
          Input 
          ,(+ (term natural_pc) 1) 
          natural_i 
          (,(not (term boolean_1)) StackEntry ...)
          SPR
          M) 
     "not")

       ;; Eq Bool
      (--> (suc ((I_1 ...) (Eq I_2 ...)) 
          Input 
          natural_pc 
          natural_i 
          (boolean_1 boolean_2 StackEntry ...)
          SPR
          M) 

          (suc ((I_1 ... Eq) ( I_2 ...)) 
          Input 
          ,(+ (term natural_pc) 1) 
          natural_i 
          (,(equal? (term boolean_1) (term boolean_2)) StackEntry ...)
          SPR
          M) 
     "eq-bool")

      ;; Eq Natural
      (--> (suc ((I_1 ...) (Eq I_2 ...)) 
          Input 
          natural_pc 
          natural_i 
          (natural_1 natural_2 StackEntry ...)
          SPR
          M) 

          (suc ((I_1 ... Eq) ( I_2 ...)) 
          Input 
          ,(+ (term natural_pc) 1) 
          natural_i 
          (,(equal? (term natural_1) (term natural_2)) StackEntry ...)
          SPR
          M) 
     "eq-nat")

       ;; Eq List
      (--> (suc ((I_1 ...) (Eq I_2 ...)) 
          Input 
          natural_pc 
          natural_i 
          (List_1 List_2 StackEntry ...)
          SPR
          M) 

          (suc ((I_1 ... Eq) ( I_2 ...)) 
          Input 
          ,(+ (term natural_pc) 1) 
          natural_i 
          (,(equal? (term List_1) (term List_2)) StackEntry ...)
          SPR
          M) 
     "eq-list")

      ;; Lt
      (--> (suc ((I_1 ...) (Lt I_2 ...)) 
          Input 
          natural_pc 
          natural_i 
          (natural_1 natural_2 StackEntry ...)
          SPR
          M) 

          (suc ((I_1 ... Lt) ( I_2 ...)) 
          Input 
          ,(+ (term natural_pc) 1) 
          natural_i 
          (,(< (term natural_2) (term natural_1)) StackEntry ...)
          SPR
          M) 
     "lt")

      ;;Head
      (--> (suc ((I_1 ...) (Head I_2 ...)) 
          Input 
          natural_pc 
          natural_i 
          ((cons Value_1 List_1) StackEntry ...)
          SPR
          M) 

          (suc ((I_1 ... Head) ( I_2 ...)) 
          Input 
          ,(+ (term natural_pc) 1) 
          natural_i 
          (Value_1 StackEntry ...)
          SPR
          M) 
     "head")

      ;;Tail
      (--> (suc ((I_1 ...) (Tail I_2 ...)) 
          Input 
          natural_pc 
          natural_i 
          ((cons Value_1 List_1) StackEntry ...)
          SPR
          M) 

          (suc ((I_1 ... Tail) ( I_2 ...)) 
          Input 
          ,(+ (term natural_pc) 1) 
          natural_i 
          (List_1 StackEntry ...)
          SPR
          M) 
     "tail")

      ;;Cons
      (--> (suc ((I_1 ...) (Cons I_2 ...)) 
          Input 
          natural_pc 
          natural_i 
          (Value_1 List StackEntry ...)
          SPR
          M) 

          (suc ((I_1 ... Cons) ( I_2 ...)) 
          Input 
          ,(+ (term natural_pc) 1) 
          natural_i 
          ((cons Value_1 List) StackEntry ...)
          SPR
          M) 
     "cons")

      ;;Concat
      (--> (suc ((I_1 ...) (Concat I_2 ...)) 
                Input 
                natural_pc 
                natural_i 
                (List_1 List_2 StackEntry ...)
                SPR
                M) 
           (suc ((I_1 ... Concat) (I_2 ...)) 
                Input 
                ,(+ (term natural_pc) 1) 
                natural_i 
                ((concat List_1 List_2) StackEntry ...)
                SPR
                M) 
           "concat")

      ;;LGet
      (--> (suc ((I_1 ...) (LGet I_2 ...)) 
                Input 
                natural_pc 
                natural_i 
                (natural List_1 StackEntry ...)
                SPR
                M) 
           (suc ((I_1 ... LGet) (I_2 ...)) 
                Input 
                ,(+ (term natural_pc) 1) 
                natural_i 
                ((readList natural List_1) StackEntry ...)
                SPR
                M) 
           "lget")

      ;;Assert
      (--> (suc ((I_1 ...) (Assert I_2 ...)) 
                Input 
                natural_pc 
                natural_i 
                (#t StackEntry ...)
                SPR
                M) 
           (suc ((I_1 ... Assert) (I_2 ...)) 
                Input 
                ,(+ (term natural_pc) 1) 
                natural_i 
                (StackEntry ...)
                SPR
                M) 
           "assert-sucess")

      ;;Assert
      (--> (suc ((I_1 ...) (Assert I_2 ...)) 
                Input 
                natural_pc 
                natural_i 
                (#f StackEntry ...)
                SPR
                M)

           (fail ((I_1 ...) (Assert I_2 ...)) 
                 Input 
                 natural_pc 
                 natural_i 
                 (StackEntry ...)
                 SPR
                 M) 
           "assert-fail")

      ;;Halt
      (--> (suc ((I_1 ...) (Halt I_2 ...)) 
                Input 
                natural_pc 
                natural_i 
                Stack
                SPR
                M)

           (suc ((I_1 ...) (Halt I_2 ...)) 
                Input 
                natural_pc 
                natural_i 
                Stack
                SPR
                M) 
           "halt")

 )
)

(define-metafunction ParsingMachineLanguage
     moveProgram : Program integer -> Program
     [(moveProgram Program 0) 
      Program]
     [(moveProgram ((I ...) ()) integer) 
      ((I ...) ()) (side-condition 
      (> (term integer) 0))]
     [(moveProgram ((I_1 ...) (I_2 I_3 ...)) integer) 
      (moveProgram((I_1 ... I_2) (I_3 ...)) ,(- (term integer) 1)) 
      (side-condition (> (term integer) 0))]
     [(moveProgram (() (I ...)) integer) 
      (() (I ...))]
     [(moveProgram ((I_1 ... I_2) (I_3 ...)) integer) 
      (moveProgram((I_1 ...) (I_2 I_3 ...)) ,(+ (term integer) 1))]
) 

(define-metafunction ParsingMachineLanguage
     moveInput : Input integer -> Input
     [(moveInput Input 0) Input]
     [(moveInput ((natural ...) ()) integer) ((natural ...) ()) (side-condition (> (term integer) 0))]
     [(moveInput ((natural_1 ...) (natural_2 natural_3 ...)) integer) (moveInput((natural_1 ... natural_2) (natural_3 ...)) ,(-  (term integer) 1)) (side-condition (> (term integer) 0))]
     [(moveInput (() (natural ...)) integer) (() (natural ...))]
     [(moveInput ((natural_1 ... natural_2) (natural_3 ...)) integer) (moveInput((natural_1 ...) (natural_2 natural_3 ...)) ,(+ (term integer) 1))]
)

(define-metafunction ParsingMachineLanguage
  readMem : natural M -> Value
  [(readMem 0 (Value_1 Value ...)) Value_1]
  [(readMem natural (Value_1 Value ...)) (readMem ,(- (term natural) 1) (Value ...))])

(define-metafunction ParsingMachineLanguage
  writeMem : natural Value M -> M
  [(writeMem 0 Value_1 (Value_2 Value ...)) (Value_1 Value ...)]
  [(writeMem 0 Value_1 ()) (Value_1)]
  [(writeMem natural Value_1 ()) (ins 0 (writeMem ,(- (term natural) 1) Value_1 ()))]
  [(writeMem natural Value_2 (Value_1 Value ...)) (ins Value_1 (writeMem ,(- (term natural) 1) Value_2 (Value ...)))])

(define-metafunction ParsingMachineLanguage
  readList : natural List -> Value
  [(readList 0 (cons Value_1 List)) Value_1]
  [(readList natural (cons Value_1 List)) (readList ,(- (term natural) 1) List)])

(define-metafunction ParsingMachineLanguage
  ins : Value (Value ...) -> (Value ...)
  [(ins Value_1 (Value ...)) (Value_1 Value ... )])

(define-metafunction ParsingMachineLanguage
  concat : List List -> List
  [(concat nill nill) nill]
  [(concat nill List) List]
  [(concat List nill) List]
  [(concat (cons Value_1 List_1) List_2) (cons Value_1 (concat List_1 List_2))])

(define-metafunction ParsingMachineLanguage
  splitStack : natural Stack -> (Stack Stack)
  [(splitStack 0 Stack) (() Stack)]
  [(splitStack natural ()) (() ())]
  [(splitStack natural (StackEntry_1 StackEntry ...)) ((StackEntry_1 StackEntry_3 ...) (StackEntry_4 ...))
                                                      (where ((StackEntry_3 ...) (StackEntry_4 ...))
                                                             (splitStack ,(- (term natural) 1) (StackEntry ...) ))])

(define (exemplo01 )
  (traces PM (term (suc
                    
                   (()
                    
                    (
                     (Call 12)
                     Halt
                     (Choice 5)
                     (Char 0)
                     (Push 0)
                     (Store 0)
                     (Commit 4)
                     (Char 1)
                     (Push 1)
                     (Store 0)
                     (Load 0)
                     (Return 1)
                     (Call -10)
                     (Store 1)
                     (Choice 10)
                     (Call -13)
                     (Store 2)
                     (Load 1)
                     (Push 2)
                     Mult
                     (Load 2)
                     Add
                     (Store 1)
                     (Commit -9)
                     (Load 1)
                     (Return 1)
                     )
                    
                    )

                   
                   (() 
                    (1 0 1 1 1))
                   
                   1
                   
                   0
                   
                   ()
                   0
                   ()
                  ))))


#;(firstJumpMove  (term (I_1 ...)) ((Jump natural) (term (I_2 ...))) (term natural))

#;(apply-reduction-relation PM (term (
                       suc
                        ()    
                        ((Char 97) (Char 99))    
                        () 
                        (97 98 99)          
                        0              
                        0              
                        ()))) 

#;(stepper PM (term (   suc
                        ()    
                        ((Char 97) (Char 99))    
                        () 
                        (97 98 99)          
                        0              
                        0              
                        ()))) 

#;(traces PM (term (      suc
                        (()    
                        ((Char 97) (Choice 4) (Char 97) (Char 98) (Commit 2) (Char 98)))  
                        (() 
                        (97 98))          
                        1              
                        0              
                        ()
                        ())))

#;(traces PM (term (suc
                   (()    
                    (LGet))  
                   (() 
                    (97 98))          
                   1              
                   0              
                   (1 (cons 1 (cons 2 (cons 3 nill)))) 
                   0
                   ()
                  )))
#;(traces PM (term (suc
                   (()    
                    ((Store 0)))  
                   (() 
                    (97 98))          
                   1              
                   0              
                   (128) 
                   5
                   (0 1 2 3)
                  )))
#;(traces PM (term (suc
                   (()    
                    ((Load 0)))  
                   (() 
                    (97 98))          
                   1              
                   0              
                   (256) 
                   5
                   (0 1 2 3 0 127)
                  )))

#;(traces PM (term (suc
                   (()    
                    ((Push 0)
                     (Store 0)
                     (Char 98)
                     (Push 1)
                     (Store 1)
                     (Char 99)
                     (Push 2)
                     (Store 2)
                     (Call 2)
                     (Return 0)
                     (Push 5)
                     (Store 0)
                     (Char 101)
                     (Push 6)
                     (Store 1)
                     (Char 102)
                     (Push 7)
                     (Store 2)
                     (Return 0)))  
                   (() (98 99 101 102))          
                   1              
                   0              
                   () 
                   0
                   ()
                  )))

(traces PM (term (suc
                   (()    
                    ((Call 2)
                     Halt
                     (Choice 9)
                     (Char 98)
                     (Call -2)
                     (Store 1)
                     (Push 1)
                     (Load 1)
                     Add
                     (Store 0)
                     (Commit 3)
                     (Push 0)
                     (Store 0)
                     (Load 0)
                     (Return 1)))  
                   (() (98 98 98 98 98))          
                   1              
                   0              
                   () 
                   0
                   ()
                  )))
#;(term (moveProgram (() ((Char 97) (Char 98) (Char 97) (Char 97) Any)) 7))
#;(term (moveProgram (() ((Char 97) (Char 98) (Char 97) (Char 97) Any)) 0))
#;(term (moveProgram (((Char 97) (Char 98) (Char 97)) ((Char 97) Any)) -2))

