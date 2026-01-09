#lang racket

(require redex)

(provide (all-defined-out))

; Definição da Linguagem da Parsing Machine
(define-language ParsingMachineLanguage
    ;; Instructions
    [I ::= (Char natural)
           Any
           (Choice integer)
           (Jump integer)
           (Call integer) 
           (Return natural) 
           (Commit integer)
           Fail
           (Load natural) 
           (Store natural)
           (Push Value)
           Pop 
           Add 
           Sub
           Mult
           Div
           And 
           Or
           Not
           Eq
           Lt
           Head ;; !!
           Tail ;; !!
           Cons ;; !!
           Concat ;; !!
           LGet ;; !!
           Assert
           Halt]  
         
    ;; Program
    [Program ::= ((I ...) (I ...))]  

    [Input ::= ((natural ...) (natural ...))]

    [Value ::= natural
               boolean
               List] 
  
    [List ::= nill
              (cons Value List)]

    ;; Stack definition
    [Stack ::= (StackEntry ...)]             
    [StackEntry ::= natural              
                    (natural natural)
                    (StackEntry ...) 
                    Value
                    M]
 
    ;; Memory
    [M ::= (Value ...)]

    [R ::= suc
           fail])    