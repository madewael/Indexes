#lang racket

(provide tagged-list?
         
         if?
         if-exp->condition
         if-exp->consequent
         if-exp->alternative
         
         var-definition?
         definition-exp->variable
         definition-exp->value-exp
         
         proc-definition?
         definition-exp->procedure-name
         definition-exp->arg-list
         definition-exp->body
         proc-definition->var-definition
         
         lambda?
         lambda-exp->params
         lambda-exp->body
         
         let?
         let-exp->var-val-pairs
         let-exp->vars
         let-exp->value-exps
         let-exp->body
         
         begin?
         begin->exps
         exps->exp-sequence
         
         application?
         application-exp->procedure-exp
         application-exp->arg-exps
         
         for?
         for-exp->clauses
         for-clause->var
         for-clause->iteratable
         for-exp->body)


(define (tagged-list? tag lst)
  (and (pair? lst)(eq? (car lst) tag)))


(define definition?  (curry tagged-list? 'define))


;(if condition consequent alternative)
(define if? (curry tagged-list? 'if))
(define if-exp->condition   cadr)
(define if-exp->consequent  caddr)
(define if-exp->alternative cadddr)



;(define variable value-exp)
(define (var-definition? exp)(and (definition? exp)
                                  (symbol? (cadr exp))))
(define definition-exp->variable cadr)
(define definition-exp->value-exp caddr)


;(define (function-name . arg-list) body ...)
(define (proc-definition? exp)(and (definition? exp)
                                   (pair? (cadr exp))))
(define definition-exp->procedure-name caadr)
(define definition-exp->arg-list       cdadr)
(define definition-exp->body           cddr)
(define (proc-definition->var-definition definition-exp)
  (let ((procedure-name (definition-exp->procedure-name definition-exp))
        (arg-list       (definition-exp->arg-list       definition-exp))
        (body           (definition-exp->body           definition-exp)))
    `(define ,procedure-name (lambda ,arg-list ,@body))))

;(lambda params body)
(define (lambda? exp) (or (tagged-list? 'lambda exp)
                          (tagged-list? 'λ exp)))
(define lambda-exp->params cadr)
(define lambda-exp->body   cddr)

;(let ((v1 e1)(v2 e2)...) body...)
(define let? (curry tagged-list? 'let))
(define let-exp->var-val-pairs cadr)
(define var-val-pair->var      car)
(define var-val-pair->val      cadr)
(define let-exp->vars          (compose (curry map var-val-pair->var)  let-exp->var-val-pairs))
(define let-exp->value-exps    (compose (curry map var-val-pair->val)  let-exp->var-val-pairs))
(define let-exp->body          cddr)


;(begin exps)
(define begin? (curry tagged-list? 'begin))
(define begin->exps cdr)

(define (exps->exp-sequence exps)
  (if (null? (cdr exps))
      (car exps)
      `(begin ,@exps)))


;(procedure-name . argument-expressions)
(define application? pair?)
(define application-exp->procedure-exp car)
(define application-exp->arg-exps      cdr)

;(for ((var iteratable) ...) body ...)
(define for? (curry tagged-list? 'for))
(define for-exp->clauses cadr)
(define for-clause->var car)
(define for-clause->iteratable cadr)
(define for-exp->body cddr)

