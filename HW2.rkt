#lang eopl

(define empty-env
  (lambda () '()))

(define empty-env?
  (lambda (env)
    (if (null? env)
	#t
	#f)))

(define extend-env
  (lambda (var val env)
    (cons (cons var val) env)))

(define apply-env
  (lambda (env search-var)
    (cond
      ((null? env)
       (report-no-binding-found search-var))
      ((eqv? (caar env) search-var)
       (cdr (car env)))
      (else
       (apply-env (cdr env) search-var)))))

(define report-no-binding-found
  (lambda (search-var env)
    (eopl:error 'apply-env "No binding for ~s in ~s" search-var env)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment ~s" env)))


(define has-binding?
  (lambda (env s)
    (cond
      ((null? env) #f)
      ((eqv? (caar env) s) #t)
      (else (has-binding? (cdr env) s)))))


  (define e
    (extend-env 'd 6
      (extend-env 'y 8
        (extend-env 'x 7
          (extend-env 'y 14
            (empty-env))))))


;;--------------------------------------------------------------------
;; exercise 2.15

; constructors:
;var -> Lc-exp
;Var x Lc-exp -> Lc-exp
;Lc-exp x Lc-exp -> Lc-exp

;predicates:
;Lc-exp -> bool
;Lc-exp -> bool
;Lc-exp -> bool

;Extractors:
;Lc-exp->var
;Lc-exp->Var
;Lc-exp->Lc-exp
;Lc-exp->Lc-exp
;Lc-exp->Lc-exp


(define var-exp
  (lambda (var)
    (cons 'var-exp var)))

(define lambda-exp
  (lambda (var exp)
    (list(list var) exp)))

(define app-exp
  (lambda (exp1 exp2)
    (list exp1 exp2)))

(define var-exp?
  (lambda (bool)
    (and (pair? bool) (eqv? (car bool) 'var-exp))))

(define lambda-exp?
  (lambda (bool)
    (and (pair? bool) (eqv? (car bool) 'lambda-exp))))

(define app-exp?
  (lambda (bool)
    (and (list? bool) (eqv? car(bool) 'app-exp))))

(define var-exp->var
  (lambda (var)
    (cdr var)))

(define lambda-exp->bound-var
  (lambda (var)
    (car(cdr var))))

(define lambda-exp->body
  (lambda (exp)
    (car (cdr(cdr exp)))))

(define app-exp->rator
  (lambda (exp)
    (car(cdr exp))))

(define app-exp->rand
  (lambda (exp)
    (car(cdr exp))))


    


;occurs-free? from the book
(define occurs-free?
  (lambda (search-var exp)
    (cond
     ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
     ((lambda-exp? exp)
      (and
       (not (eqv? search-var (lambda-exp->bound-var exp)))
       (occurs-free? search-var (lambda-exp->body exp))))
     (else
      (or
       (occurs-free? search-var (app-exp->rator exp))
       (occurs-free? search-var (app-exp->rand exp)))))))