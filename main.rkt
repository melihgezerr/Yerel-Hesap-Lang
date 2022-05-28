; melih gezer
; 2020400156
; compiling: yes
; complete: yes

#lang racket
(provide (all-defined-out))

; 10 points
(define := (lambda (var value) (cons var (cons value '()))))

; 10 points
(define -- (lambda (x . y) (cons 'let (list (cons x y)))))

; 10 points
(define @ (lambda (bindings expr) (append bindings expr)))

; 20 points
;(define split_at_delim (lambda (delim args) 0))

(define split_at_delim (lambda (delim args) (foldr 
  (lambda (first after) 
    (if (equal? first delim)
        (cons '() after)
        (cons (cons first (car after)) (cdr after))))
    (list '()) args)))

; 30 points

(define (lastelem list) (car (reverse list)))

(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define (strip lst)
  (if (or (null? lst) (atom? lst) (not (null? (cdr lst))))
      lst
      (strip (car lst))))

(define (flatten lst)
  (cond ((or (null? lst) (atom? lst))
         lst)
        ((null? (strip (car lst)))
         (flatten (cdr lst)))
        (else
         (cons (flatten (strip (car lst))) (flatten (cdr lst))))))


(define rmv (lambda (lst) (if (atom? lst)
  lst
  (if (and (= 1 (length lst)) (atom? (car lst))) lst 
  (if (and (= 1 (length lst)) (pair? (car lst)))
    (rmv (car lst))
    lst)))))


(define (isallatom expr)
  (if (or (null? expr) (atom? expr))
    #t
    (if (pair? (car expr))
      #f
      (isallatom (cdr expr)))))


(define mapplus (lambda (x) (if (atom? x)
  x
  (if (equal? '+ (car x))
    x
    (split_at_delim '+ x)))))

(define maptimes (lambda (x) (if (atom? x)
  x
  (if (equal? '* (car x))
    x
    (split_at_delim '* x)))))

(define map@ (lambda (x) (if (atom? x)
  x
  (if (equal? '@ (car x))
    x
    (split_at_delim '@ x)))))

(define map:= (lambda (x) (if (atom? x)
  x
  (if (equal? ':= (car x))
    x
    (split_at_delim ':= x)))))

(define map-- (lambda (x) (if (atom? x)
  x
  (if (equal? '-- (car x))
    x
    (split_at_delim '-- x)))))


(define addplus (lambda (x) (if (or (atom? x) (null? x))
  x
  (if (or (equal? '+ (car x)) (equal? '* (car x)))
    x
    (if (equal? (flatten x) (rmv (flatten (mapplus x))))
      x
      (cons '+ (flatten (mapplus x)))
      )))))
  

(define addtimes (lambda (x) (if (or (atom? x) (null? x))
  x
  (if (equal? '* (car x))
    x
    (if (equal? (flatten x) (rmv (flatten (maptimes x))))
      x
      (cons '* (flatten (maptimes x)))
      )))))

(define add-- (lambda (x) (if (or (atom? x) (null? x))
  x
  (if (equal? '-- (car x))
    x
    (if (equal? (flatten x) (rmv (flatten (map-- x))))
      x
      (cons '-- (flatten (map-- x)))
      )))))

(define (mymap-- lst)
    (let ([lstt (add-- (rmv lst))]) 
    (cond [(null? lstt) '()]
        [(pair? (car lstt)) (append (list (mymap-- (car lstt))) (if (pair? (cadr lstt)) (list (mymap-- (cdr lstt))) (cdr lstt)))]
        [else (cons (car lstt) (if (and (= 1 (length(cdr lstt))) (pair? (cdr lstt)) (not (atom? (cadr lstt))))
        (list (mymap-- (cdr lstt)))
        (mymap-- (cdr lstt))
        ))])))


(define (mymapadd lst)
    (let ([lstt (addplus (rmv lst))]) 
    (cond [(null? lstt) '()]
        [(pair? (car lstt)) (append (list (mymapadd (car lstt))) (if (pair? (cadr lstt)) (list (mymapadd (cdr lstt))) (cdr lstt)))]
        [else (cons (car lstt) (if (and (= 1 (length(cdr lstt))) (pair? (cdr lstt)) (not (atom? (cadr lstt))))
        (list (mymapadd (cdr lstt)))
        (mymapadd (cdr lstt))
        ))])))
  
(define (mymaptimes lst)
    (let ([lstt (addtimes (rmv lst))]) 
    (cond [(null? lstt) '()]
        [(pair? (car lstt)) (append (list (mymaptimes (car lstt))) (if (pair? (cadr lstt)) (list (mymaptimes (cdr lstt))) (cdr lstt)))]
        [else (cons (car lstt) (if (and (= 1 (length(cdr lstt))) (pair? (cdr lstt)) (not (atom? (cadr lstt))))
        (list (mymaptimes (cdr lstt)))
        (mymaptimes (cdr lstt))
        ))])))

(define add@ (lambda (x) (if (or (atom? x) (null? x))
  x
  (if (equal? '@ (car x))
    x
    (if (equal? (flatten x) (rmv (flatten (map@ x))))
      x
      (cons '@ (flatten (map@ x)))
      )))))

(define (mymap@ lst)
    (let ([lstt (add@ (rmv lst))]) 
    (cond [(null? lstt) '()]
        [(pair? (car lstt)) (append (list (mymap@ (car lstt))) (if (pair? (cadr lstt)) (list (mymap@ (cdr lstt))) (cdr lstt)))]
        [else (cons (car lstt) (if (and (= 1 (length(cdr lstt))) (pair? (cdr lstt)) (not (atom? (cadr lstt))))
        (list (mymap@ (cdr lstt)))
        (mymap@ (cdr lstt))
        ))])))


(define add:= (lambda (x) (if (or (atom? x) (null? x))
  x
  (if (equal? ':= (car x))
    x
    (if (equal? (flatten x) (rmv (flatten (map:= x))))
      x
      (cons ':= (flatten (map:= x)))
      )))))

(define (mymap:= lst)
    (let ([lstt (add:= (rmv lst))]) 
    (cond [(null? lstt) '()]
        [(pair? (car lstt)) (append (list (mymap:= (car lstt))) (if (pair? (cadr lstt)) (list (list (mymap:= (cdr lstt)))) (cdr lstt)))]
        [else (cons (car lstt) (if (and (= 1 (length(cdr lstt))) (pair? (cdr lstt)) (not (atom? (cadr lstt))))
        (list (mymap:= (cdr lstt)))
        (mymap:= (cdr lstt))
        ))])))



(define parse_expr (lambda (expr) (wrapper_parse (if (and (atom? (car expr)) (= 1 (length expr))) (car expr) (mymaptimes (mymapadd (mymap:= (mymap-- (mymap@  expr)))))))))


(define wrapper_parse (lambda (expr)
    (cond
      [(and (list? expr) (eq? (car expr) '@)) (eval (append '(@) `(,(cadr expr)) (list (cons 'quote (list (list (wrapper_parse (caddr expr))))))))]
      [(list? expr) (map wrapper_parse expr)]
      [else expr]
      )
    )
)


; 20 points
(define eval_expr (lambda (expr) (eval (parse_expr expr))))
