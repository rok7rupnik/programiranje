#lang racket
; strams


(define my-number-stream
    (letrec ([f (lambda (x)
        (cons (if (= (remainder x 3) 0)
                (- x)
                x)
            (lambda ()
                (f (+ x 1)))))])
        (lambda () (f 1))))

(define (first-n generator n)
    (if (> n 1)
        (let ([stream (generator)])
            (begin (displayln (car stream))
                (first-n (cdr stream) (- n 1))))
        (displayln (car (generator)))))

; (first-n my-number-stream 7)

(define (generiraj-tok func arg)
    (letrec ([f (lambda (x)
        (cons x 
            (lambda ()
                (f (func x arg)))))])
        (lambda () (f arg))))

(define natural-numbers (generiraj-tok + 1))
(define powers-of-two (generiraj-tok * 2))

(define (generiraj-dokler generator tester)
    (let ([stream (generator)])
        (if (tester (car stream))
            null
            (cons (car stream)
                (generiraj-dokler (cdr stream) tester)))))

(define (list-mth-mod lst m)
    (cond 
        [(< m 1) (error "Number is not positive")]
        [(null? list) (error "List is empty")]
        [#t (car (list-tail lst (remainder m (length lst))))]))

(define (krozi-po-seznamih lst1 lst2)
    (letrec ([f (lambda (n) 
                (cons (cons (list-mth-mod lst1 n) (list-mth-mod lst2 n))
                    (lambda () (f (+ n 1)))))])
            (lambda () (f 0))))

; macros

; (define-sytnax name
;     (sytnax-rules (key words)
;         [(pattern)
;         (expression)]))

(define-syntax compute
    (syntax-rules ()
        [(compute arg1 op arg2) 
         (op arg1 arg2)]))

; (compute 3 + 2)

(define-syntax for 
    (syntax-rules (to do)
        [(for lower to higher do body)
         (loop lower higher body)]
        ))

(define (loop l h body)
    (if (< l h)
        (begin
            (body)
            (loop (+ l 1) h body))
        (body)))

; (for 3 to 5 do (lambda () (display "hello")))