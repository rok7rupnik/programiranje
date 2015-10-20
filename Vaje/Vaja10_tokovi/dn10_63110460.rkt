#lang racket
; TOKOVI
; tok je generator naslednjega para: (vresdnost, funkcija oz. generator naslednjika)
(define moj-generator-stevil
  (letrec ([f (lambda (x)
                (cons (if (= (remainder x 3) 0)
                          (- x)
                          x)
                      (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define (prvihn gen n)
  (if (> n 1)
      (let ([tok (gen)])
        (begin (displayln (car tok))
               (prvihn (cdr tok) (- n 1))))
      (displayln (car (gen)))))

;(prvihn moj-generator-stevil 7)

(define (generiraj-tok fn arg)
  (letrec ([f (lambda (x)
                (cons x
                      (lambda () (f (fn x arg)))))])
  (lambda () (f arg))))

(define naravna-stevila (generiraj-tok + 1))

(define potence-stevila-dva (generiraj-tok * 2))

;(prvihn potence-stevila-dva 7)

(define (generiraj-dokler gen test)
  (let ([tok (gen)])
    (if (test (car tok))
        null
        (cons (car tok) (generiraj-dokler (cdr tok) test)))))

;(generiraj-dokler naravna-stevila (lambda (x) (> x 5)))

(define (sez-po-modulu sez n)
  (cond [(null? sez) (error "prazen seznam")]
        [(< n 0) (error "negativen n")]
        [#t (car (list-tail sez (remainder n (length sez))))]))

(define (krozi-po-seznamih xs ys)
  (letrec ([f (lambda (x)
                (cons (cons (sez-po-modulu xs x)
                            (sez-po-modulu ys x))
                      (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))

(define (stevilo-preden gen test)
  (letrec ([f (lambda (g i)
                (let ([tok (g)])
                  (if ((lambda () test))
                      i
                      (f (cdr tok)))))])
    (f gen 0)))

;(prvihn (krozi-po-seznamih (list 1 2 3) (list "a" "b")) 5)

; MAKROJI
(define-syntax ime
  (syntax-rules (kljucne besede)
    [(vzorec)
     (izraz)]))

(define-syntax izracunaj
  (syntax-rules ()
    [(izracunaj arg1 op arg2)
     (op arg1 arg2)]))

;(izracunaj 3 + 2)

(define-syntax for 
  (syntax-rules (to do)
    [(for low to high do body)
     (let ([l low]
           [h high])
       ((letrec ([loop (lambda (i)
                         (if (> i h)
                             #t
                             (begin body
                                    (loop (+ i 1)))))])
          (loop l))))]))

;(for  3 to 5 do (display "hello"))

(define-syntax while
  (syntax-rules (do)
    [(while test do body)
     (letrec ([loop (lambda (t)
                      (if (t)
                          (begin body
                                 (loop t))
                          #t))])
       (loop test))]))

;(define stevec 0)
;(while (lambda () (< stevec 5)) do (set! stevec (+ stevec 1)))
;(displayln stevec)

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([test ((lambda () e1))])
       (letrec ([loop (lambda ()
                        (if (> test ((lambda () e2)))
                            (loop)
                            #t))])
         (loop)))]))

;(define a 2) 
;(while-less (+ 5 1) do (begin (set! a (+ a 1)) (print "x") a))

