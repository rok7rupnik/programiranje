#lang racket
(define (prvihn gen n)
  (if (> n 1)
      (let ([tok (gen)])
        (begin (displayln (car tok))
               (prvihn (cdr tok) (- n 1))))
      (displayln (car (gen)))))

;(prvihn moj-generator-stevil 7)

(define (tok-pascalovega-trikotnika)
  (define (nasl-vrstica prejsnja)
    (cond [(null? prejsnja) null]
          [(null? (cdr prejsnja)) (list 1)]
          [#t (cons (+ (first prejsnja) (second prejsnja)) (nasl-vrstica (cdr prejsnja)))]))
  (define (nasl-cela-vrstica prejsnja)
    (cons 1 (nasl-vrstica prejsnja)))
  (define (prideluj-trikotnik prejsnja)
    (let ([naslednja (nasl-cela-vrstica prejsnja)])
      (cons naslednja (lambda () (prideluj-trikotnik naslednja)))))
  (lambda () (prideluj-trikotnik null)))

;(prvihn (tok-pascalovega-trikotnika) 5)

(define (stevilo-nacinov n)
  (define (racunaj i)
    (cond [(>= i n) 1]
          [#t (+ (racunaj (+ i 1)) (stevilo-nacinov (- n i)))]))
    (cond [(< n 0) 0]
          [(= n 0) 1]
          [#t (racunaj 1)]))

(define (stevilo-nacinov-memo x)
  (letrec ([resitve null]
    [f (lambda (n)
      (define (racunaj i)
        (cond [(= i n) 1]
              [#t (+ (racunaj (+ i 1)) (f (- n i)))]))
      (let ([odgovor (assoc n resitve)])
        (if odgovor
            (cdr odgovor)
            (let ([novi-odg (cond [(<= n 0) 0]
                                  [#t (racunaj 1)])])
              (set! resitve (cons (cons n novi-odg) resitve))
              novi-odg))))])
    (f x)))

;(stevilo-nacinov-memo 2)

(define (vector-assoc v vec)
  (letrec ([isci (lambda (n)
                   (cond [(= n (vector-length vec)) #f]
                         [(pair? (vector-ref vec n))
                           (if (= (car (vector-ref vec n)) v)
                               (vector-ref vec n)
                               (isci (+ n 1)))]
                         [#t (isci (+ n 1))]))])
    (isci 0)))

;(vector-assoc 5 (vector (list 1 5 1) null (cons 4 1) (cons 5 1)))

(define (cached-assoc sez n)
  (letrec ([shramba (make-vector n #f)]
           [rridx 0]
           [assoc-vrni (lambda (x)
                         (let ([odgovor (vector-assoc x shramba)])
                           (if odgovor 
                               odgovor
                               (let ([novi-odg (assoc x sez)])
                                 (begin
                                   (vector-set! shramba rridx novi-odg)
                                   (set! rridx (remainder (+ rridx 1) n))
                                   novi-odg)))))]) assoc-vrni))

;((cached-assoc (list (cons 1 2) (cons 3 4)) 3) 3)

;(define (odstej a b)
;  (sestej a (negacija b)))
;(define (sestej-ali-odstej p a b)
;  (ce-potem-sicer p 
;                  (sestej a b) 
;                  (odstej a b)))
;(define (nobeden a b)
;  (ce-potem-sicer a
;                  (bool #f)
;                  (ce-potem-sicer b (bool #f) (bool #t))))
          