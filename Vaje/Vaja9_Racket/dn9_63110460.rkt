#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Odstrani ponovljene ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;odstrani zaporedoma ponovljene
(define (odstrani-ponovljene sez)
  (cond [(null? sez) null]
        [(null? (cdr sez)) sez]
        [#t (if (equal? (car sez) (car (cdr sez)))
                (odstrani-ponovljene (cdr sez))
                (cons (car sez) (odstrani-ponovljene (cdr sez))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Pascal ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (pascalov-trikotnik n)
  (define (naslednja vrstica)
    (cond [(null? vrstica) (list 1)]
          [(null? (cdr vrstica)) (list 1 1)]
          [#t (let* ([nova (naslednja (cdr vrstica))])
                (append (list (car nova) (+ (car vrstica) (cadr vrstica))) (cdr nova)))]))
  (define (trikotnik i trenutna-vrstica acc)
    (cond [(equal? i 0) acc]
          [#t (let* ([naslednja-vrstica (naslednja trenutna-vrstica)])
                (trikotnik (- i 1) naslednja-vrstica (append acc (list naslednja-vrstica))))]))
  (trikotnik n null null))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; QUEUE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (queue) 
  (cons null (cons null null)))

(define (empty-queue? q)
       (and (null? (car q)) (null? (car (cdr q)))))

(define (enqueue x q)
  (cons (cons x (car q)) (cdr q)))

(define (dequeue q)
  (cond [(empty-queue? q) (cons void q)]    ;(list null (reverse(first q)))
        [(null? (car (cdr q))) (dequeue (cons null (cons (reverse (car q)) null)))]
        [#t (cons (car (car (cdr q))) (cons (car q) (cons (cdr (cdr q))) null))]))
                  ;=caadr q ... cdadr q

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Palindromi ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (generiraj-palindrome n lst)
  (define (naslednja-dolzina dolzina-2 stevke)
    (cond [(null? (car dolzina-2)) (map (lambda (x)
                                    (list x x))
                                  stevke)]
          [(null? stevke) null]
          [#t (append (map (lambda (x)
                             (append (list (car stevke)) x (list (car stevke))))
                           dolzina-2)
                      (naslednja-dolzina dolzina-2 (cdr stevke)))]))
  (define (palindromi i delovna-vrstica acc)
    (cond [(> i n) acc]
          [(and (null? delovna-vrstica) (equal? i 0)) (palindromi (+ i 1) null (list null))]
          [(and (null? delovna-vrstica) (equal? i 1)) (let* ([naslednja (map (lambda (x)
                                                                                     (list x))
                                                                                  lst)])
                                           (palindromi (+ i 1) naslednja (append acc naslednja)))]
          [(and (null? (cdar delovna-vrstica)) (equal? i 2)) (let * ([naslednja (map (lambda (x)
                                                                                       (list x x))
                                                                                     lst)])
                                           (palindromi (+ i 1) delovna-vrstica (append acc naslednja)))]
          [#t (let* ([naslednja (naslednja-dolzina delovna-vrstica lst)])
                (palindromi (+ i 1) naslednja (append acc naslednja)))]))
  (palindromi 0 null null))