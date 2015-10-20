; Izpiši naslov posameznega odseka testov
(define (info arg)
  (displayln (~a "\n==== " arg " ====")))

; Funkcije/makri za poganjanje testov
(define (err-handler-ni-ok e) (displayln (~a "NI OK: " (exn-message e))))
(define (err-handler-ok e) (displayln (~a "OK: " (exn-message e))))

(define-syntax testenv
  (syntax-rules ()
    [(testenv izraz okolje rezultat)
     (with-handlers ([exn:fail? err-handler-ni-ok])
      (if (equal? (jais izraz okolje) rezultat)
        (displayln "OK")
        (printf "NI OK: Izraz ~v se ne izračuna v ~v~%" izraz rezultat)))]))

(define-syntax testenve
  (syntax-rules ()
    [(testenve izraz okolje)
     (with-handlers ([exn:fail? err-handler-ok])
      (begin
        (jais izraz okolje)
        (printf "NI OK: Izraz ~v vsebuje napako~%" izraz)))]))

(define (testeq  izraz rezultat) (testenv  izraz null rezultat))
(define (testeqe izraz         ) (testenve izraz null         ))
(define (test    izraz         ) (testeq   izraz izraz))
(define (teste   izraz         ) (testeqe  izraz      ))

; Testno okolje, v katerem je nekaj senčenih spremenljivk
(define env
  (list (cons "a" (konst 3))
        (cons "y" (nic))
        (cons "x" (konst 0))
        (cons "y" (konst 2))))

(info "funkcija")
(testeq (funkcija "test" null (nic))
        (ovojnica null (funkcija "test" null (nic))))
(testeq (funkcija "test" (list "x" "y") (sestej (sprem "x") (sprem "y")))
        (ovojnica null (funkcija "test"
                                 (list "x" "y")
                                 (sestej (sprem "x") (sprem "y")))))
(testenv (funkcija "test" (list "x") (sestej (sprem "x") (sprem "y")))
         env
         (ovojnica (list (cons "y" (nic)))
                   (funkcija "test"
                             (list "x")
                             (sestej (sprem "x") (sprem "y")))))
(testenv (funkcija #f (list "y") (sestej (sprem "x") (sprem "y")))
         env
         (ovojnica (list (cons "x" (konst 0)))
                   (funkcija #f
                             (list "y")
                             (sestej (sprem "x") (sprem "y")))))
(testenv (funkcija #f null
                   (naj-bo "x" (sestej (sprem "x") (konst 2))
                           (sestej (sprem "x") (konst 3))))
         env
         (ovojnica (list (cons "x" (konst 0)))
                   (funkcija #f null
                             (naj-bo "x" (sestej (sprem "x") (konst 2))
                                     (sestej (sprem "x") (konst 3))))))
(teste (funkcija #t (list "x" "y") (sestej (sprem "x") (sprem "y"))))
(teste (funkcija #f (cons "x" "y") (sestej (sprem "x") (sprem "y"))))
(teste (funkcija #f (list 1 2) (sestej (sprem "x") (sprem "y"))))

(info "skripta")
(test (skripta "test" (nic)))
(test (skripta "test" (sestej (sprem "x") (sprem "y"))))
(test (skripta #f (nic)))
(teste (skripta #t (nic)))
(teste (skripta 3 (nic)))

(info "klici funkcija")
(testeq (klici (funkcija "test" null (nic)) null) (nic))
(testenv (klici (funkcija "test" null (nic)) null) env (nic))
(testeq (klici (funkcija "test" (list "x" "y")
                         (sestej (sprem "x") (sprem "y")))
               (list (konst 1) (konst 3)))
        (konst 4))
(testeq (klici
          (funkcija
            "fakulteta"
            (list "x")
            (ce-potem-sicer
              (vecje (konst 1) (sprem "x"))
              (konst 1)
              (zmnozi
                (sprem "x")
                (klici
                  (sprem "fakulteta")
                  (list (sestej (sprem "x") (konst -1)))))))
            (list (konst 5)))
        (konst 120))

(info "klici skripta")
(testeq (klici (skripta "test" (nic)) (nic)) (nic))
(testenv (klici (skripta #f (sprem "x")) (nic))
         env
         (konst 0))
(testenv (klici (skripta "krneki"
                         (vecje (sprem "x") (konst 10)))
                (nic))
         env
         (bool false))
(testenv (klici (skripta
                  "krneki"
                  (ce-potem-sicer
                    (vecje (sprem "x") (konst 10))
                    (sprem "x")
                    (naj-bo "x" (sestej (sprem "x") (konst 1))
                            (klici (sprem "krneki") (nic)))))
             (nic))
         env
         (konst 11))
(info "makri odstej")
(testeq (odstej (konst 1) (konst 2)) (konst -1))

(info "makri manjsi")
(testeq (manjsi (konst 4) (konst 3)) (bool false))
(testeq (manjsi (konst 4) (konst 4)) (bool false))
(testeq (manjsi (konst 4) (konst 5)) (bool true))

(info "makri enak")
(testeq (enak (konst 4) (konst 3)) (bool false))
(testeq (enak (konst 4) (konst 4)) (bool true))
(testeq (enak (konst 4) (konst 5)) (bool false))

(info "makri vsebuje")
(testeq (vsebuje (interval 2 5) (interval 3 4)) (bool true))
(testeq (vsebuje (interval 2 5) (interval 2 4)) (bool true))
(testeq (vsebuje (interval 2 5) (interval 3 5)) (bool true))
(testeq (vsebuje (interval 2 5) (interval 2 5)) (bool true))
(testeq (vsebuje (interval 2 5) (interval 2 6)) (bool false))
(testeq (vsebuje (interval 2 5) (interval 1 3)) (bool false))
(testeq (vsebuje (interval 2 5) (interval 0 6)) (bool false))
(testeq (vsebuje (interval 2 5) (interval 0 1)) (bool false))
(testeq (vsebuje (interval 2 5) (interval 6 8)) (bool false))


; Testi z ucilnice
(info "testi z ucilnice")
(testeq (sestej (interval -3 7) (konst 4)) (interval 1 11))
(testeq (zmnozi (interval 3 5) (interval 2 7)) (interval 6 35))
(testeq (zmnozi (interval -3 7) (interval 4 8)) (interval -24 56))
(testeq (presek (interval 3 7) (interval 4 8)) (interval 4 7))

(testeq (naj-bo "x" (konst 1)
                (sestej (sprem "x") (konst 2)))
        (konst 3))

(testeq (naj-bo "x" (konst 1)
                (sestej (naj-bo "x" (konst 10)
                                (zmnozi (konst 2) (sprem "x")))
                        (sprem "x")))
        (konst 21))

(testeq (naj-bo "x" (konst 2)
                (naj-bo "y" (sprem "x")
                        (naj-bo "x" (konst 1)
                                (sestej (sprem "x") 
                                        (sprem "y")))))
        (konst 3))

(testeq (klici (funkcija "povecaj"
                         (list "n")
                         (sestej (sprem "n") (konst 1)))
               (list (konst 5)))
        (konst 6))

(testeq (klici (funkcija #f (list "x" "y")
                         (zmnozi (sprem "x") (sprem "y")))
               (list (konst 2) (konst 5)))
        (konst 10))

(testeq (naj-bo "f" (funkcija #f (list "x")
                              (sestej (sprem "x") (konst 1)))
                (klici (sprem "f") (list (konst 2))))
        (konst 3))

(testeq (klici (funkcija "f" (list "x" "y")
                         (sestej (sprem "x") (sprem "y")))
               (list (konst 2)
                     (klici (funkcija "g" (list "x")
                                      (sestej (sprem "x") (konst 5)))
                            (list (konst 16)))))
        (konst 23))

(testeq (naj-bo "s" (zdruzi (konst 6)
                            (zdruzi (konst 3)
                                    (zdruzi (konst -2) (nic))))
                (klici (funkcija "vsota" (list "x")
                                 (ce-potem-sicer (je-nic? (sprem "x"))
                                                 (konst 0) 
                                                 (sestej (glava (sprem "x"))
                                                         (klici (sprem "vsota")
                                                                (list (rep (sprem "x")))))))
                       (list (sprem "s"))))
        (konst 7))

(testeq (naj-bo "x" (konst 2)
                (naj-bo "y" (sprem "x")
                        (naj-bo "x" (konst 1)
                                (sestej (sprem "x") (sprem "y")))))
        (konst 3))

