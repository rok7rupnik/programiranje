#lang racket

(struct konst (int) #:transparent)                      ; konstanta; argument je število
(struct bool (b) #:transparent)                         ; b ima lahko vrednost true or false
(struct interval (od do) #:transparent)                 ; interval med stevili a in b
(struct zdruzi (e1 e2) #:transparent)                   ; par izrazov v jeziku jais
(struct nic () #:transparent)                           ; null v jeziku jais

(struct je-konst? (e) #:transparent)                    ; preverjanje za tip konst
(struct je-bool? (e) #:transparent)                     ; preverjanje za tip bool
(struct je-interval? (e) #:transparent)                 ; preverjanje za tip interval
(struct je-nic? (e) #:transparent)                      ; preverjanje za tip interval

(struct ce-potem-sicer (b e1 e2) #:transparent)         ; evalvacija izraza e1 ali e2 glede na b
(struct negacija (e) #:transparent)                     ; negacija izraza e
(struct vecje (e1 e2) #:transparent)                    ; primerjava e1 ali e2 v primeru ko sta e1 in e2 oba konstanti ali intervala
(struct glava (e) #:transparent)                        ; glava - zacetna tocka intervala ali prvi element para
(struct rep (e) #:transparent)                          ; rep - koncna tocka intervala ali drugi element para

(struct sestej (e1 e2) #:transparent)                   ; sestevanje izrazov e1 in e2 ce sta stevilski konstanti ali intervala
(struct zmnozi (e1 e2) #:transparent)                   ; mnozenje izrazov e1 in e2 ce sta stevilski konstanti ali intervala
(struct presek (e1 e2) #:transparent)                   ; mnozenje izrazov e1 in e2 ce sta stevilski konstanti ali intervala

(struct naj-bo (s e1 e2) #:transparent)                 ; konstrukt, ki omogoca shranjevanje spremenljivk
(struct sprem (s) #:transparent)                        ; konstrukt za branje spremenljivk

(struct funkcija (ime farg telo) #:transparent)         ; deklaracija funkcij
(struct skripta (ime telo) #:transparent)               ; deklaracija skript
(struct ovojnica (env f) #:transparent)                 ; shranjevanje okolja ob deklaraciji funkcij
(struct klici (e arg) #:transparent)                    ; klic funkcij in skript

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; pomozne funkcije ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (razsiri-okolje arg-keys arg-values env)
  (cond [(not (= (length arg-keys) (length arg-values)))
         (error "Razlicno stevilo imen in vrednosti argumentov")]
        [#t (append (map (lambda (k v)
                   (cons k (jais v env))) arg-keys arg-values) env)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; interpreter za jais ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (jais e env)
  (cond 
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; preverjanje tipov ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        [(je-konst?? e)
         (let ([k (je-konst?-e e)]) 
           (bool (and (konst? k) (or (number? (konst-int k)) 
                                     (error "Konstanta z nenumericno vrednostjo")))))]
        [(je-bool?? e)
         (let ([b (je-bool?-e e)])
           (bool (and (bool? b) (or (boolean? (bool-b b))
                                    (error "Logicna vrednost ni #t ali #f")))))]
        [(je-interval?? e)
         (let ([i (je-interval?-e e)])
           (bool (and (interval? i)
                      (or (number? (interval-od i))
                          (error "Spodnja meja intervala ni stevilo"))
                      (or (number? (interval-do i))
                          (error "Zgornja meja intervala ni stevilo"))
                      (or (<= (interval-od i) (interval-do i))
                          (error "Zgornja meja intervala je manjsa kot spodnja")))))]
        [(je-nic?? e)
         (let ([i (je-nic?-e e)])
           (bool (nic? i)))]
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; podatkovni tipi ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        [(bool-b (jais (je-konst? e) env)) e]   ; vrnemo konstanto, ce je celo stevilo
        [(bool-b (jais (je-bool? e) env)) e]
        [(bool-b (jais (je-interval? e) env)) e] ; vrnemo interval, ce so meje cela stevila in ce je spodnja meja manjsa od zgornje
        [(bool-b (jais (je-nic? e) env)) e]
        [(and 
          (zdruzi? e)) (zdruzi (jais (zdruzi-e1 e) env) (jais (zdruzi-e2 e) env))]
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; nadzor toka ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        [(ce-potem-sicer? e) 
         (let ([v-test (jais (ce-potem-sicer-b e) env)])
           (if (bool? v-test)
               (if (bool-b v-test)
                   (jais (ce-potem-sicer-e1 e) env)
                   (jais (ce-potem-sicer-e2 e) env))
               (error "pogoj ni logična vrednost")))]
        
        [(negacija? e) 
         (let ([v-test (jais (negacija-e e) env)])
           (cond [(bool-b (jais (je-bool? v-test) env))                            ; negacija bool
                   (bool (not (bool-b v-test)))]
                 [(bool-b (jais (je-konst? v-test) env))                           ; negacija konstante
                   (konst (- (konst-int v-test)))]
                 [(bool-b (jais (je-interval? v-test) env))                        ; negacija intervala
                   (interval 
                    (- (interval-do v-test)) 
                    (- (interval-od v-test)))]
                 [#t (error "nepravilna uporaba negacije")]))]
        
        [(vecje? e) 
         (let ([v1-test (jais (vecje-e1 e) env)]
               [v2-test (jais (vecje-e2 e) env)])
           (cond [(and
                   (bool-b (jais (je-konst? v1-test) env))
                   (bool-b (jais (je-konst? v2-test) env)))
                    (bool (> (konst-int v1-test) (konst-int v2-test)))]
                 [(and
                   (bool-b (jais (je-interval? v1-test) env))
                   (bool-b (jais (je-interval? v2-test) env)))
                    (bool (>                                                   ; vecji interval je sirsi - tisti z vecjo 
                           (- (interval-do v1-test) (interval-od v1-test))     ; absolutno razliko med zacetno in koncno tocko
                           (- (interval-do v2-test) (interval-od v2-test))))]
                 [(bool-b (jais (je-nic? v1-test) env)) (bool #f)]             ; posebna logika za primerjavo z (nic)
                 [(bool-b (jais (je-nic? v2-test) env)) (bool #t)]
                 [#t (error "nepravilna uporaba primerjanja vecje")]))]
        
        [(glava? e) 
         (let ([v-test (jais (glava-e e) env)])
           (cond [(bool-b (jais (je-interval? v-test) env))                    ; glava intervala
                   (konst (interval-od v-test))]
                 [(zdruzi? v-test)                                             ; glava para
                   (zdruzi-e1 v-test)]
                 [#t (error "nepravilna uporaba glave")]))]
        
        [(rep? e) 
         (let ([v-test (jais (rep-e e) env)])
           (cond [(bool-b (jais (je-interval? v-test) env))                    ; rep intervala
                   (konst (interval-do v-test))]
                 [(zdruzi? v-test)                                             ; rep para
                   (zdruzi-e2 v-test)]
                 [#t (error "nepravilna uporaba repa")]))]
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; aritmetika ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        [(sestej? e) 
         (let ([v1-test (jais (sestej-e1 e) env)]
               [v2-test (jais (sestej-e2 e) env)])
           (cond [(bool-b (jais (je-konst? v1-test) env))                                              
                   (cond [(bool-b (jais (je-konst? v2-test) env))                   ; sestevanje konstant
                           (konst (+ (konst-int v1-test) (konst-int v2-test)))]
                         [(bool-b (jais (je-interval? v2-test) env))                ; sestevanje konstante in intervala
                           (interval 
                            (+ (konst-int v1-test) (interval-od v2-test))
                            (+ (konst-int v1-test) (interval-do v2-test)))])]
                 [(bool-b (jais (je-interval? v1-test) env))
                   (cond [(bool-b (jais (je-konst? v2-test) env))                   ; sestevanje konstante in intervala
                           (interval 
                            (+ (interval-od v1-test) (konst-int v2-test))
                            (+ (interval-do v1-test) (konst-int v2-test)))]
                         [(bool-b (jais (je-interval? v2-test) env))                ; sestevanje intervalov
                           (interval 
                            (+ (interval-od v1-test) (interval-od v2-test))
                            (+ (interval-do v1-test) (interval-do v2-test)))])]
                 [#t (error "nepravilna uporaba sestevanja")]))]
        
        [(zmnozi? e) 
         (let ([v1-test (jais (zmnozi-e1 e) env)]
               [v2-test (jais (zmnozi-e2 e) env)])
           (cond [(bool-b (jais (je-konst? v1-test) env))                                              
                   (cond [(bool-b (jais (je-konst? v2-test) env))                   ; mnozenje konstant
                           (konst (* (konst-int v1-test) (konst-int v2-test)))]
                         [(bool-b (jais (je-interval? v2-test) env))                ; mnozenje konstante in intervala
                           (let* ([a (konst-int v1-test)]
                                  [b a]
                                  [c (interval-od v2-test)]
                                  [d (interval-do v2-test)])
                             (interval 
                              (min (* a c) (* a d) (* b c) (* b d))
                              (max (* a c) (* a d) (* b c) (* b d))))]
                         [#t (error "nepravilna uporaba mnozenja")])]
                 [(bool-b (jais (je-interval? v1-test) env))
                   (cond [(bool-b (jais (je-konst? v2-test) env))                   ; mnozenje intervala in konstante
                           (let* ([a (interval-od v1-test)]
                                  [b (interval-do v1-test)]
                                  [c (konst-int v2-test)]
                                  [d c])
                             (interval 
                              (min (* a c) (* a d) (* b c) (* b d))
                              (max (* a c) (* a d) (* b c) (* b d))))]
                         [(bool-b (jais (je-interval? v2-test) env))                ; mnozenje intervalov
                           (let* ([a (interval-od v1-test)]
                                  [b (interval-do v1-test)]
                                  [c (interval-od v2-test)]
                                  [d (interval-do v2-test)])
                             (interval 
                              (min (* a c) (* a d) (* b c) (* b d))
                              (max (* a c) (* a d) (* b c) (* b d))))]
                         [#t (error "nepravilna uporaba mnozenja")])]
                 [#t (error "nepravilna uporaba mnozenja")]))]
        
        [(presek? e) 
         (let ([v1-test (jais (presek-e1 e) env)]                                   ; evalviramo oba izraza
               [v2-test (jais (presek-e2 e) env)])
           (cond [(and                                                              ; preverimo, da gre za intervale
                   (bool-b (jais (je-interval? v1-test) env))
                   (bool-b (jais (je-interval? v2-test) env)))
                    (let ([presek-sirine (lambda (sirsi ozji)                       ; ustvarimo pomozno funkcijo, saj je lazje iskati 
                                           (let ([a (interval-od sirsi)]            ; presek, ce vemo kateri interval je sirsi
                                                 [b (interval-do sirsi)]
                                                 [c (interval-od ozji)]
                                                 [d (interval-do ozji)])
                                             (cond [(and (>= c a) (<= d b))         ; ozji interval je vsebovan v sirsem
                                                     (interval c d)]
                                                   [(and (<= c a) (>= d a))         ; ozji interval je na spodnjem robu sirsega
                                                     (interval a d)]
                                                   [(and (<= c b) (>= d b))         ; ozji interval je na zgornjem robu sirsega
                                                     (interval c b)]
                                                   [#t (nic)])))])                  ; presek je prazen
                      
                      (if (bool-b (jais (vecje v1-test v2-test) env))               ; poiscemo sirsi interval in ustrezno
                          (presek-sirine v1-test v2-test)                           ; poklicemo pomozno funkcijo
                          (presek-sirine v2-test v1-test)))]
                 [#t (error "nepravilna uporaba preseka")]))]
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; spremenljivke ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        [(naj-bo? e)                                                                   ; evalviramo izraz e2 v razsirjenem okolju
         (jais (naj-bo-e2 e) (cons (cons (naj-bo-s e) (jais (naj-bo-e1 e) env)) env))]
        [(sprem? e)                                                                    ; vrnemo vrednost prve pojavitve spremenljivke 
         (let ([a (assoc (sprem-s e) env)])
                  (if a
               (cdr a)
               (error "Ne najdem spremenljivke v okolju")))]
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; funkcije ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        [(and 
          (funkcija? e)
          (or (string? (funkcija-ime e))                                 ; preverimo, da je ime funkcije niz ali #f
              (and (boolean? (funkcija-ime e))
                   (not (funkcija-ime e)))
              (error "Ime funkcije ni niz"))
          (or (list? (funkcija-farg e))                                  ; preverimo, ce so imena argumentov podana v seznamu
              (error "Imena argumentov funkcije niso podana v seznamu")))
          (or (foldl (lambda (x is-string)                               ; preverimo, ce so imena argumentov nizi
                  (and is-string (string? x))) #t (funkcija-farg e))
              (error "Imena argumentov funkcije niso nizi")) 
         (ovojnica env e)]                                               ; definicijo funkcije evalviramo kot ovojnico
        [(and 
          (skripta? e)
          (or (string? (skripta-ime e))                                  ; ime skripte mora biti niz ali #f
              (and (boolean? (skripta-ime e))
                   (not (skripta-ime e)))
              (error "Ime skripte ni niz"))) e]
        [(klici? e) (let ([klic (jais (klici-e e) env)])                ; najprej evalviramo klic
                      (cond [(skripta? klic) 
                             (jais (skripta-telo klic)                  ; izvedemo skripto s tem da jo dodamo 
                                   (cons (cons (skripta-ime klic)       ; na okolje zaradi rekurzije
                                               klic)
                                         env))]
                            [(ovojnica? klic)
                             (let ([klic-env (razsiri-okolje                            ; razsirimo okolje s parametri v svoji
                                           (funkcija-farg (ovojnica-f klic))            ; funkciji (glej zgoraj)
                                           (klici-arg e)
                                           (ovojnica-env klic))])
                              (jais (funkcija-telo (ovojnica-f klic))                   ; evalviramo telo funkcije z okoljem,
                                    (cons (cons (funkcija-ime (ovojnica-f klic))        ; kamor dodamo par imena funkcije 
                                                (ovojnica klic-env (ovojnica-f klic)))  ; in nove ovojnice z razsirjenim okoljem
                                          klic-env)))]
                            [#t (error "klic funkcije nima ustreznih argumentov")]))]        
        
        
        [#t (error "sintaksa izraza ni pravilna")]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MAKROJI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (odstej e1 e2)
  (sestej e1 (negacija e2)))

(define (manjsi e1 e2)                             ; ce je prvi manjsi od drugega je drugi vecji od prvega
  (vecje e2 e1))

(define (enak e1 e2)
  (ce-potem-sicer (vecje e1 e2)
                  (bool #f)
                  (ce-potem-sicer (manjsi e1 e2)
                                  (bool #f)
                                  (bool #t))))     ; ce je enak ni niti manjsi niti vecji

(define (vsebuje i1 i2)
  (ce-potem-sicer (enak (presek i1 i2) i2)
                  (bool #t)                        ; ce je presek enak i2 potem i1 vsebuje i2
                  (bool #f)))