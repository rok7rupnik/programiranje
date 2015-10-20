#lang racket


; ***************************************************************
; ************** 1. IMPLEMENTACIJA INTERPRETERJA ****************
; ***************************************************************


(struct konst (int) #:transparent)     ; konstanta; argument je število
(struct bool (b) #:transparent)        ; b ima lahko vrednost true or false
(struct negiraj (e) #:transparent)     ; e je lahko izraz
(struct sestej (e1 e2) #:transparent)  ; e1 in e2 sta izraza
(struct ce-potem-sicer (pogoj res nires) #:transparent) ; pogoj, res, nires hranijo izraze


(define (jais e)
  (cond [(konst? e) e]   ; vrnemo izraz v ciljnem jeziku
        [(bool? e) e]
        [(negiraj? e) 
         (let ([v (jais (negiraj-e e))])
           (cond [(konst? v) (konst (- (konst-int v)))]
                 [(bool? v) (bool (not (bool-b v)))]
                 [#t (error "negacija nepričakovanega izraza")]))]
        [(sestej? e) 
         (let ([v1 (jais (sestej-e1 e))]
               [v2 (jais (sestej-e2 e))])
           (if (and (konst? v1) (konst? v2))
               (konst (+ (konst-int v1) (konst-int v2)))
               (error "seštevanec ni številka")))]
        [(ce-potem-sicer? e) 
         (let ([v-test (jais (ce-potem-sicer-pogoj e))])
           (if (bool? v-test)
               (if (bool-b v-test)
                   (jais (ce-potem-sicer-res e))
                   (jais (ce-potem-sicer-nires e)))
               (error "pogoj ni logična vrednost")))]
        [#t (error "sintaksa izraza ni pravilna")]
        ))


; TESTI
;(jais (bool true))
;(jais (negiraj (bool true)))
;(jais (negiraj (konst 5)))
;(jais (sestej (konst 1) (ce-potem-sicer (bool true) (konst 5) (konst 10))))

;težava s sintakso
;(jais (negiraj (konst "lalala")))
;(jais (negiraj 1 2))



; ***************************************************************
; **************** 2. DEKLARIRANJE SPREMENLJIVK *****************
; ***************************************************************

; dopolnimo interpreter tako, da imamo "register" - prostor za 
; eno samo spremenljivko, ki je evalviran poljuben izraz v jeziku JAIS

; dodamo deklaracijo 
(struct shrani (vrednost izraz) #:transparent)   ; shrani vrednost v register in evalvira izraz v novem okolju
(struct beri () #:transparent)                   ; bere register, ki je že evalviran v vrednost JAIS

; razširimo interpreter z okoljem, ki je od začetka prazno
(define (jais2 e) 
  (letrec ([jais (lambda (e env) 
                   (cond [(konst? e) e]   ; vrnemo izraz v ciljnem jeziku
                         [(bool? e) e]
                         ;[(shrani? e) (jais (shrani-izraz e) (shrani-vrednost e))]  ; ne zadošča, potrebna evalvacija vrednosti
                         [(shrani? e) (jais (shrani-izraz e) (jais (shrani-vrednost e) env))]
                         [(beri? e) env]
                         ; tukaj pride koda za negacijo
                         [(sestej? e) 
                          (let ([v1 (jais (sestej-e1 e) env)]
                                [v2 (jais (sestej-e2 e) env)])
                            (if (and (konst? v1) (konst? v2))
                                (konst (+ (konst-int v1) (konst-int v2)))
                                (error "seštevanec ni številka")))]
                         ; tukaj pride koda za ce-potem-sicer
                         [#t (error "sintaksa izraza ni pravilna")]))])
    (jais e null)))


; TESTI
;(jais2 (bool true))
;(jais2 (shrani (konst 5) (sestej (beri) (sestej (konst 1) (beri)))))
;(jais2 (shrani (konst 4) (konst 4)))
;(jais2 (shrani (konst 4) (beri)))
;(jais2 (shrani (sestej (konst 4) (konst 1)) (beri)))
;(jais2 (shrani (konst 4) (sestej (beri) (beri))))
; senčenje spremenljivk
;(jais2 (shrani (konst 4) 
;               (sestej (beri)
;                       (shrani (konst 2)
;                               (sestej (beri)
;                                       (sestej (konst 1) (beri)))))))




; ***************************************************************
; ***************** 3. DEKLARIRANJE FUNKCIJ *********************
; ***************************************************************

; dopolnimo interpreter tako, da imamo interno strukturo za ovojnico,
; definicije spremenljivk in funkcijske klice
; omogočimo samo klice FUNKCIJ BREZ ARGUMENTOV

; struktura za ovojnico
(struct ovojnica (okolje fun) #:transparent)   ; funkcijska ovojnica: vsebuje okolje in kodo funkcije
; definicija funkcije v programu)
(struct funkcija (ime telo) #:transparent)     ; ime funkcije in telo
; funkcijski klic
(struct klici (ovojnica) #:transparent)        ; funkcijski klic


; razširimo interpreter z okoljem, ki je od začetka prazno
(define (jais3 e) 
  (letrec ([jais (lambda (e env) 
                   (cond [(funkcija? e) (ovojnica env e)]   ; definicijo funkcije shranimo kot ovojnico
                         [(klici? e) (let ([o (jais (klici-ovojnica e) env)])
                                       (if (ovojnica? o)
                                           (jais (funkcija-telo (ovojnica-fun o))  ; izvedemo kodo, ki je v telesu funkcije
                                                 (ovojnica-okolje o))              ; kodo funkcije izedemo v okolju ovojnice (leksikalno)
                                                  ; okolje je potrebno še razširiti (glej predavanja)!!!
                                           (error "klic funkcije nima ustreznih argumentov")))]
                         [(konst? e) e]   ; vrnemo izraz v ciljnem jeziku
                         [(bool? e) e]
                         [(shrani? e) (jais (shrani-izraz e) (jais (shrani-vrednost e) env))]
                         [(beri? e) env]
                         ; tukaj pride koda za negacijo
                         [(sestej? e) 
                          (let ([v1 (jais (sestej-e1 e) env)]
                                [v2 (jais (sestej-e2 e) env)])
                            (if (and (konst? v1) (konst? v2))
                                (konst (+ (konst-int v1) (konst-int v2)))
                                (error "seštevanec ni številka")))]
                         ; tukaj pride koda za ce-potem-sicer
                         [#t (error "sintaksa izraza ni pravilna")]))])
    (jais e null)))


; TESTI
;(jais3 (funkcija "sestevanje" (sestej (beri) (konst 1))))          ; samo prikaz interne predstavitve
;(jais3 (klici (funkcija "sestevanje" (sestej (beri) (konst 1)))))  ; ni okolja
;(jais3 (shrani (konst 4) (klici (funkcija "sestevanje" (sestej (beri) (konst 1))))))
;(jais3 (shrani (konst 4) (klici (funkcija "sestevanje" (sestej (beri) (konst 1))))))
;(jais3 (shrani (konst 4) (shrani (funkcija "sestevanje" (sestej (beri) (konst 1))) 
;                                 (sestej (shrani (konst 1) (beri)) (klici (beri)))))) ; čeprav shrani povozi lokalno okolje, ima funkcija svoje v ovojnici


; MOŽNE RAZŠIRITVE 
; - rekurzivne funkcije
; - več argumentov funkcije
; - anonimne funkcije
; - optimizacija ovojnic



; ***************************************************************
; ********************** 4. MAKRO ZA JAIS ***********************
; ***************************************************************


(define (trikratnik x)
  (sestej x (sestej x x)))
;> (jais3 (trikratnik (konst 4)))


; logični operatorji
(define (in e1 e2)
  (ce-potem-sicer e1 e2 (bool #f)))
;> (jais3 (in (bool #f) (bool #f)))
;> (jais3 (in (bool #f) (bool #t)))
;> (jais3 (in (bool #t) (bool #f)))
;> (jais3 (in (bool #t) (bool #t)))

(define (ali e1 e2)
  (ce-potem-sicer e1 (bool #t) e2))

(define (xor e1 e2)
  (ce-potem-sicer e1 (ce-potem-sicer e2 (bool #f) (bool #t)) e2))


; vsota seznama
(define (vsota-sez sez)
  (if (null? sez)
      (konst 0)
      (sestej (car sez) (vsota-sez (cdr sez)))))
;> (vsota-sez (list (konst 3) (konst 5) (konst 2) (konst 7)))


