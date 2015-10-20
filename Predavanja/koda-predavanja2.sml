
(***************************************************************)
(************************* 1. TERKE  ***************************)
(***************************************************************)

(* sestej stevili, podani v terki *)
fun vsota (stevili: int*int) =
    (#1 stevili) + (#2 stevili)

(* obrni elementa terke-para *)	
fun obrni (stevili: int*int) =
    (#2 stevili, #1 stevili)

(* prepleti dve trimestni terki *)
fun prepleti (terka1: int*int*int, terka2: int*int*int) =
    (#1 terka1, #1 terka2, #2 terka1, #2 terka2, #3 terka1, #3 terka2)

(* sortiraj par stevil v terki po velikosti *)
fun sortiraj_par (terka: int*int) =
    if #1 terka < #2 terka
    then terka
    else (#2 terka, #1 terka)



(***************************************************************)
(************************* 2. SEZNAMI **************************)
(***************************************************************)

(* stevilo elementov v seznamu *)
fun stevilo_el(sez: int list) =
    if null sez
    then 0
    else 1 + stevilo_el(tl sez)

(* vsota elementov v seznamu *)
fun vsota_el(sez: int list) =
    if null sez
    then 0
    else hd sez + vsota_el(tl sez)

(* n-ti element seznama *)
fun n_ti_element(sez: int list, n: int) =
    if n=1
    then hd sez
    else n_ti_element(tl sez, n-1)

(* konkatenacija seznamov - append *)
fun zdruzi_sez(sez1: int list, sez2: int list) =
    if null sez1
    then sez2
    else (hd sez1)::zdruzi_sez(tl sez1, sez2)

(* prepletemo seznama v terke do dolzine krajsega od seznamov *)
fun prepleti_sez(sez1: int list, sez2: int list) =
    if null sez1 orelse null sez2
    then []
    else (hd sez1, hd sez2)::prepleti_sez(tl sez1, tl sez2)

(* vsota parov elementov v terkah vzdolz seznama *)
fun vsota_parov(sez: (int*int) list) =
    if null sez
    then []
    else (#1 (hd sez) + #2 (hd sez))::vsota_parov(tl sez)

(* filtiranje imen predmetov, kjer smo dobili pozitivno oceno *)
fun filter_poz_ocen(sez: (string*int) list) =
    if null sez
    then []
    else if #2 (hd sez) > 5 
    then (#1 (hd sez))::filter_poz_ocen(tl sez)
    else filter_poz_ocen(tl sez)



(***************************************************************)
(*********************** 3. LOKALNO OKOLJE *********************)
(***************************************************************)

(* eksperiment *)
val a = 3
val b = 7
fun sestej3(c: int) =
    let
	val a = 5
    in
	a + (let val b=4 in b+1 end) + (let val c=b+2 in c end)
    end


(* lokalna funkcija *)
(* uporaba zunanje funkcije *)
fun povprecje(sez: int list) =
    Real.fromInt(vsota_el(sez)) / Real.fromInt(stevilo_el(sez))

(* uporaba notranje pomozne funkcije *)
fun povprecje2(sez: int list) =
    let
	fun stevilo_el(sez: int list) =
	    if null sez
	    then 0
	    else 1 + stevilo_el(tl sez)
	fun vsota_el(sez: int list) =
	    if null sez
	    then 0
	    else hd sez + vsota_el(tl sez)
	val vsota = Real.fromInt(vsota_el(sez))
	val n = Real.fromInt(stevilo_el(sez))
    in
	vsota/n
    end


(* primer: odstranitev odvecnih parametrov *)
fun sestej1N (n: int) =
    let fun sestejAB (a: int, b: int) =  (* pomozna funkcija *)
	    if a=b then a else a+ sestejAB(a+1, b)
    in
	sestejAB(1, n)
    end

fun sestej1N_lepse (n: int) =
    let 
	fun sestejAB (a: int) =  (* odstranimo parameter b *)
	    if a=n then a else a + sestejAB(a+1)
    in
	sestejAB(1)
    end



(* ucinkovitost pri rekurziji *)
fun najvecji_el (sez : int list) =
    if null sez
    then 0  (* !!! *)
    else if null (tl sez)
    then hd sez
    else if hd sez > najvecji_el(tl sez)
    then hd sez
    else najvecji_el(tl sez)

fun najvecji_el_popravljen (sez : int list) =
    if null sez
    then 0  (* !!! *)
    else if null (tl sez)
    then hd sez
    else let val max_rep = najvecji_el_popravljen(tl sez)
	 in
	     if hd sez > max_rep
	     then hd sez
	     else max_rep
	 end


