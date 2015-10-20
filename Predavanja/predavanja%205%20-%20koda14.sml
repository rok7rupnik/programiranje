
(***************************************************************)
(***************** 0. FUNKCIJE VI©JEGA REDA  *******************)
(***************************************************************)


(* 1. ILUSTRATIVEN PRIMER *)

fun operacija1 x = x*x*x
fun operacija2 x = x + 1
fun operacija3 x = ~x

val zbirka_operacij = (operacija1, "lala", operacija3, 144)

fun izvedi1 podatek =
	   (#1 zbirka_operacij) ((#3 zbirka_operacij) podatek)

fun izvedi2 (pod, funkcija) =
    funkcija (pod+100)



(***************************************************************)
(* 2. FUNKCIJE KOT ARGUMENTI FUNKCIJ *)
(* ponavljajoèa se programska koda: *)
fun zmnozi_nkrat (x,n) =  
    if n=0
    then x 
    else x * zmnozi_nkrat(x, n-1)

fun sestej_nkrat (x,n) =  
    if n=0 
    then x 
    else x + sestej_nkrat(x, n-1)

fun rep_nti (sez,n) =  
    if n=0 
    then sez 
    else tl (rep_nti(sez, n-1))


(* faktorizacija ponavljajoèih delov kode v splo¹no funkcijo *)
fun nkrat (f, x, n) =
    if n=0
    then x
    else f(x, nkrat(f, x, n-1))

fun pomnozi(x,y) = x*y
fun sestej(x,y) = x+y
fun rep(x,y) = tl y

fun zmnozi_nkrat_kratka (x,n) = nkrat(pomnozi, x, n)
fun sestej_nkrat_kratka (x,n) = nkrat(sestej, x, n)
fun rep_nti_kratka (x,n) = nkrat(rep, x, n)


(***************************************************************)
(* 3. FUNKCIJE, KI VRAÈAJO FUNKCIJE  *)

fun odloci x =
    if x>10
    then (let fun prva x = 2*x in prva end)
    else (let fun druga x = x div 2 in druga end)



(***************************************************************)
(* 4. ANONIMNE FUNKCIJE *)

fun zmnozi_nkrat_skoraj (x,n) = 
    nkrat(let fun pomnozi (x,y) = x*y in pomnozi end,   x, n)

fun zmnozi_nkrat_mega (x,n) = nkrat(fn (x,y) => x*y, x, n)
fun sestej_nkrat_mega (x,n) = nkrat(fn(x,y) => x+y, x, n)
fun rep_nti_mega (x,n) = nkrat(fn(_,x)=>tl x, x, n)


(***************************************************************)
(* primer na seznamu - anon. fun. in izogib ovijanju funkcij v funkcije *)
fun prestej sez =
    case sez of 
	[] => 0
      | glava::rep => 1 + prestej rep

fun sestej_sez sez =
    case sez of 
	[] => 0
      | glava::rep => glava + sestej_sez rep

(* faktorizacija *)
fun predelaj_seznam (f, sez) =
    case sez of
	[] => 0
      | glava::rep => (f sez) + (predelaj_seznam (f,rep))

fun prestej_super sez = predelaj_seznam (fn x => 1, sez)
fun sestej_sez_super sez = predelaj_seznam(hd, sez)   (* hd namesto fn x => hd x !!! *)


(***************************************************************)
(* 5. MAP IN FILTER *)

fun map (f, sez) =
    case sez of
	[] => []
      | glava::rep => (f glava)::map(f, rep)

fun filter (f, sez) =
    case sez of
	[] => []
      | glava::rep => if (f glava)
		      then glava::filter(f, rep)
		      else filter(f, rep)


(* PRIMERI *)

(* preslikaj seznam seznamov v seznam glav vgnezdenih seznamov *)
fun nal1 sez = map(hd, sez)
(* preslikaj seznam seznamov v seznam dol¾in vgnezdenih seznamov *)
fun nal2 sez = map(prestej, sez)
(* preslikaj seznam seznamov v seznam samo tistih seznamov, katerih dol¾ina je dalj¹a od 2 *)
fun nal3 sez = filter(fn x => (prestej x) >= 2, sez)
(* preslikaj seznam seznamov v seznam vsot samo lihih elementov vgnezdenih seznamov *)
fun nal4 sez =
    map(sestej_sez,
	map(
	    fn el => filter(fn x => x mod 2 = 1, el),
	    sez)
       )

	   
(***************************************************************)
(******************** 1. MAP IN FILTER *************************)
(***************************************************************)
fun map (f, sez) =
    case sez of
	[] => []
      | glava::rep => (f glava)::map(f, rep)

fun filter (f, sez) =
    case sez of
	[] => []
      | glava::rep => if (f glava)
		      then glava::filter(f, rep)
		      else filter(f, rep)

(* PRIMER: preslikaj seznam seznamov v seznam samo tistih seznamov, katerih dolžina je liha *)
fun lihi_podsez sez = filter(fn x => (List.length x) mod 2 = 1, sez)


(***************************************************************)
(************************* 2. FOLD *****************************)
(***************************************************************)

fun fold (f, acc, sez) =
    case sez of 
	[] => acc
      | glava::rep => fold(f, f(acc, glava), rep)


fun f2 xs = fold ((fn (x,y) => x andalso y >= 0), true, xs)

(* PRIMER 1: vsota elementov *)
fun vsota_el sez = fold(fn (x,y) => x+y, 0, sez);  

(* PRIMER 2: dolžina seznama *) 
fun dolzina_sez sez = fold(fn (x,y) => x+1, 0, sez);

(* PRIMER 3: izberi zadnji element v seznamu *)
fun zadnji sez = fold (fn (x,y) => y, hd sez, sez)

(* PRIMER 4: skalarni produkt [a,b,c]*[d,e,f] = ab+be+cf *)
fun skalarni [v1, v2] =
    fold(fn (x,y) => x+y, 0, map(fn (e1,e2) => e1*e2, ListPair.zip(v1,v2)))
  | skalarni _ = raise Fail "napaèni argumenti";

(* PRIMER 5: izberi nti element v seznamu *)
fun nti (sez, n) = 
    fold(fn(x,(y,z)) => z, ~1,
	   filter(fn (x,y) => x=n,
		  ListPair.zip (List.tabulate (List.length sez, fn x => x+1),
				sez)
		 )
	  )



(***************************************************************)
(******************** 3. LEKSIKALNI DOSEG **********************)
(***************************************************************)

(*
(* 1. primer *)

val a = 1            (* a=1 *)
fun f1 x = x + a     (* fn: x => x+1 *)
val rez1 = f1 3      (* rez1 = 4 *)
val a = 5            (* a=5 *)
val rez2 = f1 3      (* rez2 = (fn: x => x+1) 3 = 4 *)

(* 2. primer *)
val c = 1            (* c=1 *)
fun f2 b = c + b     (* fn: b => b+1 *)
val c = 5            (* c=5 *)
val b = 2            (* b=2 *)
val rez = f2 (c+b)   (* rez = (fn: b => b+1) (5+2) = 7+1 = 8 *)

(* 3. primer *)
val u = 1                  (* u = 1 *)
fun f v =
    let 
        val u = v + 1      (* u = v+1 *)
    in
        fn w => u + v + w  (* f = fn w => v+1 + v + w *)
    end
val u = 3                  (* u = 3 *)
val g = f 4                (* g = (fn w => v+1 + v + w) 4  --> 4+1+4+w = 9+w *)   
val v = 5                  (* v = 5 *)
val w = g 6                (* w = (fn w => 9+w) 6 --> 15*) 

*)

(***************************************************************)
(*************** 4. PREDNOSTI LEKSIKALNEGA DOSEGA **************)
(***************************************************************)

(*
(* PREDNOST 1:
     - neodvisnost lokalnih spremenljivk od zunanjega okolja 
     - neodvisnost funkcije od argumentov *)
(* spodnji funkciji sta enakovredni *)
fun fun1 y =
    let 
	val x = 3
    in
	fn z => x + y + z
    end

fun fun2 y =
    let 
	val q = 3
    in
	fn z => q + y + z
    end

val x = 42 (* ne igra nobene vloge *)
val a1 = (fun1 7) 4
val a2 = (fun2 7) 4



(* PREDNOST 2:
     - podatkovni tip lahko doloèimo pri deklaraciji funkcije *)
val x = 1
fun fun3 y = 
    let val x = 3
    in fn z => x+y+z end   (* int -> int -> int *)
val x = false              (* NE VPLIVA NA PODATKOVNI TIP KASNEJŠEGA KLICA! *)
val g = fun3 10            (* vrne fn, ki prišteje 13 *)
val z = g 11               (* 13 + 11 = 24 *)



(* PREDNOST 3:
     - ovojnica shrani ("zapeèe") interne podatke za klic funkcije *)
fun filter (f, sez) =
    case sez of
	[] => []
      | x::rep => if (f x)
		  then x::filter(f, rep)
		  else filter(f, rep)
			     
fun vecjiOdX x = fn y => y > x
fun brezNegativnih sez = filter(vecjiOdX ~1, sez)
(* POZOR:
- x je neodvisen od x-a v funkciji filter; èe ne bi bil, 
  bi primerjali elemente same s sabo (x, ki je argument predikata
  in x, ki nastopa kot glava v funkciji filter 
- prvi argument v klicu filter() --- vecjiOdX ~1 --- je ovojnica,
  ki hrani shranjen interni x, ki je neodvisen od x v filter() *)

*)

