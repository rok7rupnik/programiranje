Control.Print.printDepth := 100;

(***************************************************************)
(************************* 0. OPCIJE ***************************)
(***************************************************************)

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
	 
	 
(* najvecji3: sez -> int option *)
fun najvecji3 (sez : int list) =
    if null sez
    then NONE
    else let val max_rep = najvecji3(tl sez)
	 in
	     if isSome(max_rep) andalso valOf(max_rep) > hd sez
	     then max_rep
	     else SOME (hd sez)
	 end
	 
	 
(***************************************************************)


(* podan je seznam meritev; èe meritev manjka, ima vrednost -1 *)
(* vse meritve morajo biti podane, da je tudi se¹tevek veljaven *)
fun sestej1 (seznam_meritev: int list) =
    if null seznam_meritev
    then 0
    else let val trenutna = hd seznam_meritev
	     val ostanek = sestej1 (tl seznam_meritev)
	 in if (trenutna <> ~1) andalso (ostanek <> ~1)
	    then trenutna + ostanek
	    else ~1
	 end

(* problema sta dva: 
(1) ~1 kot neveljavna vhodna meritev in
(2) kaj vrniti kot neveljavno vsoto? 
Uporabimo torej opcije 2x: kot elemente seznama in kot odgovor *)

(* int option list --> int option *)
fun sestej2 (seznam_meritev: int option list) =
    if null seznam_meritev
    then NONE
    else if null (tl seznam_meritev)
    then hd seznam_meritev	     
    else let val trenutna = hd seznam_meritev
	     val vsota_ostalih = sestej2 (tl seznam_meritev)
	 in if (isSome trenutna) andalso (isSome vsota_ostalih)
	    then SOME ((valOf trenutna) + (valOf vsota_ostalih))
	    else NONE
	 end



(***************************************************************)
(********************** 1. ZAPISI  *****************************)
(***************************************************************)

fun izpis_studenta (zapis: {absolvent:bool, ime:string, ocene:(string * int) list, starost:int}) =
    (#ime zapis) ^ " je star " ^  Int.toString(#starost zapis) ^ " let."


(***************************************************************)
(************** 2. SINONIMI PODATKOVNIH TIPOV  *****************)
(***************************************************************)


(* primer 1: ¹tudent *)
type student = {absolvent:bool, ime:string, ocene:(string * int) list, starost:int}

fun izpis_studenta2 (zapis: student) =
    (#ime zapis) ^ " je star " ^  Int.toString(#starost zapis) ^ " let."



(* primer 2: artikli v trgovini *)
type artikel = string * int

fun najmanj2mleka (a: artikel) =
    (#1 a = "mleko") andalso (#2 a >=2)   

fun prestejizdelke(sa: artikel list): int =
    if null sa
    then 0
    else #2 (hd sa) + prestejizdelke(tl sa)




(***************************************************************)
(******************* 3. TERKE SO ZAPISI  ***********************)
(***************************************************************)

val test = {1="Zivjo", 2="adijo"}; (* enakovredno terki *)




(***************************************************************)
(*************** 4. DEKLARACIJA LASTNIH TIPOV  *****************)
(***************************************************************)
datatype prevozno_sredstvo = Bus of int
			   | Avto of string * string 
			   | Pes

fun obdelaj_prevoz x =
    case x of
	Bus i => i+10
      | Avto (s1,s2) => String.size s1 + String.size s2
      | Pes => 0

fun obdelaj_prevoz2 x =
    case x of
	Bus i => i+10
      | Avto (s1,s2) => String.size s1 + String.size s2
      | Pes => 0 



(* PRIMER: ARITMETIÈNI IZRAZI **********************************)

datatype izraz =  Konstanta of int 
		| Negiraj of izraz
		| Plus of izraz * izraz
		| Minus of izraz * izraz
		| Krat of izraz * izraz
		| Deljeno of izraz * izraz
		| Ostanek of izraz * izraz
				      
val izraz1 = Konstanta 3
val izraz2 = Negiraj (Konstanta 3)
val izraz3 = Plus (Konstanta 3, Ostanek(Konstanta 18, Konstanta 4))
val izraz4 = Deljeno (izraz3, Negiraj izraz2)

fun eval e =
    case e of
        Konstanta i => i
      | Negiraj e  => ~ (eval e)
      | Plus(e1,e2) => (eval e1) + (eval e2)
      | Minus(e1,e2) => (eval e1) - (eval e2)
      | Krat(e1,e2) => (eval e1) * (eval e2)
      | Deljeno(e1,e2) => (eval e1) div (eval e2)
      | Ostanek(e1,e2) => (eval e1) mod (eval e2)

fun stevilo_negacij e =
    case e of
	Konstanta i => 0
      | Negiraj e  => (stevilo_negacij e) + 1
      | Minus(e1,e2) => (stevilo_negacij e1) + (stevilo_negacij e2)
      | Krat(e1,e2) => (stevilo_negacij e1) + (stevilo_negacij e2)
      | Deljeno(e1,e2) => (stevilo_negacij e1) + (stevilo_negacij e2)
      | Ostanek(e1,e2) => (stevilo_negacij e1) + (stevilo_negacij e2)




(***************************************************************)
(************* 5. SEZNAMI IN OPCIJE KOT DATATYPE  **************)
(***************************************************************)
 
(* definicija lastnega seznama *)
datatype mojlist = konec
       | Sez of int * mojlist

(* definicija lastne opcije *)
datatype intopcija = SOME of int
		   | NONE


(* SEZNAM - ujemanje vzorcev *)
fun glava sez =
    case sez of
	[] => 0 (* !!! kasneje: exception *)
     | prvi::ostali => prvi

fun prestej_elemente sez =
    case sez of
	[] => 0
     | glava::rep => 1 + prestej_elemente rep


(* OPCIJE - ujemanje vzorcev *)
fun vecji_od_5 opcija =
    case opcija of
	NONE => false
     | SOME x => (x>5)



(***************************************************************)
(****************  6. POLIMORFIZEM POD. TIPOV  *****************)
(***************************************************************)

datatype ('a, 'b) seznam =
	 Elementa of ('a * ('a, 'b) seznam)
       | Elementb of ('b * ('a, 'b) seznam)
       | konec

(* fn: seznam -> (int * int) *)
fun prestej sez =
    case sez of
	Elementa(x, preostanek) => let val vp = prestej(preostanek)
				   in (1+ (#1 vp), #2 vp)
				   end
      | Elementb(x, preostanek) => let val vp = prestej(preostanek)
				   in (#1 vp, 1+ (#2 vp))
				   end
      | konec => (0,0)



(***************************************************************)
(************* 7. UJEMANJE VZORCEV PRI DEKLARACIJAH ************)
(***************************************************************)
val (a,b,c) = (1,2,3);
val (a,b) = (3,(true,3.14));
val (a,(b,c)) = (3,(true,3.14));
val {prva=a, tretja=c, druga=b} = {prva=true, druga=false, tretja=3};
val glava::rep = [1,2,3,4];
val prvi::drugi::ostali = [1,2,3,4,5];

fun sestej1 (trojcek: int*int*int) =
    let val (a,b,c) = trojcek
    in a+b+c
    end

fun sestej2 (a,b,c) =   (* izvede ujemanje vzorca tukaj *)
    a + b + c



(***************************************************************)
(******** 8. FUNKCIJE SPREJEMAJO SAMO 1 ARGUMENT ***************)
(***************************************************************)

fun povecaj (a,b,c) = (a+1,b+1,c+1)
