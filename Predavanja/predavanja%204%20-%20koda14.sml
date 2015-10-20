
(***************************************************************)
(*************** 1. REKURZIVNO UJEMANJE VZORCEV  ***************)
(***************************************************************)


(* uporaba anonimne spremenljivke, kjer ne potrebujemo vrednosti *)
fun dolzina (sez:int list) =
    case sez of
       [] => 0
      | (* NAMESTO: glava::rep *) _::rep => 1 + dolzina rep


(***************************************************************)
(* 1. PRIMER *)
(* se¹tevanje dveh seznamov po elementih; seznama morata biti enako dolga *)

(* SLAB NAÈIN: *)
exception LengthProblem

fun sestej_seznama (sez1, sez2) =
    case sez1 of
	[] => (case sez2 of
		   [] => []
		 | glava::rep => raise LengthProblem)
      | glava1::rep1 => (case sez2 of
			     [] => raise LengthProblem
			   | glava2::rep2 => (glava1+glava2)::sestej_seznama(rep1,rep2))

(* BOLJ©I NAÈIN z gnezdenjem vzorcev*)
fun sestej_seznama2 seznama =
    case seznama of
	([], []) => []
      | (glava1::rep1, glava2::rep2) => (glava1+glava2)::sestej_seznama(rep1,rep2)
      | _ => raise LengthProblem


(***************************************************************)
(* 2. PRIMER *)
fun check_fibonacci sez =
    case sez of
      (glava::(drugi::(tretji::rep))) => (tretji = (glava+drugi)) andalso check_fibonacci (drugi::(tretji::rep))
      | _ => true


(***************************************************************)
(* 3. PRIMER *)
datatype sodost = S | L | N

fun sodost_sestevanje (a,b) = 
  let 
      fun sodost x = if x=0 then N
		     else if x mod 2 = 0 then S 
		     else L
  in
      case (sodost a, sodost b) of
	  (S,L) => L
	| (S,_) => S
	| (L,L) => S
	| (L,_) => L
	| (N,x) => x
  end




(***************************************************************)
(*************** 2. SKLEPANJE NA PODATKOVNI TIP  ***************)
(***************************************************************)

(* ne deluje, unknown flex record *)
(*
fun sestej1 stevili =
  #1 stevili + #2 stevili
*)

(* moramo opredeliti podatkovni tip *)
fun sestej2 (stevili:int*int) =
    #1 stevili + #2 stevili

(* sklepanje na tip deluje pri uporabi vzorcev *)
fun sestej3 (s1, s2) =
    s1 + s2


(***************************************************************)
(* polimorfizem pri sklepanju na tip *)

(* ni polimorfna *)
fun vsota_el sez =
    case sez of
	[] => 0
      | glava::rep => glava + vsota_el rep

(* je polimorfna *)
fun zdruzi (sez1, sez2) =
    case sez1 of
	[] => sez2
      | glava::rep => glava::zdruzi(rep, sez2)

(* je polimorfna *)
fun sestej_zapis {prvi=a, drugi=b, tretji=c, cetrti=d, peti=e} =
    a+d



(***************************************************************)
(************************* 3. IZJEME  **************************)
(***************************************************************)


(* prej - brez uporabe izjem *)
(*
fun glava sez =
    case sez of
	[] => 0 (* !!! kasneje: exception *)
     | prvi::ostali => prvi
*)

exception PrazenSeznam

fun glava sez =
    case sez of
	[] => raise PrazenSeznam
     | prvi::ostali => prvi


(***************************************************************)
(* primer izjeme pri deljenju z 0 *)
exception DeljenjeZNic

fun deli1 (a1, a2) =
    if a2 = 0 
    then raise DeljenjeZNic
    else a1 div a2

fun tabeliraj1 zacetna =
    deli1(zacetna,zacetna-5)::tabeliraj1(zacetna-1)
    handle DeljenjeZNic => [999]


(***************************************************************)
(* ¹e bolj splo¹no: prenos izjeme v parametru *)
(* fn : int * int * exn -> int *)
fun deli2 (a1, a2, napaka) =
    if a2 = 0 
    then raise (* SPREMEMBA *) napaka
    else a1 div a2

fun tabeliraj2 (zacetna, moja_napaka) =
    deli2(zacetna, zacetna-5, moja_napaka)::tabeliraj2(zacetna-1, moja_napaka)
    handle moja_napaka => [999]


(***************************************************************)
(* izjema s parametrom *)
exception MatematicnaTezava of int*string

fun deli3 (a1, a2) =
    if a2 = 0 
    then raise MatematicnaTezava(a1, "deljenje z 0")
    else a1 div a2

fun tabeliraj3 zacetna =
    Int.toString(deli3(zacetna,zacetna-5)) ^ "  " ^ tabeliraj3(zacetna-1) 
    handle MatematicnaTezava(a1, a2) => a2 ^ " stevila " ^ Int.toString(a1)




(***************************************************************)
(************ 4. REPNA REKURZIJA Z AKUMULATORJEM  **************)
(***************************************************************)

fun potenca (x,y) =  
    if y=0 
    then 1 
    else x * potenca(x, y-1)

(* prevedba v repno rekurzijo *)
fun potenca_repna (x,y) =
    let
	fun pomozna (x,y,acc) =
	    if y=0
	    then acc
	    else pomozna(x, y-1, acc*x)
    in
	pomozna(x,y,1)
    end
	   

(***************************************************************)
(* PRIMERI *)
(* obrni elemente seznama *)
fun obrni sez =
   case sez of
       [] => []
     | x::rep => (obrni rep) @ [x]

fun obrni_repna sez =
    let 
	fun pomozna(sez,acc) =
            case sez of
                [] => acc
              | x::rep => pomozna(rep, x::acc)
    in
        pomozna(sez,[])
    end

(***************************************************************)
(* prestej pozitivne elemente *)
fun prestejpoz sez =
    case sez of
	[] => 0
      | g::rep => if g>=0 
		 then 1+ prestejpoz rep
		 else prestejpoz rep

fun prestejpoz_repna sez =
    let
	fun pomozna (sez, acc) =
	    case sez of
		[] => acc
	      | g::rep => if g>=0
			  then pomozna(rep, acc+1)
			  else pomozna(rep, acc)
    in
	pomozna(sez,0)
    end
    

