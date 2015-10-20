(***************************************************************)
(******************* 1. VZAJEMNA REKURZIJA  ********************)
(***************************************************************)

(* PRIMER 1: sodost in lihost stevil *)
fun sodo x =
    if x=0
    then true
    else liho (x-1)
and liho x =
    if x=0
    then false
    else sodo (x-1)


(* PRIMER 2: rekurzija v podatkovnih tipih *)
datatype zaporedje1 = A of zaporedje2 | Konec1
     and zaporedje2 = B of zaporedje1 | Konec2

(* A (B (A (B (A Konec2)))); *)

(* ideja za konèni avtomat, ki sprejema nize oblike [1,2,1,2,...] *)




(***************************************************************)
(************************ 2. MODULI  ***************************)
(***************************************************************)

(* PRIMER 1: Modul za delo z nizi *)

structure Nizi =
struct

val prazni_niz = ""

fun dolzina niz =
    String.size niz

fun prvacrka niz =
    hd (String.explode niz)

fun povprecnadolzina seznam_nizov =
    Real.fromInt (foldl (fn (x,y) => (String.size x)+y) 0 seznam_nizov)
    /
    Real.fromInt (foldl (fn (_,y) => y+1) 0 seznam_nizov)
end



(* PRIMER 2: Modul za delo s polinomi *)

(* podpisi *)
signature PolinomP1 =
sig
    datatype polinom = Nicla | Pol of (int * int) list
    val novipolinom : int list -> polinom
    val mnozi : polinom -> int -> polinom
    val izpisi : polinom -> string
end

signature PolinomP2 =
sig
    type polinom
    val novipolinom : int list -> polinom
    val izpisi : polinom -> string
end

signature PolinomP3 =
sig
    type polinom
    val Nicla : polinom
    val novipolinom : int list -> polinom
    val izpisi : polinom -> string
end


(* modul *)
structure Polinom :> PolinomP3 =
struct

datatype polinom = Pol of (int * int) list | Nicla;

fun novipolinom koef = 
    let	fun novi koef stopnja =
	    case koef of
		[] => []
	      | g::r => if g<>0
			then (stopnja-1,g)::(novi r (stopnja-1))
			else (novi r (stopnja-1))
    in
	Pol (novi koef (List.length koef))
    end

fun mnozi pol konst =
    case pol of
	Pol koef => if konst = 0
		    then Nicla
		    else Pol (map (fn (st,x) => (st,konst*x)) koef)
      | Nicla => Nicla

fun izpisi pol =
    case pol of
	Pol koef => let val v_nize = (map (fn (st,x) => (if st=0 
							 then Int.toString(x) 
							 else Int.toString(x) ^ "x^" ^ Int.toString(st))) koef)
		    in foldl (fn (x,acc) => (acc ^ " + " ^ x))
			     (hd v_nize)
			     (tl v_nize)
		    end
      | Nicla =>  "0"

end


(*
- Polinom.mnozi (Polinom.novipolinom [7,6,0,0,0,4]) 2;
val it = Pol [(5,14),(4,12),(0,8)] : Polinom.polinom
-  Polinom.mnozi (Polinom.novipolinom [7,6,0,0,0,4]) 0;
val it = Nicla : Polinom.polinom
- Polinom.mnozi (Polinom.Nicla) 3;
val it = Nicla : Polinom.polinom
- Polinom.izpisi (Polinom.mnozi (Polinom.novipolinom [7,6,0,0,0,4]) 2);
val it = "14x^5 + 12x^4 + 8" : string
*)

(*
(* uporabnik kr¹i pravila uporabe *)
- Polinom.izpisi (Polinom.Pol [(3,1),(1,2),(16,0),(~5,3)]);
val it = "1x^3 + 2x^1 + 0x^16 + 3x^~5" : string
*)

