(*
	Denis Kotnik;
	FRI, Programiranje, 7. laboratorijske vaje
*)

(************************************************************************)

(* val obseg = fn : int -> int -> int list
Napišite funkcijo obseg, ki ji sprejme spodnjo in zgodnjo
	mejo intervala in vrne seznam vseh števil med in vključno z mejami.  *)
fun obseg a b =
	if a > b then []
	else a::obseg (a+1) b

(* val stejNavzdor = fn : int -> int list
Napiši funkcijo, ki sprejme zgornjo mejo in vrne števila med 1 in zgornjo mejo. *)
val stejNavzdor = obseg 1

(* val obrniNiz = fn : string -> string
Napišite funkcijo, ki zamenja vrstni red znakov v nizu. *)
val obrniNiz = implode o rev o explode
(*val obrniNiz str = implode(rev(explode(str))  -> to so 3 funkcije *)

(* val enDva = fn : int list -> bool
Napišite "končni avtomat", ki sprejema sezname oblike 1,2,1,2,1,2,1,2... *)
(* VZAJEMNA REKURZIJA *)
fun enDva l =
	let
		fun hasOne l =
			case l of
				[] => true
				| 1::x => hasTwo x
				| _ => false
		and
			hasTwo l =
			case l of
				[] => true
				| 2::x => hasOne x
				| _ => false
	in
		hasOne l
	end

(* Napišite funkcijo, ki sprejme enoto in vrne števec. 
	Števec je zapis dveh funkcij, naslednji in ponastavi. 
	naslednji je funkcija, ki jo pokličemo z enoto, 
	vrne pa naslednje število; ponastavi vrne števec na začetek. 

	val stevec = fn : unit -> {naslednji : unit -> int, ponastavi : unit -> unit} *)
(* () -> unit *)
(* MUTACIJA *)
fun stevec () = 
	let
		val c = ref 0
		fun next () = (c:= (!c + 1); !c)
		fun reset () = (c:=0)
		val vrniVrednost = !c
	in
		{ next = next, reset = reset, vrniVrednost = vrniVrednost}
	end
(* UPORABA:
	val a = stevec();
	#next a ();
	#next a();
	#reset a();
	#next a()
	#vrniVrednost a;
*)

(* Podan je podpis mutabilnega sklada. Implementirajte ga, npr. z referecno na seznam.  *)
signature M_STACK = 
sig
    type 'a mstack
    (* new ustvari nov sklad z elementom *)
    val new : 'a -> 'a mstack
    (* push(m,x) potisne x na sklad m *)
    val push : 'a mstack * 'a -> unit
    (* pop(m) vrne vrh sklada m. *)
    val pop : 'a mstack -> 'a option
end

structure mstack :> M_STACK =
	struct
		type 'a mstack = 'a list ref
		val new = fn x => ref [x]
		val push = fn (stack, x) => stack := x::(!stack)
		val pop = fn stack =>
					case !stack of
						[] => NONE
						| x::xs => (stack := xs; SOME x)
	end
(* UPORABA:
	val s = mstack.new 1;  -> it SOME 2, SOME 1; NONE
	mstack.push(s,2);
	mstack.pop()
*)

(* Podan je podatkovni tip pcl: *)
datatype 'a pcl = Pcl of 'a pcell ref
and 'a pcell = Nil | Cons of 'a * 'a pcl;

(* val cons = fn : 'a * 'a pcl -> 'a pcl
Napišite funkcijo cons, ki vrne tip pcl, ki vsebuje Cons (h, t) *)
fun cons (h, t) =
	Pcl (ref (Cons (h,t)))

(* val nill = fn : unit -> 'a pcl
Napišite funkcijo nill, ki predstavlja prazen pcl *)
fun nill () = Pcl (ref Nil) 

(* val car = fn : 'a pcl -> 'a
val cdr = fn : 'a pcl -> 'a pcl
Napišite funkciji car in cdr ki so ekvivalentne hd in tl, le da delujeta nad pcl. *)
fun car (Pcl (ref (Cons (h,_)))) = h
fun cdr (Pcl (ref (Cons (_,t)))) = t

(* Imamo funkcijo fun stl (Pcl (r as ref (Cons (h, t))), u) = (r := Cons (h, u));
Z njo ustvarite neskončen seznam enic in dvojk! *)




