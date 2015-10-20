fun obseg a b = 
	if a > b then [] 
	else a::obseg (a+1) b

(* delna aplikacija funcije obseg *)
val stejNavzgor = obseg 1

val obrniNiz = implode o rev o explode 

fun enDva sez =
	let fun has1 s1 =
		case s1 of
			[] => true
			| 1::t => has2 t
			| _ => false
		and has2 s2 =
			case s2 of
				[] => true
				| 2::t => has1 t
				| _ => false
	in has1 sez end

fun stevec () = 
	let
		val st = ref 0
		fun naslednji () = (st:= (!st + 1); !st)
		fun ponastavi () = (st:=0)
		val vrednost = !st
	in
		{ naslednji = naslednji, ponastavi = ponastavi, vrednost = vrednost}
	end

(********************* M_STACK ********************)

signature M_STACK = 
	sig
	    type 'a mstack
	    val new : 'a -> 'a mstack
	    val push : 'a mstack * 'a -> unit
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
	end;

(* test:
	val s = mstack.new 2;
	mstack.push(s,2);
	mstack.pop(s)
*)

(************************ pcl *********************)
datatype 'a pcl = Pcl of 'a pcell ref
and 'a pcell = Nil | Cons of 'a * 'a pcl;

fun cons (h, t) =
	Pcl (ref (Cons (h,t)))

fun nill () = Pcl (ref Nil)

fun car (Pcl (ref (Cons (h,_)))) = h
fun cdr (Pcl (ref (Cons (_,t)))) = t

(*fun stl (Pcl (r as ref (Cons (h, t))), u) = (r := Cons (h, u));

val nula = cons (0, nill ())
val nule = stl (nula, nula) *)

