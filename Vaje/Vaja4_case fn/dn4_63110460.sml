(* zapiski - ponovitev
	exception Cons
		Cons_2 of Type
		  
(raise Cons_2 r) handle p1 =>
						| p2 => 
						| Cons_2 x => x *)
						
fun sin (x, eps) =
	let fun racunaj (prejsnji, i, acc) =
		let 
			val naslednji = ((~1.0) * prejsnji * (x*x)) / (Real.fromInt(i+1) * Real.fromInt(i+2))
		in 
			if prejsnji < eps
			then acc
			else racunaj(naslednji, i+1, acc+prejsnji)
		end
	in racunaj(x, 1, 0.0)
	end
			
			
fun fib n =
   let fun fib1 (n1, n2, i) =
       if i = n
       then n1 + n2
       else fib1(n2, n1 + n2, i + 1)
   in if n < 1
       then 0
       else fib1(1, 0, 1)
   end
   
exception NapacnaDolzina
   
fun sestavi3 terka =
	case terka of
		([], [], []) 			   => []
		| (h1::t1, h2::t2, h3::t3) => (h1, h2, h3)::sestavi3 (t1, t2, t3)
		|_ 						   => raise NapacnaDolzina;
		
(* Racunanje tipov
	sestavi3: T1 -> T2
	terka: T3; T1 = T3
	T3 = T4 list * T5 list * T6 list
	[] -> T2 = T7 list
	h1: T8, h2:T9, h3:T10
	t1: T8 list, t2: T9 list, t3: T10 list
	----- iz tega sledi:
	T8 = T4, T9 = T5, T10 = T6
	
	(T4 list * T5 list * T6 list) -> (T4 * T5 * T6) list
	('a list * 'b list * 'c list) -> ('a * 'b * 'c) list
*)

fun razstavi3 sez =
	let fun razstavljaj (sez, s1, s2, s3) =
		case sez of
			[] 			=> razstavljaj ([], rev s1, rev s2, rev s3)
			|(a,b,c)::t => razstavljaj (t, a::s1, b::s2, c::s3)
	in razstavljaj (sez, [], [], [])
	end
	
fun naLihih (f, sez) =
	case sez of
		[] 			=> []
		| h::[] 	=> [f h]
		| h1::h2::t => (f h1)::naLihih (f, t);
		
fun veljaNaVseh (f, sez) =
	case sez of
		[] 	   => true
		| a::t => (f a) andalso (veljaNaVseh (f, t))
		
val vsiPozitivni = fn sez =>
	veljaNaVseh (fn x => x > 0, sez)
	
val vsiLihi = fn sez =>
	veljaNaVseh (fn x => x mod 2 = 1, sez)
	
fun map (f, sez) =
	case sez of
		[] 	   => []
		| h::t => f h::map (f, t)
		
fun filter (f, sez) =
	case sez of
		[] 	   => []
		| h::t => if f h
				  then h::filter (f, t)
				  else filter (f, t)
				  
fun fold (f, init, sez) =
	case sez of
		[] 	   => init
		| h::t => fold (f, f(init, h), t)
		
val preslikaj = fn sez =>
	map (fn sez1 =>
			fold (fn (x, sum) => x + sum, 0, sez1),
		sez);
		