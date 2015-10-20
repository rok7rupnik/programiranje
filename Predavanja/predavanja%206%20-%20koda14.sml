(***************************************************************)
(*********************** 1. CURRYING ***************************)
(***************************************************************)

fun vmejah_terka (min,max,sez) = 
    filter(fn x => x>=min andalso x<=max, sez)


fun vmejah_curry min =    (* razlièica, ki uporablja currying *)
    fn max => 
       fn sez =>
	  filter(fn x => x>=min andalso x<=max, sez)

(* sintaktiène olepšave *)
fun vmejah_lepse min max sez =
    filter(fn x => x>=min andalso x<=max, sez)
(*
vmejah_lepse 5 15 [1,5,3,43,12,3,4];   
vmejah_curry 5 15 [1,5,3,43,12,3,4];
*)


(***************************************************************)
(******************** 2. DELNA APLIKACIJA **********************)
(***************************************************************)

(* PRIMER 1: vrne samo števila od 1 do 10 *)
val prva_desetica = vmejah_curry 1 10;

(* PRIMER 2: obrne vrstni red argumentov *)
fun vmejah2 sez min max = vmejah_lepse min max sez;
(* doloèi zgornjo mejo fiksnega seznama *)
val zgornja_meja = vmejah2 [1,5,2,6,3,7,4,8,5,9] 1;

(* PRIMER 3. primeri z uporabo map/filter/foldl *)
val povecaj = List.map (fn x => x + 1);
val samoPozitivni = List.filter (fn x => x > 0);
val vsiPozitivni = List.foldl (fn (x,y) => y andalso (x>0)) true;  (* pozor, vrstni red arg v fn! *)



(***************************************************************)
(************************ 3. MUTACIJA **************************)
(***************************************************************)

fun zdruzi_sez sez1 sez2 =
    case sez1 of
	[] => sez2
      | g::rep => g::(zdruzi_sez rep sez2)

val s1 = [1,2,3]
val s2 = [4,5]
val rezultat = zdruzi_sez s1 s2

(*
- val x = ref 15;
val x = ref 15 : int ref
- val y = ref 2;
val y = ref 2 : int ref
- (!x)+(!y);
val it = 17 : int
- x:=7;
val it = () : unit
- (!x)+(!y);
val it = 9 : int
*)

(* PRIMER: neprièakovani uèinek *)

val x = ref "zivjo";
val y = ref 2013;
val z = (x, y)
val _ = x:="kuku"
val w = (x,y)


(* PRIMER: uporaba mutacije *)
val zgodovina = ref ["zacetek"];
val sez = [1,2,3]

fun pripni element  =
    (zgodovina:= (!zgodovina) @ ["pripet " ^ Int.toString(element)]
    ;
      sez@[element])

fun brez_zadnjega () =
    let fun brez s =
	    case s of
		[] => []
	      | [el] => (zgodovina:= (!zgodovina) @ ["odstranjen zadnji " ^ Int.toString(el)]
			; [])
	      | g::rep => g::(brez rep)
    in brez sez
    end


(*
- zgodovina;
val it = ref ["zacetek"] : string list ref
- pripni 3;
val it = [1,2,3,3] : int list
- brez_zadnjega ();
val it = [1,2] : int list
- zgodovina;
val it = ref ["zacetek","pripet 3","odstranjen zadnji 3"] : string list ref
*)



(***************************************************************)
(******************* 4. DOLOÈANJE TIPOV  ***********************)
(***************************************************************)

(* PRIMER 1 *)
fun fakt x =                (* 1.   fakt: 'a -> 'b *)   (* 3.  fakt : int -> __ *)  (* 6.  fakt: int -> int *)
    if x = 0                (* 2.   x: 'a; 'a = int, zato da primerjava z 0 uspe *)
    then 1                  (* 4.   rezultat funkcije je 'b = int *)
    else x*(fakt (x-1))     (* 5.   mora biti skladno s 4; x: int, (fakt x): int, 'b = int *)


(* PRIMER 2 *)
fun f (q, w, e) =           (* 1.  f: 'a * 'b * 'c -> 'd *)   
                            (* 3.  f: ('f * 'g) list * 'b * 'c -> 'd *)
                            (* 5.  f: ('f * 'g) list * bool list * 'c -> 'd *)
                            (* 8.  f: ('f * int) list * bool list * 'c -> int *)
    let val (x,y) = hd(q)   (* 2.  'a = 'e list;  'e = ('f * 'g);  'a = ('f * 'g) list *)
    in if hd w              (* 4.  'b = 'h list;  'h = bool;  'b = bool list *)
       then y mod 2         (* 6.  y: int; 'd = int *)
       else y*y	            (* 7.  skladno s 6 velja y: int; 'd = int *)
    end


(* PRIMER 3 *)
(* fun compose (f,g) = fn x => f (g x)   *)
(* val koren_abs = compose (Math.sqrt, abs);
      je enakovredno kot
   val koren_abs2 = Math.sqrt o abs;  *)

fun compose1 (f,g) =   (* 1.  f: 'a -> 'b;  g: 'c -> 'd *; 
                              compose: ('a -> 'b) * ('c -> 'd) -> 'e                         *)
                       (* 6.  compose: ('a -> 'b) * ('c -> 'a) -> ('c -> 'b)                 *)
    fn x => f (g x)    (* 2.  x: 'c, 'e: 'c -> NEKAJ                                         *)
                       (* 3.                   g: 'c -> 'd;  g x: 'd                         *)
                       (* 4.                   f: 'a -> 'b;  f (g x) = 'b  --> velja 'd=='a! *)
                       (* 5.         'e: 'c -> 'b                                            *)




(***************************************************************)
(****************** 5. OMEJITEV VREDNOSTI  *********************)
(***************************************************************)

(*
val sez = ref [];     (* sez je tipa 'a list ref *)
sez := !sez @ [5];    (* v seznam dodamo int *)
sez := !sez @ true;   (* pokvari pravilnost tipa seznama! *)
*)


(*
val sez = ref [];                  (* NE DELUJE: omejitev vrednosti *)
val xx = ref NONE;                 (* NE DELUJE: omejitev vrednosti *)

val xxx = ref []: int list ref;    (* RE©ITEV 1: opredelimo podatkovne tipe *)

val mojaf = map (fn x => x+1);     (* ni polimorfna, deluje *)
val mojaf1 = map (fn x => 1);      (* te¾ava: polimorfizem + klic funkcije map *)
fun mojaf2 sez = map (fn x => 1) sez     (* RE©ITEV 2: ovijemo vrednost v funkcijo *)

- mojaf [1,2,3]
- mojaf1 [1,2,3]
*)

