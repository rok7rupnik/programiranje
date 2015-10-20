fun polozaj(n:int, sez: int list) =
    if null sez
    then NONE
    else
        if hd sez = n
        then SOME 1
        else
            let
                val x = polozaj(n, tl sez)
            in
                if isSome(x)
                then SOME(valOf(x) + 1)
                else NONE
            end;

(* 
    x = sqrt(n)
    x^2 = n
    (y + e)^2 = n
    y^2 + 2ey + e^2 = n
    e(2y + e) = n - y^2
    e = (n - y^2)/(2y - e)
    e << y
    e = (n - y^2)/(2y)

    y + e = y + (n - y^2)/(2y) = ... = 0.5 * (y + n/y)
*)
fun koren(n: real, eps: real) =
    if n < 0.0
    then NONE
    else
        let
            fun racunaj(y: real) =
                if abs((n - y*y) / (2.0*y)) < eps
                then y
                else racunaj(0.5 * (y + n/y))
        in
            SOME(racunaj(1.0))
        end;

(********************************* number ************************************************)
datatype number =
    Int of int
    | Real of real;

fun sestej(a: number, b: number) =
    case (a, b) of
        (Int ai, Int bi)      => Int(ai + bi)
        | (Real ai, Real bi)    => Real(ai + bi)
        | (Int ai, Real bi)     => Real(Real.fromInt(ai) + bi)
        | (Real ai, Int bi)     => Real(ai + Real.fromInt(bi));

fun toString(n: number) =
    case n of
        Int x  => Int.toString x
        |Real x => Real.toString x;

(************************************* BSTrees = BSForest? ******************************************)

datatype bstree = Nil
    | Node of {key:int, left: bstree, right: bstree};

fun vstavi(n:int, tree:bstree): bstree =
    case tree of
        Nil                            => Node {key = n, left = Nil, right = Nil}
        |Node {key=k, left=l, right=r} => case Int.compare(n, k) of
                                                EQUAL    => tree
                                                |LESS    => Node {key=k, left=vstavi(n, l), right=r}
                                                |GREATER => Node {key=k, left=l, right=vstavi(n, r)};

(* 
    levo podrevo je urejeno, desno poddrevo je urejeno, 
    zato samo zdruzimo poVrsti od levo, k in poVrsti od desno 
*)
fun poVrsti(tree: bstree) =
    case tree of
        Nil                             => []
        | Node {key=k, left=l, right=r} => poVrsti(l) @ [k] @ poVrsti(r);

fun visina(tree: bstree) =
    case tree of
        Nil => 0
        | Node {key=k, left=l, right=r} => Int.max(visina(l), visina(r)) + 1;

fun jeBstree(tree: bstree) =
    let 
        fun veljavno(t: bstree, min: int, max: int) =
            case t of
                Nil                             => true
                | Node {key=k, left=l, right=r} => min < k andalso
                                                   k < max andalso
                                                   veljavno(l, min, k) andalso
                                                   veljavno(r, k, max)

    in 
        veljavno(tree, valOf(Int.minInt), valOf(Int.maxInt))
    end;

fun praznoDrevo(tree: bstree) =
    case tree of
        Nil       => true
        | Node nd => false;

fun stElementov(tree: bstree) =
    case tree of
        Nil       => 0
        | Node nd => 1 + stElementov(#left nd) + stElementov(#right nd);

fun stListov(tree: bstree) =
    case tree of
        Nil       => 0
        | Node nd => if praznoDrevo(#left nd) andalso praznoDrevo(#right nd)
                     then 1
                     else stListov(#left nd) + stListov(#right nd);

(* test bstree *)
(*
val t1 = Node {key=4, left=Nil, right=Node({key=6, left=Nil, right=Nil})};
val t2 = vstavi(3, t1);
val t2sez = poVrsti(t2);
val t3 = vstavi(2,t2);
val t3sez = poVrsti(t3);

visina(t2);
visina(t3);

val falseT = Node {key=4, left=Nil, right=Node({key=3, left=Nil, right=Nil})};
jeBstree(t3);
jeBstree(falseT);

praznoDrevo(t3);
praznoDrevo(Nil);

stElementov(t3);
stListov(t3);
*)

(* fun sestej *)
fun sestavi(sez : int list, vsota : int) =
    let 
        fun izracunajVsoto(sez : int list, vsota : int, rez : int list) =
            if vsota = 0
            then rez
            else if null sez
                 then []
                 else let 
                          val prviSeznam = izracunajVsoto(tl sez, vsota, rez)
                      in
                          if prviSeznam <> []
                          then prviSeznam
                          else izracunajVsoto(tl sez, vsota - hd sez, hd sez :: rez)
                      end
    in 
        let 
            val rez = izracunajVsoto(sez, vsota, [])
        in
            if null rez
            then NONE
            else SOME rez
        end
    end;

(* test sestavi *)
(*
sestavi([2,3,4,1], 6);
*)

(* fun casovniRazpon *)
type cas = (int * int * int)

fun casovniRazpon(c1 : cas, c2 : cas) =     
        let 
            val h1 = (#1 c1) val h2 = (#1 c2)
            val m1 = (#2 c1) val m2 = (#2 c2)
            val s1 = (#3 c1) val s2 = (#3 c2)

            fun odstejMinute(c1 : cas, c2 : cas) =
                let
                    val h1 = (#1 c1) val h2 = (#1 c2)
                    val m1 = (#2 c1) val m2 = (#2 c2)
                    val s1 = (#3 c1) val s2 = (#3 c2)
                in
                    if (m2 - m1) < 0
                    then ((h2 - (h1 + 1)) mod 24, (m2 - m1) mod 60, s1)
                    else ((h2 - h1) mod 24, (m2 - m1) mod 60, s1)
                end
           
        in 
            if (s2 - s1)  < 0
            then odstejMinute((s1, m1 + 1, (s2 - s1) mod 60), c2)
            else odstejMinute((s1, m1, (s2 - s1) mod 60), c2)
        end;



(* test casovniRazpon *)
(* razlicne relacije med urami, minutami in sekundami *)
(*casovniRazpon((0, 0, 0),(1, 1, 1));
casovniRazpon((0, 0, 1),(1, 1, 0));
casovniRazpon((0, 1, 0),(1, 0, 1));
casovniRazpon((0, 1, 1),(1, 0, 0));
casovniRazpon((1, 0, 0),(0, 1, 1));
casovniRazpon((1, 0, 1),(0, 1, 0));
casovniRazpon((1, 1, 0),(0, 0, 1));
casovniRazpon((1, 1, 1),(0, 0, 0));*)

(* se nekaj enakih polj *)
(*casovniRazpon((0, 0, 0),(0, 0, 1));
casovniRazpon((0, 0, 1),(0, 0, 0));
casovniRazpon((0, 0, 1),(1, 1, 0));*)

(* se nekaj bolj normalnih ur *)
(*casovniRazpon((8, 0  , 0),(7, 33, 33));
casovniRazpon((6, 35, 0),(7, 33, 33));
casovniRazpon((8, 35, 0),(7, 33, 33));
casovniRazpon((7, 35, 0),(7, 33, 33));*)

