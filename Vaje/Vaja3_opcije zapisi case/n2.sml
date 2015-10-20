fun position ( element : int, elements : int list ) = 
    if null elements
    then NONE
    else if hd elements = element
        then SOME 1
        else let 
            val elp=position ( element, tl elements )
        in
            if isSome ( elp ) 
            then SOME ( valOf ( elp ) + 1 )
            else NONE
        end

fun sqrt ( x : real, eps : real ) =
    if x < 0.0
    then NONE
    else let
        fun iterate ( y : real ) =
            if ( 0.5 * ( x / y - y ) ) < eps andalso ( 0.5 * ( x / y - y ) ) > ~eps
            then y
            else iterate ( 0.5 * ( y + x / y ) )
    in
        SOME ( iterate ( 1.0 ) )
    end

datatype number = 
    Int of int
    | Real of real

fun toString (n : number) =
case n of
    Int x => Int.toString (x)
    | Real x => Real.toString (x)

fun sum (x : number, y : number) = 
    case (x, y) of
        (Int a, Int b) => Int (a + b)
        | (Real a, Real b) => Real (a + b)
        | (Int a, Real b) => Real (Real.fromInt(a) + b)
        | (Real a, Int b) => Real (Real.fromInt(b) + a)

datatype  bstree = Nil 
    | Node of { key : int, left : bstree, right : bstree }

fun insert ( x : int, t : bstree ) : bstree =
    case t of
        Nil => Node {key = x, left = Nil, right = Nil}
        | Node {key = y, left = l, right = r} => case Int.compare (x, y) of
            GREATER => Node {key = y, left = l, right = insert (x, r)}
            | LESS => Node {key = y, left = insert (x,l), right = r}
            | EQUAL => t

fun height (t : bstree) =
    case t of
        Nil => 0
        | Node {key = k, left = l, right = r} => 1 + Int.max(height (l), height (r))

fun inOrder (tree : bstree) =
    case tree of
        Nil => []
        | Node {key = k, left = l, right = r} => inOrder(l) @ [k] @ inOrder(r)

fun jeBST (tree : bstree) =
    let 
        fun jeUrejen (b : int list, a : bool) =
            if a = false
            then false
            else 
                if null (tl b)
                then a
                else jeUrejen(tl b, hd b <= hd (tl b))
    in jeUrejen(inOrder(tree), true)
    end;

fun praznoDrevo (tree : bstree) =
    case tree of
        Nil => true
        | _ => false


fun dolzina (a : int list) = 
    if null a then 0
    else 1+length(tl a)

fun stElementov (tree : bstree) =
    dolzina(inOrder(tree))

fun stListov (tree : bstree) =
    case tree of
        Nil => 0
        | Node {key = k, left = l, right = r} => if (l = Nil andalso r = Nil)
                                                then 1 
                                                else stListov(l)  + stListov(r)

type time = (int * int * int)

fun casovniRazpon (a : time, b : time ) =     
        let fun urediMin(a : time, b : time) =            
            if (#2 b - #2 a) < 0
            then ((#1 b - (#1 a + 1)) mod 24, (#2 b - #2 a) mod 60, #3 a)
            else ((#1 b - #1 a) mod 24, (#2 b - #2 a) mod 60, #3 a)           
        in 
            if (#3 b - #3 a)  < 0
            then urediMin((#1 a, #2 a + 1, (#3 b - #3 a) mod 60), b)
            else urediMin((#1 a, #2 a, (#3 b - #3 a) mod 60), b)
        end

fun dolzina (a : int list) = 
    if null a then 0
    else 1+length(tl a)

fun sestavi (a : int, b  : int list) = 
    let fun sestej (l, sum, res) =
        if sum = 0
        then res
        else if (null l)
             then []
             else sestej(tl l, sum, res) @ sestej (tl l, sum - hd l, hd l :: res)
    in let val res = sestej(b, a, [])
        in
        if  null res then NONE
        else SOME res
       end
    end


