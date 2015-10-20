fun obstaja (f, list) =
    case list of
        []        => false
        | x :: xs => f(x) orelse obstaja(f, xs)

fun zaVse (f, list) =
    case list of
        []        => true
        | x :: xs => f(x) andalso zaVse(f, xs)

fun clan (element, list) =
    obstaja (fn (x) => element = x, list)

fun vstavi (new, list) =
    if clan (new, list)
    then list
    else new :: list

fun jePodmnozica (list1, list2) =
    zaVse (fn el => clan (el, list1), list2) 

fun jePodmnozica (list1, list2) = 
    zaVse (fn x => obstaja (fn el => el = x, list1), list2)

fun staLoceni (list1, list2) =
    zaVse (fn x => not (clan (x, list2)), list1)

fun staLoceni (list1, list2) =
    zaVse (fn x => zaVse (fn y => x <> y, list2) , list1)

fun map (f, sez) =
    case sez of
        []       => []
        | h :: t => (f h) :: map (f, t)

fun filter (f, sez) =
    case sez of 
        []       => []
        | h :: t => if f(h)
                    then h :: filter (f, t)
                    else filter (f, t)

fun presek (list1, list2) =
    filter (fn x => clan(x, list1), list2)

fun razlika (list1, list2) =
    filter (fn x => not (clan(x, list2)), list1)

fun zamenjaj (oldelement, newelement, list) =
    map (fn a => if a = oldelement then newelement else a, list)

fun kartprod(list1, list2) =
    map (fn x => map (fn y => (x,y), list1), list2)

fun unija (list1, list2) =
    list1 @ razlika (list2, list1)

fun odvod (list) =
    filter ( fn (a,b) => b > ~1 andalso a <> 0, map (fn (x, y) => ((x * y), (y - 1)), list))