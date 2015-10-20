fun gcd(x:int, y:int):int =
    if y = 0
    then x
    else gcd(y, x mod y);

 fun sestej(a: int * int, b: int * int): int * int =
    let 
        val imenovalec_a = #2 a 
        val stevec_a = #1 a
        val imenovalec_b = #2 b
        val stevec_b = #1 b

        fun okrajsaj(ulomek: int * int): int * int =
            let val nsd = gcd ulomek
            in
                (#1 ulomek div nsd, #2 ulomek div nsd)
            end
    in
        okrajsaj(stevec_a*imenovalec_b + stevec_b*imenovalec_a, imenovalec_a*imenovalec_b)
    end;

fun vsebuje(el: int, sez: int list): bool =
    if null sez
    then false
    else el = hd sez orelse vsebuje(el, tl sez);

fun zadnji(sez: int list): int =
    if null (tl sez)
    then hd sez
    else zadnji (tl sez);

fun dolzina(sez: int list): int =
    if null sez
    then 0
    else 1 + dolzina(tl sez);

fun vrni_ntega(sez: string list, n: int): string =
    if n = 1
    then hd sez
    else vrni_ntega(tl sez, n - 1);

fun obrni(sez: int list): int list =
    if null sez
    then []
    else obrni(tl sez) @ [hd sez];

fun brisi(el: int, sez: int list): int list =
    if null sez then []
    else if el = hd sez
         then brisi(el, tl sez)
         else hd sez :: brisi(el, tl sez);

fun vsota(s: real list, p: real list): real list =
    if null p
    then s
    else if null s
         then p
         else hd s + hd p :: vsota(tl s, tl p);

fun jePalindrom(sez: int list): bool =
    null sez orelse let
                        val zes = obrni sez

                        fun enaka(s1: int list, s2: int list): bool=
                            null s1 orelse hd s1 = hd s2 andalso enaka(tl s1, tl s2)
                    in
                        enaka(sez, zes)
                    end;

fun zdruzi(s1: int list, s2: int list): int list =
    if null s1
    then s2
    else if null s2
         then s1
         else if hd s1 > hd s2
              then hd s2 :: zdruzi(s1, tl s2)
              else hd s1 :: zdruzi(tl s1, s2);
