( 1, (2, 3, 4), (4, "pro") );

[];

1::[];

2::1::[];

hd [1, 2, 3];

tl [1, 2, 3];

fun newnull e=
    if e=[] then true else false;

(* nek kao datatype student s podatki:
    vpisna, ime, datum rojstva (dan, mesec, leto) in seznam imen predmetov z ocenami *)
(* student : (int * string * (int * int * int)) * (string * int) list; *)

fun ime_studenta (student : (int * string * (int * int * int)) * (string * int) list): string =
    #2 (#1 student);

fun mesec_leto (student : (int * string * (int * int * int)) * (string * int) list): int * int =
    let val datum = #3 (#1 student)
    in (#2 datum, #3 datum)
    end;

fun predmetnik (student : (int * string * (int * int * int)) * (string * int) list): string list =
    let val redovalnica = #2 student
    in 
        let 
            fun vrni_predmete (redovalnica : (string * int) list): string list =
                if null redovalnica
                then []
                else #1 (hd redovalnica) :: vrni_predmete(tl redovalnica)
        in
            vrni_predmete(redovalnica)
        end
    end;

