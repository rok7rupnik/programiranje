(*ダレ・

  _____                                      _                      _                       _                   
 |  __ \                                    (_)                    | |                     | |                  
 | |__) | ____   ____ _   ___  ___ _ __ ___  _ _ __   __ _ _ __ ___| | ____ _   _ __   __ _| | ___   __ _  __ _ 
 |  ___/ '__\ \ / / _` | / __|/ _ \ '_ ` _ \| | '_ \ / _` | '__/ __| |/ / _` | | '_ \ / _` | |/ _ \ / _` |/ _` |
 | |   | |   \ V / (_| | \__ \  __/ | | | | | | | | | (_| | |  \__ \   < (_| | | | | | (_| | | (_) | (_| | (_| |
 |_|   |_|    \_/ \__,_| |___/\___|_| |_| |_|_|_| |_|\__,_|_|  |___/_|\_\__,_| |_| |_|\__,_|_|\___/ \__, |\__,_|
                                                                                                     __/ | 
                                                                                    (__)            |___/  
                                                                                    (oo)                      
                                                pri predmetu Programiranje           \/-------\              
                                                                                      ||     | \     
                                                                                      ||----||  *     
                                                                                      ^^    ^^         

    Seminarska naloga je sestavljena iz dveh delov. V prvem delu bomo implementirali izjavni račun, v drugem
delu pa se bomo ukvarjali z ujemanjem vzorcev in računanjem tipov našega miniML jezika.
    Seminarske naloge bodo avtomatsko ocenjevane, zato obstoječe kode (tipov, podatkovnih struktur, signatur)
ne spreminjajte. Na vas je, da implementirate manjkajoče funkcije znotraj definicij struktur, tj. popravite in
dopišete tiste funkcije, ki trenutno zgolj prožijo izjemo NiImplementirano. Prvi del naloge je vreden 40 točk,
drugi 50, preostalih 10 točk pa bo ocena sintakse (pravilna raba nevidnih znakov, izraznost, komentarji). 
Potrudite se napisati kratke, elegantne in berljive rešitve. Vzorčna rešitev potrebuje 35 vrstic kode za prvi 
del in okoli 80 vrstic za drugi.


  ▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄ *)


exception NiImplementirano;

type spremenljivka = string
datatype izjava = S of spremenljivka  (* izjavne spremenljivke *)
                | DA | NE                 (* izjavne konstante *)
                | Neg of izjava           (* negacija *)
                | In  of izjava * izjava  (* konjunkcija *)
                | Ali of izjava * izjava  (* disjunkcija *)
                | Imp of izjava * izjava  (* implikacija *)

infix  7  In  (* In veže močneje kot Ali          *)
infix  6  Ali (* Ali veže močneje kot Implikacija *)
infixr 5  Imp (* Implikacija je desno asociativna *)
(* Primer veljavne izjave: DA Imp S "z" Ali Neg(S "p") In S "c" *)


signature IZJAVNA_SEMANTIKA =
sig
    type interpretacija
    exception DomenaInterpretacije of string

    val ustvariInterpretacijo : (spremenljivka * bool) list -> interpretacija
    val apliciraj : interpretacija * spremenljivka -> bool
    val evalviraj : izjava * interpretacija -> bool
    val spremenljivkeIzjave : izjava -> spremenljivka list
    val vseInterpretacije : spremenljivka list -> interpretacija list
    val jeTavtologija : izjava -> bool
    val jeIzpolnjiva  : izjava -> bool
    val jeProtislovje : izjava -> bool
    val izpolnjujejo  : izjava ->(*interpretacija list*)(spremenljivka * bool) list list (* da vam izpiše vsebino*)
end


structure Izjave :> IZJAVNA_SEMANTIKA = 
struct
    type interpretacija = (spremenljivka * bool) list
    (* interpretacija je seznam spremenljivk in njihovih vrednosti *)
    exception DomenaInterpretacije of string
    (* sproži jo funkcija apliciraj, če iskane spremenljivke ni v interpretaciji *)

    (* Seznam parov spremenljivka * bool pretvorimo v notranjo predstavitev interpretacije.
     * V notranji predstavitvi privzemamo, da so spremenljivke v interpretaciji leksikografsko urejene. *)
    fun ustvariInterpretacijo sez =
        let fun primerjaj ((spr1, _), (spr2, _)) = String.compare (spr1, spr2)
        in ListMergeSort.uniqueSort primerjaj sez
        end

    (* Funkcija poišče vrednost iskane spremenljivke znotraj podane interpretacije. Če spremenljivke ni v 
     * interpretaciji, se sproži izjema DomenaInterpretacije. Morda pride prav funkcija String.compare *)
    fun apliciraj (interpretacija, spremenljivka) =
        let fun poisci sez spr = 
            case sez of
                []                       => raise DomenaInterpretacije spr
                | (sprG, vrednostG)::rep => 
                    case String.compare (sprG, spr) of
                        EQUAL     => vrednostG
                        | LESS    => poisci rep spr
                        | GREATER => raise DomenaInterpretacije spr
                        (* Ker je intrepretacija leksikografsko urejena, pomeni, da spremenljivke ni v interpretaciji, 
                         * ce je spr "vecja" od spr v glavi  *)
        in poisci interpretacija spremenljivka
        end
    
    (* Evalviraj izračuna vrednost izjave, ali je resnična ali ne, na podlagi podane interpretacije.
     * Ni nič posebnega, v resnici gre za en stavek case. 
     * ---------------------------------------------------------------------------------------------
     * Za vsak mozen tip izjave definiramo kako, ga evalviramo *)
    fun evalviraj (izjava, interpretacija) =
        let fun eval izjavaTmp =
            case izjavaTmp of
                S spr           => apliciraj (interpretacija, spr)
                | DA            => true
                | NE            => false
                | Neg izj       => not (eval izj)
                | izj1 In  izj2 => (eval izj1) andalso (eval izj2)
                | izj1 Ali izj2 => (eval izj1) orelse (eval izj2)
                | izj1 Imp izj2 => not (eval izj1) orelse (eval izj2)
        in eval izjava
        end

    
    (* spremenljivkeIzjave vrne seznam vseh različnih spremenljivk v podanem izrazu.
     * Ponovno gre v resnici za en case stavek, ki za spremenljivke vrača neprazen seznam. 
     * -----------------------------------------------------------------------------------
     * Ko pridemo do S vrnemo spremenljivko
     * ce pridemo do DA | NE ne vrnemo nicesar, 
     * drugace ustrezno zdruzujemo rekurzivne klice *)
    fun spremenljivkeIzjave izjava =
        let fun najdiSpremenljivke izjavaTmp =
            case izjavaTmp of
                S spr           => [spr]
                | DA            => [] 
                | NE            => []
                | Neg izj       => najdiSpremenljivke izj
                | izj1 In  izj2 => (najdiSpremenljivke izj1) @ (najdiSpremenljivke izj2)
                | izj1 Ali izj2 => (najdiSpremenljivke izj1) @ (najdiSpremenljivke izj2)
                | izj1 Imp izj2 => (najdiSpremenljivke izj1) @ (najdiSpremenljivke izj2)
        in ListMergeSort.uniqueSort String.compare (najdiSpremenljivke izjava)
        end

    (* Funkcija pretvori seznam imen spremenljivk v seznam, ki vsebuje
        vse možne interpretacije podanih spremenljivk. 
     * ----------------------------------------------------------------
     * prvi spremenljivki damo true in false
     * pri naslednjih spremenljivkah podvojimo nabor in mu dodamo true and false naslednje spr *)
    fun vseInterpretacije spremenljivke =
        case spremenljivke of
            []             => []
            | [spr]        => [[(spr, true)], [(spr, false)]]
            | sprG::sprR   =>
                map 
                    (fn a => ( sprG, true) :: a) 
                    (vseInterpretacije sprR) 
                @ map 
                    (fn a => ( sprG, false) :: a) 
                    (vseInterpretacije sprR)

    (* Funkcija preveri, ali je izjava tavtologija. 
     * -----------------------------------------------------------------
     * Najdemo vse spremenljivke,
     * za njih najdemo za njih vse interpretacije,
     * nato gremo cez vse interpretacije s funkcijo foldl in za vsako evalviramo izjavo.
     * Ce imamo v vseh primerih true, potem je izjava tavtologija *)
    fun jeTavtologija izjava =
        let val intSprIzjave = vseInterpretacije (spremenljivkeIzjave izjava)
        in List.foldl (fn (interpretacija, jeTavt) => jeTavt andalso evalviraj (izjava, interpretacija)) true intSprIzjave
        end
    (* val tavtologija = Neg (S "a") Ali S "a"; *)

    (* Funkcija preveri, ali je izjavi možno zadostiti. 
     * --------------------------------------------------- --------------------------
     * Najdemo vse interpretacije in za vsaj eno od njih se mora izjava evalvirati v true
     * Gremo cez vse interpretacije s foldl, podobno kot pri je tavtologija*)
    fun jeIzpolnjiva izjava =
        let val intSprIzjave = vseInterpretacije (spremenljivkeIzjave izjava)
        in List.foldl (fn (interpretacija, jeIzp) => jeIzp orelse evalviraj (izjava, interpretacija)) false intSprIzjave
        end
    (* val izpolnjiva = S "a" Imp S "z" Ali Neg(S "p") In S "c" In DA Ali NE *)

    (* Funkcija preveri, ali izjavi ni možno zadostiti. 
     * --------------------------------------------------
     * Izjava ni izpolnjiva <=> izjava je protislovje *)
    fun jeProtislovje izjava = not(jeIzpolnjiva izjava)
    (* val protislovje = Neg(S "a") Ali S "a" Imp NE; *)

    (* Funkcija vrne seznam vseh interpretacij, pri katerih je izjava resnična. Torej če je izraz
     * protislovje, funkcija vrne prazen seznam; če je tavtologija, pa kar množico vseInterpretacije 
     * ----------------------------------------------------------------------------------------------
     * Ustvarimo vse interpretacije, 
     * gremo cez njih in vsako, ki izpolnjuje izjavo, dodamo v akumulator funkcije fold *)
    fun izpolnjujejo izjava =
        let val intSprIzjave = vseInterpretacije (spremenljivkeIzjave izjava)
        in List.foldl 
                (fn (interpretacija, praveInt) => 
                    if evalviraj(izjava, interpretacija) 
                    then interpretacija::praveInt 
                    else praveInt)
                [] 
                intSprIzjave
        end

end (* structure Izjave *)

(*val moo = Izjave.ustvariInterpretacijo ([("moo", true), ("boo", false)]);
val boo = Izjave.apliciraj (moo, "boo") = false;
val t01 = Izjave.evalviraj (Neg(S"moo")Ali S"boo", moo) = false;
val t02 = Izjave.spremenljivkeIzjave(Neg(S"foo" Ali S"moo")Ali S"moo") = ["foo", "moo"];
val t03 = Izjave.jeTavtologija(Neg(S"a")Ali S"a") = true;
val t04 = Izjave.jeIzpolnjiva(DA Imp S"z" Ali Neg(S"p") In S"c") = true;
val t05 = Izjave.jeProtislovje(S"moo" Ali Neg(S"moo") Imp NE) = true;
val t06 = Izjave.izpolnjujejo(Neg(S"a")Ali S"a") = [[("a",false)],[("a",true)]]; *)

(*         _.====.._                         
   _     ,:._       ~-_      _                      
.,_)`'-._ ~  `\ `_     ~_  ~ )`'-.,_ .    ~\-,_     ,.
    _  ) `-.,_`|    \    v.    ~   _'      -.    _      ~~-.
\-,_)`'  )`  ,/  `_    \   `~-_   _)`-   ,.      )`'-.          
    -..__..-ダ'   _      `_    レ~ ~ -.--..._____........_____________.....................   Drugi del   ・*)


datatype vrednost = Konst of int
                  | Enota
                  | Terka of vrednost list
                  | Konstruktor of string * vrednost

datatype vzorec = Poljubno  (* vzorec se priredi čemurkoli, poznamo ga kot _ *)
                | Spr of spremenljivka  (* spremenljivke *)
                | KonstV of int  (* predstavitev števil *)
                | EnotaV
                | TerkaV of vzorec list (* elemente terke hranimo v seznamu *)
                | KonstruktorV of string * vzorec

datatype tip = Karkoli (* poljuben tip bo v redu *)
             | EnotaT (* tip za Enota *)
             | IntT (* tip za cela števila *)
             | TerkaT of tip list (* terka je neprazen seznam tipov *)
             | PodatkovniTip of string (* poimenovan podatkovni tip *)


type zalogaPodatkovnihTipov = (string * string * tip) list 

signature VZORCI_SEMANTIKA =
sig
    val veljavenVzorec : vzorec -> bool
    val priredi : vrednost * vzorec -> (spremenljivka * vrednost) list option
    val prvaPrireditev : vrednost * vzorec list -> (spremenljivka * vrednost) list option

    val podatkovniTip : string * tip * zalogaPodatkovnihTipov -> tip option
    val izracunajTip  : vzorec * zalogaPodatkovnihTipov -> tip option
    val prizanesljiv  : tip * tip -> tip option
    val izracunajTipVzorcev : vzorec list * zalogaPodatkovnihTipov -> tip option
end

structure Vzorci :> VZORCI_SEMANTIKA = 
struct

    exception NiOdgovora
    (* Pomožna funkcija, ki vam morda pride prav. Podana funkcija se po vrsti izvaja na elementih seznama, vse dokler
     * ne dobi SOME v, ko v postane rezultat, oz. ne pride do konca seznama, ko sproži izjemo NiOdgovora. *)
    fun prviOdgovor funkcija seznam =
        case seznam of
            [] => raise NiOdgovora
          | x::xs => case funkcija x of
                      NONE => prviOdgovor funkcija xs
                    | SOME y => y

    (* Pomožna funkcija, ki vam morda pride prav. Podana funkcija se izvede na vseh elementih seznama. Če funkcija
     * vrne NONE za kateri koli element, je rezultat NONE. Sicer je rezultat SOME seznam konkateniranih posameznih
     * odgovorov. *)
    fun vsiOdgovori funkcija seznam =
            let fun akumuliraj (acc, seznam) =
                case seznam of
                    [] => SOME acc
                  | x::xs => case funkcija x of
                                   NONE => NONE             
                                 | SOME vrednost => akumuliraj ((vrednost @ acc), xs)
            in akumuliraj ([], seznam)
            end

    (* Vzorec je veljaven če in samo če so v njem vse spremenljivke med seboj različne. Imena konstruktorjev niso
     * pomembna. Namig: vzorčna rešitev vsebuje dve pomožni funkciji. Prva pretvori vzorec v seznam spremenljivk, 
     * tu morda pride prav List.foldl. Druga pomožna funkcija sprejme seznam nizov in preveri za ponovitvami, 
     * uporablja List.exists . 
     * ----------------------------------------------------------------------------------------------------------
     * Funkcija vseSpremenljivke shrani spremenljivke v vzorcu v seznam.
     * Rekurzivni klici so pri konstruktorjih TerkaV, ki je seznam vzorcev 
     * in KonstruktorV, kjer je drugi element terke vzorec.
     * Pri Poljubno, KonstV in EnotaV vrnemo prazen seznam.
     * -------------------------------------------------------------------------------------------
     * V funkciji isUnique gremo cez vse spremenljivke in za vsako preverimo, 
     * ce obstaja v preostalem delu spremenljivk
     * Ce pridemo do konca, seznam ne vsebuje ponovitev *)
    fun veljavenVzorec vzorec =
        let fun vseSpremenljivke vzorec =
                case vzorec of
                    Spr spr                       => [spr]
                    | TerkaV sezVzorcev           => List.foldl 
                                                        (fn (vzorec, vseSpr) => vseSpr @ vseSpremenljivke vzorec) 
                                                        [] 
                                                        sezVzorcev
                    | KonstruktorV (_, vzorecTmp) => vseSpremenljivke (vzorecTmp)
                    | _                           => []
            fun isUnique vseSpr = 
                case vseSpr of
                    []             => true 
                    | sprG::sprRep => not (List.exists (fn sprTmp => sprG = sprTmp) sprRep) andalso (isUnique sprRep)
        in isUnique (vseSpremenljivke vzorec)
        end

    (* priredi sprejme vrednost in vzorec. Če prirejanje uspe, funkcija vrne seznam vezav, to je seznam spremenljivk
     * s pripadajočimi vrednostmi, v poljubnem vrstnem redu. Opazimo, da če vzorec ne vsebuje nobenega vzorca 
     * Spr s, je rezultat SOME []. Če prirejanje ne uspe, funkcija vrne NONE. Namig: vzorčna rešitev ima en case 
     * stavek in uporablja funkciji vsiOdgovori ter ListPair.zip  
     * -------------------------------------------------------------------------------------------------------------
     * Pregledamo vse primere klicev funkcije priredi
     * V primeru spremenljivke ji priredimo vrednost
     * V primeru da najdemo enoto in njen vzorec vrnemo prazen seznam
     * Gremo v rekurzivni klic v primeru Konstruktorja in Terke *)
    fun priredi (vrednost, vzorec) = 
        case (vrednost, vzorec) of
            (vrednostSpr, Spr spr)                                                     => SOME [(spr, vrednostSpr)]
            | (Enota, EnotaV)                                                          => SOME []
            | (Konst konst, KonstV konstV)                                             => 
                if konst = konstV then SOME [] else NONE
            | (_, Poljubno)                                                            => SOME []
            | (Konstruktor (konsIme, vrednostTmp), KonstruktorV (konsVIme, vzorecTmp)) => 
                if konsIme = konsVIme (* konstruktorja se morata ujemati po imenu *)
                then priredi (vrednostTmp, vzorecTmp) 
                else NONE
            | (Terka sezVrednosti, TerkaV seznamVzorcev) => 
                if (length seznamVzorcev) = (length sezVrednosti) (* terki se morata ujemati po dolzini *)
                then vsiOdgovori priredi (ListPair.zip(sezVrednosti, seznamVzorcev)) (* priredi za vsak element terke *)
                else NONE
            | (_, _) => NONE

    (* prvaPrireditev sprejme vrednost in seznam vzorcev, vrne seznam vezav prvega uspešno prirejenega vzorca.
     * Če prirejanje ne uspe nad nobenim vzorcem, vrnemo NONE. Uporabite prviOdgovor in izraz handle. Vzorčna
     * rešitev ima 3 vrstice. 
     * -------------------------------------------------------------------------------------------------------
     * Vsem vzorcom dodamo v terko vrednost in nato na tem seznamu klicemo prvi odgovor s funkcijo priredi
     * Ce odgovora ni, ukrepamo ob izjemi NiOdgovora*)
    fun prvaPrireditev (vrednost, vzorci) = 
        SOME (prviOdgovor priredi (map (fn vzorecTmp => (vrednost, vzorecTmp)) vzorci))
        handle NiOdgovora => NONE

    (* šele tu potrebujemo tipe naših vrednosti *)
    type zalogaPodatkovnihTipov = (string * string * tip) list     
    (* Elementi zaloge Podatkovnih Tipov so oblike na primer ("Foo","Bar",EnotaT); predstavlja zalogo podatkovnih 
     * tipov, pomeni pa, da konstruktor z imenom "Foo" vrne PodatkovniTip "Bar", če dobi enoto. Lahko predpostavimo, 
     * da so prva polja (imena konstruktorjev) unikatna, lahko pa več konstruktorjev vrne isti PodatkovniTip. *)

    (* podatkovniTip iz zalogePodatkovnihTipov poišče iskani konstruktor, preveri, če se tudi tip ujema, in vrne
     * ustrezni PodatkovniTip, oz. NONE, če funkcija ne uspe.  
     * ----------------------------------------------------------------------------------------------------------
     * Gremo po celi zalogi podatkovnih tipov, dokler ne najdemo ustrezni konstruktor in tip ali pridemo do konca *)
    fun podatkovniTip (konstruktor, tip, zalogaPodatkovnihTipov: zalogaPodatkovnihTipov) =
        case zalogaPodatkovnihTipov of 
            []                                      => NONE
            |(konsZal, podTipZal, tipZal) :: repZal => 
                if konsZal = konstruktor andalso tip=tipZal
                then SOME (PodatkovniTip podTipZal)
                else podatkovniTip (konstruktor, tip, repZal)

    (* izracunajTip sprejme vzorec in zalogoPodatkovnihTipov (opisana zgoraj) ter izračuna najmanj splošen tip
     * podanega vzorca. Če tak tip ne obstaja, vrnemo NONE. 
     * --------------------------------------------------------------------------------------------------------
     * Poskusam z ujemanjem vzorcem po vseh moznostih za vzorce, naletel na tezave ob seznamu vzorcev, 
     * kjer funkcija vsiOdgovori ocitno vraca napacen tip*)
    fun izracunajTip (vzorec, zalogaPodatkovnihTipov) = raise NiImplementirano (*
        case vzorec of
            Poljubno                            => SOME Karkoli
            | Spr _                             => SOME Karkoli       
            | KonstV _                          => SOME IntT
            | EnotaV                            => SOME EnotaT
            | TerkaV sezVzorcev                 =>
                (case (vsiOdgovori (fn vzorecTmp => izracunajTip (vzorecTmp, zalogaPodatkovnihTipov)) sezVzorcev) of
                    NONE     => NONE
                    |SOME sezTipov => SOME (TerkaT sezTipov))
            | KonstruktorV (konsTmp, vzorecTmp) => 
                case izracunajTip (vzorecTmp, zalogaPodatkovnihTipov) of 
                    NONE    => NONE
                    |SOME tip => podatkovniTip(konsTmp, tip, zalogaPodatkovnihTipov)
    *)

    (* prizanesljiv izračuna najmanj splošen tip, ki zajema podana tip1 in tip2. Če za primer dobimo tip
     * TerkaT[Karkoli,TerkaT[Karkoli,Karkoli]] in TerkaT[Karkoli,Karkoli], potem vrnemo tip 
     * SOME TerkaT[Karkoli,TerkaT[Karkoli,Karkoli]]. Če tak tip ne obstaja, vrnemo NONE. *)
    fun prizanesljiv (tip1, tip2) = raise NiImplementirano

    (* izracunajTipVzorcev sprejme seznam vzorecev in zalogo podatkovnih tipov (zalogaPodatkovnihTipov, opisana zgoraj),
     * izračuna pa najbolj prizanesljiv (najmanj splošen) tip, ki zajema vse podane vzorce. Če tak tip ne obstaja, 
     * vrnemo NONE. *)
    fun izracunajTipVzorcev (vzorci, zalogaPodatkovnihTipov) = raise NiImplementirano

end (* structure Vzorci *)

(*
val t11 = Vzorci.veljavenVzorec(TerkaV [KonstV 42, KonstruktorV ("bar", EnotaV), Poljubno]) = true;
val t12 = Vzorci.veljavenVzorec(TerkaV [Spr"foo", Spr"foo"]) = false;
val t13 = Vzorci.priredi(Konst 0, KonstV 1) = NONE;
val t14 = Vzorci.priredi(Terka [Konst 42, Konstruktor ("bar", Enota)], Poljubno) = SOME[];
val t15 = Vzorci.prvaPrireditev(Konst 42, [KonstV 7, Spr"moo", EnotaV]) = SOME [("moo",Konst 42)];
val t16 = Vzorci.podatkovniTip("foo", Karkoli, [("bar","foo",Karkoli), ("foo","bar",EnotaT)]) = NONE;
val t17 = Vzorci.podatkovniTip("foo", EnotaT, [("foo","bar",EnotaT)]) = SOME (PodatkovniTip "bar");
val t18 = Vzorci.izracunajTip(KonstruktorV("foo",EnotaV), [("foo","bar",EnotaT)])= SOME (PodatkovniTip "bar");
val t19 = Vzorci.prizanesljiv(TerkaT [Karkoli, Karkoli], TerkaT [TerkaT [Karkoli, Karkoli], Karkoli]) = SOME (TerkaT [TerkaT [Karkoli,Karkoli], Karkoli]);
val t20 = Vzorci.izracunajTipVzorcev([Poljubno, Spr"moo", KonstruktorV("foo",EnotaV)], [("foo","bar",EnotaT)]) = SOME (PodatkovniTip "bar");
*)

(* 

                                  _
                              ,--.\`-. __
                            _,.`. \:/,"  `-._
                        ,-*" _,.-;-*`-.+"*._ )
                       ( ,."* ,-" / `.  \.  `.
                      ,"   ,;"  ,"\../\  \:   \
                     (   ,"/   / \.,' :   ))  /
                      \  |/   / \.,'  /  // ,'                         '.    \   :   /    .'  
                       \_)\ ,' \.,'  (  / )/                             '.   \  :  /   .'      
                           `  \._,'   `"                                   '.  \ : /  .'     
                              \../                                   -----____   _  _____-----      
_ __ _________________________\../______________________________________________(_)_____________________ __ _     
                    ~        ~\../           (__)              ~~     ~             ~
             ~~          ~~   \../           (‿‿)     ~~   ~        ~~     ~~
        ~~    ~   ~~  __...---\../-.- /-------\/ ..__ ~~~       ~~            ~~               ~
          ~~~~  ~_,--'        \../   / |     ||      `--.__               ~~           ~~           ~~
      ~~~  __,--'              `"   *  ||----||            `--.__    ~~
   ~~  ,--'                            ^^    ^^               ___`--.        ~~               ~
   ダレ'------......______             ______......------`````                        ~
    ~~~   ~    ~~      ~ `````---"""""  ~~   ~     ~~           ~~      ~~~
           ~~~~    ~~  ~~~~       ~~~~~~  ~ ~~   ~~ ~~~     ~~~~   ~~~        ~~     
        ~~   ~   ~~~     ~~~ ~         ~~       ~~       ~
                 ~        ~~       ~~~       ~
・*)