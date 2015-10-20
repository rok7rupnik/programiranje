(*
 ___________ _______  ________ ___________ __     _______       __     _____  ___       ___  _______  
("     _   ")"     "|/"       |"     _   ")" \   /"      \     /""\   (\"   \|"  \     |"  |/"     "| 
 )__/  \\__(: ______|:   \___/ )__/  \\__/||  | |:        |   /    \  |.\\   \    |    ||  (: ______) 
    \\_ /   \/    |  \___  \      \\_ /   |:  | |_____/   )  /' /\  \ |: \.   \\  |    |:  |\/    |   
    |.  |   // ___)_  __/  \\     |.  |   |.  |  //      /  //  __'  \|.  \    \. | ___|  / // ___)_  
    \:  |  (:      "|/" \   :)    \:  |   /\  |\|:  __   \ /   /  \\  \    \    \ |/  :|_/ |:      "| 
     \__|   \_______|_______/      \__|  (__\_|_)__|  \___|___/    \___)___|\____\|_______/ \_______) 
*)
val moo = Izjave.ustvariInterpretacijo ([("moo", true), ("boo", false)]);
val boo = Izjave.apliciraj (moo, "boo") = false;
val t01 = Izjave.evalviraj (Neg(S"moo")Ali S"boo", moo) = false;
val t02 = Izjave.spremenljivkeIzjave(Neg(S"foo")) = ["foo"];
val t03 = Izjave.jeTavtologija(Neg(S"a")Ali S"a") = true;
val t04 = Izjave.jeIzpolnjiva(DA Imp S"z" Ali Neg(S"p") In S"c") = true;
val t05 = Izjave.jeProtislovje(S"moo" Ali Neg(S"moo") Imp NE) = true;
(*``'-.,_,.-'``'-.,_,.='``'-.,_,.-'``'-.,_,.='````'-.,_,.-'``'-.,_,.='``'-.,_,.-'``'-.,_,.='``*)
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
(*

                   |"|                                     .      .        !!!         |              _   _                        |"|      
     '*`          _|_|_       `  _ ,  '       ()_()      .  .:::.       `  _ _  '      |.===.        '\\-//`         vvv          _|_|_     
    (o o)         (o o)      -  (o)o)  -      (o o)        :(o o):  .  -  (OXO)  -     {}o o{}        (o o)         (0~0)         (o o)     
ooO--(_)--Ooo-ooO--(_)--Ooo--ooO'(_)--Ooo-ooO--`o'--Ooo-ooO--(_)--Ooo-ooO--(_)--Ooo-ooO--(_)--Ooo-ooO--(_)--Ooo-ooO--(_)--Ooo-ooO--(_)--Ooo-*)
