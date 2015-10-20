let fun convert 0 = [0] 
        | convert x = 
            if x = 1 then [1] else (Int.rem(x,2))::convert(x div 2) 
in fun foo num = rev (convert(num)) 
end

let val urejeneSpremenljivke = ListMergeSort.uniqueSort String.compare spremenljivk
            val len = Real.fromInt(List.length(urejeneSpremenljivke))
            val potenca = (Math.pow(2.0, len))
            val firstBinaryNum =  (Int.toReal(potenca)) - 1
            fun dec2bin 0 = [0] 
                | dec2bin num = (num mod 2)::dec2bin(num div 2)
            fun novaInterpretacija sezSpr sezBin =
                case sezSpr of
                    [] => []
                    | gSpr::rSpr => 
                        case sezBin of
                            [] => (gSpr, false) :: (novaInterpretacija rSpr [])
                            | gBin::rBin => 
                                if gBin = 0
                                then (gSpr, false) :: (novaInterpretacija rSpr rBin)
                                else (gSpr, true) :: (novaInterpretacija rSpr rBin)
            fun zankaInterpretacij i =
                if i < 0
                then []
                else (novaInterpretacija urejeneSpremenljivke (dec2bin i))::(zankaInterpretacij i-1)
        in zankaInterpretacij firstBinaryNum
        end