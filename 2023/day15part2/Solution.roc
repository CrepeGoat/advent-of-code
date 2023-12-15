interface Solution
    exposes [solve]
    imports [
        ParseInput.{ parse, Steps, Step },
    ]

expect
    testInput =
        "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7\n"
    result =
        testInput
        |> parse
        |> Result.map solve
    exptResult = 1320
    result == Ok exptResult

solve : Steps -> U32
solve = \steps ->
    steps |> List.map hashStep |> List.map Num.toU32 |> List.sum

hashStep : List U8 -> U8
hashStep = \stepStr -> stepStr |> List.walk 0 hash

hash : U8, U8 -> U8
hash = \value, unit -> value |> Num.addWrap unit |> Num.mulWrap 17
