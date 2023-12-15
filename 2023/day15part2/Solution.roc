interface Solution
    exposes [solve]
    imports [
        ParseInput.{ parse, Steps, Operation, LabelledLens },
    ]

expect
    testInput =
        "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7\n"
    result =
        testInput
        |> parse
        |> Result.map solve
    exptResult = 145
    result == Ok exptResult

solve : Steps -> U32
solve = \steps ->
    steps |> processOps |> calculateBoxesPower

Boxes : List (List LabelledLens)

newBoxes : Nat -> Boxes
newBoxes = \count -> List.repeat [] count

calculateBoxesPower : Boxes -> U32
calculateBoxesPower = \boxes ->
    powerPerBox =
        box, boxIndex <- boxes |> List.mapWithIndex

        powerPerLens =
            lens, lensIndex <- box |> List.mapWithIndex
            (Num.toU32 (lensIndex + 1)) * (Num.toU32 lens.number)
        (List.sum powerPerLens) * (Num.toU32 (boxIndex + 1))

    List.sum powerPerBox

expect
    testIn = [
        Append { label: Str.toUtf8 "rn", number: 1 },
        Remove (Str.toUtf8 "cm"),
        Append { label: Str.toUtf8 "qp", number: 3 },
        Append { label: Str.toUtf8 "cm", number: 2 },
        Remove (Str.toUtf8 "qp"),
        Append { label: Str.toUtf8 "pc", number: 4 },
        Append { label: Str.toUtf8 "ot", number: 9 },
        Append { label: Str.toUtf8 "ab", number: 5 },
        Remove (Str.toUtf8 "pc"),
        Append { label: Str.toUtf8 "pc", number: 6 },
        Append { label: Str.toUtf8 "ot", number: 7 },
    ]
    result = processOps testIn
    exptResult =
        List.concat
            [
                [
                    { label: Str.toUtf8 "rn", number: 1 },
                    { label: Str.toUtf8 "cm", number: 2 },
                ],
                [],
                [],
                [
                    { label: Str.toUtf8 "ot", number: 7 },
                    { label: Str.toUtf8 "ab", number: 5 },
                    { label: Str.toUtf8 "pc", number: 6 },
                ],
            ]
            (List.repeat [] (256 - 4))
    result == exptResult
processOps : List Operation -> Boxes
processOps = \ops ->
    boxes, op <- ops |> List.walk (newBoxes 256)
    when op is
        Remove targetLabel ->
            boxIndex = hashStep targetLabel |> Num.toNat
            box = boxes |> List.get boxIndex |> okOrCrash "TODO handle errors"
            when List.findFirstIndex box (\{ label, number } -> label == targetLabel) is
                Ok index ->
                    List.set boxes boxIndex (List.dropAt box index)

                Err _ -> boxes

        Append labelledLens ->
            boxIndex = hashStep labelledLens.label |> Num.toNat
            box = boxes |> List.get boxIndex |> okOrCrash "TODO handle errors"
            when List.findFirstIndex box (\{ label, number } -> label == labelledLens.label) is
                Ok index ->
                    List.set boxes boxIndex (List.set box index labelledLens)

                Err _ ->
                    List.set boxes boxIndex (List.append box labelledLens)

hashStep : List U8 -> U8
hashStep = \stepStr -> stepStr |> List.walk 0 hash

hash : U8, U8 -> U8
hash = \value, unit -> value |> Num.addWrap unit |> Num.mulWrap 17

# ##############################################################################
okOrCrash : Result a *, Str -> a
okOrCrash = \result, crashMsg ->
    when result is
        Ok value -> value
        _ -> crash crashMsg
