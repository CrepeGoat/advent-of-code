interface Solution2
    exposes [solve]
    imports [
        ParseInput.{ parse, Platform, Space },
    ]

testInput =
    """
    O....#....
    O.OO#....#
    .....##...
    OO.#O....O
    .O.....O#.
    O.#..O.#.#
    ..O..#O..O
    .......O..
    #....###..
    #OO..#....

    """
expect
    result =
        testInput
        |> parse
        |> Result.map solve
    exptResult = 136
    result == Ok exptResult

solve : Platform -> Nat
solve = \platform ->
    platform
    |> transpose
    |> moveRoundRocksWest
    |> transpose
    |> List.reverse
    |> calculateSouthLoad

calculateSouthLoad = \platform ->
    platform
    |> List.mapWithIndex (\row, i -> row |> List.countIf (\x -> x == Round) |> Num.mul (i + 1))
    |> List.sum

moveRoundRocksWest : Platform -> Platform
moveRoundRocksWest = \platform ->
    row <- platform |> List.map

    (rowMid, index) <- loop (row, 0)
    when findFirstInListFrom rowMid index [Empty, Round] is
        Ok patternIndex ->
            newRow =
                rowMid
                |> List.set patternIndex Round
                |> List.set (patternIndex + 1) Empty
            Continue (newRow, Num.subSaturated patternIndex 1)

        Err _ -> Break rowMid

# ##############################################################################

findFirstInListFrom : List a, Nat, List a -> Result Nat _ where a implements Eq
findFirstInListFrom = \sourceOriginal, index, target ->
    source = sourceOriginal |> List.dropFirst index
    when target is
        [] -> Ok index
        [targetFirst, ..] ->
            targetRest = List.dropFirst target 1

            subSource <- loop source
            when List.findFirstIndex subSource (\x -> x == targetFirst) is
                Err NotFound -> NotFound |> Err |> Break
                Ok subIndex ->
                    if subSource |> List.dropFirst subIndex |> List.startsWith target then
                        (List.len sourceOriginal)
                        - (List.len subSource)
                        + subIndex
                        |> Ok
                        |> Break
                    else
                        subSource |> List.dropFirst (subIndex + 1) |> Continue

expect
    listlist = [[1, 2, 3], [4, 5, 6]]
    listlistT = [[1, 4], [2, 5], [3, 6]]
    transpose listlist == listlistT
transpose : List (List a) -> List (List a)
transpose = \listlist ->
    when listlist is
        [] -> []
        [list1, ..] ->
            resultInit = list1 |> List.map List.single
            resultMid, listN <- listlist |> List.dropFirst 1 |> List.walk resultInit
            subResultMid, listNItem <- List.map2 resultMid listN
            List.append subResultMid listNItem

walkFromFirst : List a, (a, a -> a) -> Result a [ListWasEmpty]
walkFromFirst = \list, fold ->
    when list is
        [] -> Err ListWasEmpty
        [item, ..] -> Ok (List.walkFrom list 1 item fold)

loop : state, (state -> [Break b, Continue state]) -> b
loop = \stateInit, runIteration ->
    when runIteration stateInit is
        Continue state -> loop state runIteration
        Break result -> result

okOrCrash : Result a *, Str -> a
okOrCrash = \result, crashMsg ->
    when result is
        Ok value -> value
        _ -> crash crashMsg
