interface Solution2
    exposes [solve]
    imports [
        ParseInput.{ parse, Platform, Space },
    ]

expect
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
    result =
        testInput
        |> parse
        |> Result.map solve
    exptResult = 64
    result == Ok exptResult

cycleReps = 1000000000

solve : Platform -> Nat
solve = \platform ->
    (platformFinal, _) =
        (platformMid, cache), cycleNum <-
            List.range { start: At 1, end: Length cycleReps }
            |> List.walkUntil (platform, Dict.empty {})

        nextPlatform = runCycle platformMid

        if Dict.contains cache nextPlatform then
            prevCycleNum = Dict.get cache nextPlatform |> okOrCrash "valid key"
            repeatLen = cycleNum - prevCycleNum
            if (cycleReps - cycleNum) % repeatLen == 0 then
                Break (nextPlatform, cache)
            else
                Continue (nextPlatform, Dict.insert cache nextPlatform cycleNum)
        else
            Continue (nextPlatform, Dict.insert cache nextPlatform cycleNum)

    platformFinal |> List.reverse |> calculateSouthLoad

calculateSouthLoad = \platform ->
    platform
    |> List.mapWithIndex (\row, i -> row |> List.countIf (\x -> x == Round) |> Num.mul (i + 1))
    |> List.sum

expect
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
    exptOutput =
        """
        .....#....
        ....#...O#
        ...OO##...
        .OO#......
        .....OOO#.
        .O#...O#.#
        ....O#....
        ......OOOO
        #...O###..
        #..OO#....

        """
    result =
        testInput
        |> parse
        |> Result.map runCycle
    exptResult = exptOutput |> parse
    result == exptResult
expect
    testInput =
        """
        .....#....
        ....#...O#
        ...OO##...
        .OO#......
        .....OOO#.
        .O#...O#.#
        ....O#....
        ......OOOO
        #...O###..
        #..OO#....

        """
    exptOutput =
        """
        .....#....
        ....#...O#
        .....##...
        ..O#......
        .....OOO#.
        .O#...O#.#
        ....O#...O
        .......OOO
        #..OO###..
        #.OOO#...O

        """
    result =
        testInput
        |> parse
        |> Result.map runCycle
    exptResult = exptOutput |> parse
    result == exptResult

runCycle = \platform -> platform # v roll north
    |> transpose
    |> moveRoundRocksWest
    |> transpose # v roll west
    |> moveRoundRocksWest # v roll south
    |> List.reverse
    |> transpose
    |> moveRoundRocksWest
    |> transpose
    |> List.reverse # v roll east
    |> List.map List.reverse
    |> moveRoundRocksWest
    |> List.map List.reverse

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
