interface Solution
    exposes [solve]
    imports [
        ParseInput.{ parse, DigPlan, Instruction, Direction },
    ]

LabelledAreaSections : { positives : List AreaSection, negatives : List AreaSection }
AreaSection : { positive : AreaNum, negative : AreaNum }
AreaNum : U64

## The given test
expect
    testInput =
        """
        R 6 (#70c710)
        D 5 (#0dc571)
        L 2 (#5713f0)
        D 2 (#d2c081)
        R 2 (#59c680)
        D 2 (#411b91)
        L 5 (#8ceee2)
        U 2 (#caa173)
        L 1 (#1b58a2)
        U 2 (#caa171)
        R 2 (#7807d2)
        U 3 (#a77fa3)
        L 2 (#015232)
        U 2 (#7a21e3)

        """
    result =
        testInput
        |> parse
        |> Result.try solve
    exptResult = 952408144115
    result == Ok exptResult

## Four-sided cardinal cross
expect
    testInput =
        """
        R 2 (#000020)
        D 2 (#000021)
        R 2 (#000020)
        U 2 (#000023)
        R 2 (#000020)
        U 2 (#000023)
        L 2 (#000022)
        U 2 (#000023)
        L 2 (#000022)
        D 2 (#000021)
        L 2 (#000022)
        D 2 (#000021)

        """
    result =
        testInput
        |> parse
        |> Result.try solve
    exptResult = 33
    result == Ok exptResult

solve : DigPlan -> Result AreaNum _
solve = \plan ->
    if plan |> List.map .given |> startMeetsEnd then
        plan
        |> List.map .encoded
        |> collectAreaSections
        |> Result.try calculateArea
    else
        Err InvalidInstructions

calculateArea : LabelledAreaSections -> Result AreaNum _
calculateArea = \{ positives, negatives } ->
    Num.subChecked
        (positives |> List.map .positive |> List.sum)
        (negatives |> List.map .negative |> List.sum)
    |> Result.mapErr (\_ -> InvalidAreaSections)

collectAreaSections : List Instruction -> Result LabelledAreaSections _
collectAreaSections = \lines ->
    (linesMid, areasMid, rollCount) <- loop (lines, { lefts: [], rights: [] }, 0)

    if List.len linesMid < 4 then
        InvalidInstructionLoop |> Err |> Break
    else if List.len linesMid == 4 then
        (sqArea, sqSide) = getSquareArea linesMid

        result =
            when (areasMid, sqSide) is
                ({ lefts, rights }, Left) ->
                    { positives: lefts |> List.append sqArea, negatives: rights }

                ({ lefts, rights }, Right) ->
                    { positives: rights |> List.append sqArea, negatives: lefts }
        result |> Ok |> Break
    else if rollCount >= List.len linesMid then
        CouldNotReduce |> Err |> Break
    else
        reducedBend =
            (bend, i) <-
                List.range { start: linesMid |> List.len |> Num.sub 5 |> After, end: At 0 }
                |> List.map (\i -> (List.sublist linesMid { start: i, len: 5 }, i))
                |> List.mapTry

            reduction <-
                verifyMinimalBend bend
                |> Result.mapErr
            (reduction, i)

        when reducedBend is
            Ok _ ->
                linesRolled = List.concat
                    (List.dropFirst linesMid 1)
                    (List.takeFirst linesMid 1)
                (linesRolled, areasMid, rollCount + 1) |> Continue

            Err ((reducedLine, area, side), i) ->
                linesNext = List.join [
                    List.takeFirst linesMid i,
                    reducedLine,
                    List.dropFirst linesMid (i + 5),
                ]
                areasNext =
                    when (areasMid, side) is
                        ({ lefts, rights }, Left) ->
                            { lefts: lefts |> List.append area, rights }

                        ({ lefts, rights }, Right) ->
                            { lefts, rights: rights |> List.append area }
                Continue (linesNext, areasNext, 0)

getSquareArea : List Instruction -> (AreaSection, [Left, Right])
getSquareArea = \lines ->
    when lines is
        [l1, l2, l3, l4] ->
            area =
                (l1.distance + 1 |> Num.toU64)
                * (l2.distance + 1 |> Num.toU64)
            side = getSide l1.dir l2.dir

            ({ positive: area, negative: 0 }, side)

        _ -> crash "not a square"

verifyMinimalBend : List Instruction -> Result {} (List Instruction, AreaSection, [Left, Right])
verifyMinimalBend = \bend ->
    # Try button reduction
    _ <- bend |> verifyButtonBend |> Result.try

    # Try forwards P-reduction
    _ <- bend |> verifyPBend |> Result.try

    # Try backwards P-reduction
    bend |> verifyPBendReverse

verifyButtonBend : List Instruction -> Result {} (List Instruction, AreaSection, [Left, Right])
verifyButtonBend = \bend ->
    when bend is
        [x0, x1, x2, x3, x4] if
        (x0.dir == x2.dir)
        && (x0.dir == x4.dir)
        && (x1.dir == turnDirectionBack x3.dir)
        && (x1.distance == x3.distance) ->
            (
                [
                    { x0 & distance: x0.distance + x2.distance + x4.distance },
                ],
                {
                    positive: [x3.distance, x2.distance + 1]
                    |> List.map Num.toU64
                    |> List.product,
                    negative: [x3.distance, x2.distance - 1]
                    |> List.map Num.toU64
                    |> List.product,
                },
                getSide x1.dir x2.dir,
            )
            |> Err

        [_, _, _, _, _] -> Ok {}
        _ -> crash "should pass precisely 5 items to this function"

verifyPBend : List Instruction -> Result {} (List Instruction, AreaSection, [Left, Right])
verifyPBend = \bend ->
    when bend is
        [x1, x2, x3, x4, xLeftover] if
        (x1.dir == turnDirectionBack x3.dir)
        && (x2.dir == x4.dir)
        && (x1.distance > x3.distance) ->
            (
                [
                    { x1 & distance: x1.distance - x3.distance },
                    { x2 & distance: x2.distance + x4.distance },
                    xLeftover,
                ],
                {
                    positive: [x3.distance, x2.distance + 1]
                    |> List.map Num.toU64
                    |> List.product,
                    negative: [x3.distance, x2.distance - 1]
                    |> List.map Num.toU64
                    |> List.product,
                },
                getSide x1.dir x2.dir,
            )
            |> Err

        [_, _, _, _, _] -> Ok {}
        _ -> crash "should pass precisely 5 items to this function"

verifyPBendReverse : List Instruction -> Result {} (List Instruction, AreaSection, [Left, Right])
verifyPBendReverse = \bend ->
    (bendReversed, area, side) <-
        bend |> List.reverse |> verifyPBend |> Result.mapErr

    newBend = bendReversed |> List.reverse
    newSide =
        when side is
            Left -> Right
            Right -> Left
    (newBend, area, newSide)

getSide : Direction, Direction -> [Left, Right]
getSide = \dir1, dir2 ->
    if dir2 == turnDirectionLeft dir1 then
        Left
    else if dir2 == turnDirectionRight dir1 then
        Right
    else
        crash "invalid instruction sequence"

turnDirectionLeft : Direction -> Direction
turnDirectionLeft = \dir ->
    when dir is
        Up -> Left
        Left -> Down
        Down -> Right
        Right -> Up

turnDirectionBack : Direction -> Direction
turnDirectionBack = \dir ->
    when dir is
        Up -> Down
        Down -> Up
        Left -> Right
        Right -> Left

turnDirectionRight = \dir ->
    dir |> turnDirectionLeft |> turnDirectionLeft |> turnDirectionLeft

startMeetsEnd : List Instruction -> Bool
startMeetsEnd = \instructions ->
    (instructions |> List.keepIf (\instr -> instr.dir == Left) |> List.map .distance |> List.sum)
    == (instructions |> List.keepIf (\instr -> instr.dir == Right) |> List.map .distance |> List.sum)
    && (instructions |> List.keepIf (\instr -> instr.dir == Up) |> List.map .distance |> List.sum)
    == (instructions |> List.keepIf (\instr -> instr.dir == Down) |> List.map .distance |> List.sum)

# ##############################################################################

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

okOr : Result a err, (err -> a) -> a
okOr = \result, xformErr ->
    when result is
        Ok a -> a
        Err e -> xformErr e

okOrTry : Result a err, (err -> Result a err) -> Result a err
okOrTry = \result, xformErr ->
    when result is
        Ok a -> Ok a
        Err e -> xformErr e
