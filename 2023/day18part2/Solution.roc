interface Solution
    exposes [solve]
    imports [
        ParseInput.{ parse, DigPlan, Instruction, Direction },
    ]

LabelledAreaSections : { positives : List AreaSection, negatives : List AreaSection }
AreaSection : { margin : U32, inner : U32 }

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
    exptResult = 62
    result == Ok exptResult

solve : DigPlan -> Result U32 _
solve = \plan ->
    plan
    |> List.map .given
    |> collectAreaSections
    |> Result.try calculateArea

calculateArea : LabelledAreaSections -> Result U32 _
calculateArea = \{ positives, negatives } ->
    Num.subChecked
        (positives |> List.map .inner |> List.sum)
        (negatives |> List.map (\area -> area.inner + area.margin) |> List.sum)
    |> Result.mapErr (\_ -> InvalidAreaSections)

collectAreaSections : List Instruction -> Result LabelledAreaSections _
collectAreaSections = \lines ->
    (linesMid, areasMid, rollCount) <- loop (lines, { lefts: [], rights: [] }, 0)
    if List.len linesMid < 4 then
        InvalidInstructionLoop |> Err |> Break
    else if List.len linesMid == 4 then
        (sqArea, sqSide) = getSquareArea linesMid
        sqAreaSection = { inner: sqArea, margin: 0 }

        result =
            when (areasMid, sqSide) is
                ({ lefts, rights }, Left) ->
                    { positives: lefts |> List.append sqAreaSection, negatives: rights }

                ({ lefts, rights }, Right) ->
                    { positives: rights |> List.append sqAreaSection, negatives: lefts }
        result |> Ok |> Break
    else if rollCount >= List.len linesMid then
        CouldNotReduce |> Err |> Break
    else
        reducedBend =
            (bend, i) <-
                List.map4
                    linesMid
                    (linesMid |> List.dropFirst 1)
                    (linesMid |> List.dropFirst 2)
                    (linesMid |> List.dropFirst 3)
                    (\x1, x2, x3, x4 -> (x1, x2, x3, x4))
                |> List.mapWithIndex (\x, i -> (x, i))
                |> List.mapTry

            reduction <- verifyMinimalBend [bend.0, bend.1, bend.2, bend.3] |> Result.mapErr
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
                    List.dropFirst linesMid (i + 4),
                ]
                areasNext =
                    when (areasMid, side) is
                        ({ lefts, rights }, Left) ->
                            { lefts: lefts |> List.append area, rights }

                        ({ lefts, rights }, Right) ->
                            { lefts, rights: rights |> List.append area }
                Continue (linesNext, areasNext, 0)

getSquareArea : List Instruction -> (U32, [Left, Right])
getSquareArea = \lines ->
    when lines is
        [l1, l2, l3, l4] ->
            area =
                (l1.distance - 1 |> Num.toU32)
                * (l2.distance - 1 |> Num.toU32)
            side = getSide l1.dir l2.dir

            (area, side)

        _ -> crash "not a square"

verifyMinimalBend : List Instruction -> Result {} (List Instruction, AreaSection, [Left, Right])
verifyMinimalBend = \bend ->
    # Try forwards reduction
    _ <- verifyMinimalBendInner bend |> Result.try

    # Try backwards reduction
    (bendReversed, area, side) <-
        bend |> List.reverse |> verifyMinimalBendInner |> Result.mapErr
    (bendReversed |> List.reverse, area, side)

verifyMinimalBendInner = \bend ->
    when bend is
        [x1, x2, x3, x4] if
        (x1.dir == turnDirectionBack x3.dir)
        && (x2.dir == x4.dir)
        && (x1.distance > x3.distance) ->
            (
                [
                    { x1 & distance: x1.distance - x3.distance },
                    { x2 & distance: x2.distance + x4.distance },
                ],
                {
                    inner: (x3.distance - 1) * (x2.distance - 1) |> Num.toU32,
                    margin: (x2.distance - 1) + x3.distance * 2 |> Num.toU32,
                },
                getSide x1.dir x2.dir,
            )
            |> Err

        [_, _, _, _] -> Ok {}
        _ -> crash "should pass precisely 4 items to this function"

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
        Left -> Right
        Down -> Up
        Right -> Left

turnDirectionRight = \dir ->
    dir |> turnDirectionLeft |> turnDirectionLeft |> turnDirectionLeft

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
