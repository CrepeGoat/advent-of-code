interface Solution
    exposes [solve]
    imports [
        ParseInput.{ parse, DigPlan, Instruction, Direction },
    ]

BiDirections : { enter : Direction, exit : Direction }
Position : { x : I32, y : I32 }
Bounds : { minx : I32, maxx : I32, miny : I32, maxy : I32 }

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
    crash "TODO"

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
