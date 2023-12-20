interface Solution
    exposes [solve]
    imports [
        ParseInput.{ parse, DigPlan, Instruction, Direction },
    ]

LabelledAreaSections : { positives : List AreaSection, negatives : List AreaSection }
AreaSection : { margin : U32, inner : U32 }
LineWithMetadata : { points : Line, instruction : Instruction I32 }
Line : (Position, Position)
Position : { x : I32, y : I32 }

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
    |> planToBorderLines
    |> collectAreaSections
    |> Result.try calculateArea

calculateArea : LabelledAreaSections -> Result U32 _
calculateArea = \{ positives, negatives } ->
    Num.subChecked
        (positives |> List.map .inner |> List.sum)
        (negatives |> List.map (\area -> area.inner + area.margin) |> List.sum)

collectAreaSections : List LineWithMetadata -> Result LabelledAreaSections _
collectAreaSections = \lines ->
    crash "TODO"

getAllPoints : List LineWithMetadata -> List Position
getAllPoints = \lines -> lines |> List.map (\line -> line.points.0)

planToBorderLines : DigPlan -> List LineWithMetadata
planToBorderLines = \plan ->
    (borderFinal, _) =
        posFirst = { x: 0, y: 0 }
        (borderMid, posMid), instructionPair <-
            plan |> List.walk ([], posFirst)

        instruction = getInstruction instructionPair
        lineNext = makeLineWithMetadata posMid instruction
        posNext = lineNext.1
        borderNext = List.append borderMid lineNext
        (borderNext, posNext)
    borderFinal

getInstruction : Instruction num -> { dir : Direction, distance : num }
getInstruction = .given

makeLineWithMetadata : Position, Instruction I32 -> LineWithMetadata
makeLineWithMetadata = \pos, instruction ->
    { points: (pos, getNearbyPos pos instruction), instruction }

getNearbyPos : Position, Instruction I32 -> Position
getNearbyPos = \{ x, y }, { dir, distance } ->
    when dir is
        Up -> { x, y: y + distance }
        Down -> { x, y: y - distance }
        Left -> { x: x - distance, y }
        Right -> { x: x + distance, y }

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
