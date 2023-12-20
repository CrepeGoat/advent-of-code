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
    border = plan |> makeBorderList
    biDirs <- plan |> makeDirs |> makeBiDirs |> Result.try
    borderFilled <- fillBorder border biDirs |> Result.map

    (Set.len borderFilled) |> Num.toU32

expect
    border = [
        { x: 0, y: 1 },
        { x: -1, y: 1 },
        { x: -2, y: 1 },
        { x: -3, y: 1 },
        { x: -3, y: 0 },
        { x: -3, y: -1 },
        { x: -3, y: -2 },
        { x: -2, y: -2 },
        { x: -1, y: -2 },
        { x: -1, y: -1 },
        { x: 0, y: -1 },
        { x: 0, y: 0 },
    ]
    biDirs = [
        { enter: Up, exit: Left },
        { enter: Left, exit: Left },
        { enter: Left, exit: Left },
        { enter: Left, exit: Down },
        { enter: Down, exit: Down },
        { enter: Down, exit: Down },
        { enter: Down, exit: Right },
        { enter: Right, exit: Right },
        { enter: Right, exit: Up },
        { enter: Up, exit: Right },
        { enter: Right, exit: Up },
    ]
    result = fillBorder border biDirs |> okOrCrash "test err"
    result |> Set.len == (List.len border + 3)

fillBorder : List Position, List BiDirections -> Result (Set Position) _
fillBorder = \border, biDirs ->
    borderSet = Set.fromList border
    bounds <- getBounds border |> Result.try

    adjBorderDirections : List { left : List Direction, right : List Direction }
    adjBorderDirections = border |> List.map2 biDirs getAdjBorderPoss

    leftFilledBorder =
        adjBorderPositions =
            pos, dirs <- List.map2 border (adjBorderDirections |> List.map .left)
            dirs |> List.map (\dir -> getAdjPos pos dir)
        borderSetMid, adjPos <-
            adjBorderPositions
            |> List.joinMap (\x -> x)
            |> List.walkTry borderSet
        fillToBounds borderSetMid bounds adjPos
        |> Result.map (\fill -> Set.union fill borderSetMid)

    _ <- leftFilledBorder |> okOrTry
    rightFilledBorder =
        adjBorderPositions =
            pos, dirs <- List.map2 border (adjBorderDirections |> List.map .right)
            dirs |> List.map (\dir -> getAdjPos pos dir)
        borderSetMid, adjPos <-
            adjBorderPositions
            |> List.joinMap (\x -> x)
            |> List.walkTry borderSet
        fillToBounds borderSetMid bounds adjPos
        |> Result.map (\fill -> Set.union fill borderSetMid)

    rightFilledBorder |> Result.mapErr (\_ -> InvalidBorder)

getAdjBorderPoss : Position, BiDirections -> { left : List Direction, right : List Direction }
getAdjBorderPoss = \pos, { enter, exit } ->
    if exit == enter |> turnDirectionLeft then
        { left: [], right: [enter, exit |> reverseDirection] }
    else if exit == enter |> turnDirectionRight then
        { left: [enter, exit |> reverseDirection], right: [] }
    else if enter == exit then
        { left: [enter |> turnDirectionLeft], right: [enter |> turnDirectionRight] }
    else
        crash "TODO handle error: direction cannot turn around"

## An interior node should expand to a finite set of points.
expect
    borderSet = Set.fromList [
        { x: 0, y: 1 },
        { x: -1, y: 1 },
        { x: -2, y: 1 },
        { x: -3, y: 1 },
        { x: -3, y: 0 },
        { x: -3, y: -1 },
        { x: -3, y: -2 },
        { x: -2, y: -2 },
        { x: -1, y: -2 },
        { x: -1, y: -1 },
        { x: 0, y: -1 },
        { x: 0, y: 0 },
    ]
    bounds = { minx: -3, maxx: 0, miny: -2, maxy: 1 }
    pos = { x: -1, y: 0 }
    result = fillToBounds borderSet bounds pos
    exptResult =
        Set.fromList [
            { x: -1, y: 0 },
            { x: -2, y: 0 },
            { x: -2, y: -1 },
        ]
        |> Ok
    result == exptResult
## An exterior node should expand to the boundary and then error.
expect
    borderSet = Set.fromList [
        { x: 0, y: 1 },
        { x: -1, y: 1 },
        { x: -2, y: 1 },
        { x: -3, y: 1 },
        { x: -3, y: 0 },
        { x: -3, y: -1 },
        { x: -3, y: -2 },
        { x: -2, y: -2 },
        { x: -1, y: -2 },
        { x: -1, y: -1 },
        { x: 0, y: -1 },
        { x: 0, y: 0 },
    ]
    bounds = { minx: -3, maxx: 0, miny: -2, maxy: 1 }
    pos = { x: 0, y: -2 }
    result = fillToBounds borderSet bounds pos
    exptResult = Err OutOfBounds
    result == exptResult
## A border node should not expand.
expect
    borderSet = Set.fromList [
        { x: 0, y: 1 },
        { x: -1, y: 1 },
        { x: -2, y: 1 },
        { x: -3, y: 1 },
        { x: -3, y: 0 },
        { x: -3, y: -1 },
        { x: -3, y: -2 },
        { x: -2, y: -2 },
        { x: -1, y: -2 },
        { x: -1, y: -1 },
        { x: 0, y: -1 },
        { x: 0, y: 0 },
    ]
    bounds = { minx: -3, maxx: 0, miny: -2, maxy: 1 }
    pos = { x: 0, y: 0 }
    result = fillToBounds borderSet bounds pos
    exptResult = Set.empty {} |> Ok
    result == exptResult
fillToBounds : Set Position, Bounds, Position -> Result (Set Position) [OutOfBounds]
fillToBounds = \borderSet, bounds, posStart ->
    (resultMid, queueMid) <- loop (Set.empty {}, [posStart])
    when queueMid is
        [] -> resultMid |> Ok |> Break
        [.., posN] ->
            queueRest = List.dropLast queueMid 1
            if isInBounds posN bounds |> Bool.not then
                Break (Err OutOfBounds)
            else if Set.contains resultMid posN || Set.contains borderSet posN then
                Continue (resultMid, queueRest)
            else
                resultNext = resultMid |> Set.insert posN
                queueNext = getAdjPoss posN |> List.walk queueRest List.append
                Continue (resultNext, queueNext)

expect
    plan = [
        { dir: Up, distance: 1, hexColor: "" },
        { dir: Left, distance: 2, hexColor: "" },
        { dir: Down, distance: 2, hexColor: "" },
        { dir: Right, distance: 2, hexColor: "" },
        { dir: Up, distance: 1, hexColor: "" },
    ]
    result = plan |> makeDirs |> makeBiDirs
    exptResult = [
        { enter: Up, exit: Left },
        { enter: Left, exit: Left },
        { enter: Left, exit: Down },
        { enter: Down, exit: Down },
        { enter: Down, exit: Right },
        { enter: Right, exit: Right },
        { enter: Right, exit: Up },
        { enter: Up, exit: Up },
    ]
    result == Ok exptResult
makeBiDirs : List Direction -> Result (List BiDirections) _
makeBiDirs = \dirs ->
    firstDir <- dirs |> List.first |> Result.map
    dirs
    |> List.dropFirst 1
    |> List.append firstDir
    |> List.map2 dirs (\exit, enter -> { exit, enter })

turnDirectionLeft : Direction -> Direction
turnDirectionLeft = \dir ->
    when dir is
        Up -> Left
        Left -> Down
        Down -> Right
        Right -> Up

turnDirectionRight = \dir ->
    dir |> turnDirectionLeft |> turnDirectionLeft |> turnDirectionLeft

reverseDirection : Direction -> Direction
reverseDirection = \dir ->
    when dir is
        Up -> Down
        Down -> Up
        Left -> Right
        Right -> Left

makeDirs : DigPlan -> List Direction
makeDirs = \plan ->
    { dir, distance, hexColor: _ } <- plan |> List.joinMap
    List.repeat dir (Num.toNat distance)

isInBounds : Position, Bounds -> Bool
isInBounds = \{ x, y }, { minx, miny, maxx, maxy } ->
    minx <= x && x <= maxx && miny <= y && y <= maxy

getBounds : List Position -> Result Bounds _
getBounds = \poss ->
    minx <- poss |> List.map .x |> walkFromFirst Num.min |> Result.map
    miny = poss |> List.map .y |> walkFromFirst Num.min |> okOrCrash "confirmed non-empty"
    maxx = poss |> List.map .x |> walkFromFirst Num.max |> okOrCrash "confirmed non-empty"
    maxy = poss |> List.map .y |> walkFromFirst Num.max |> okOrCrash "confirmed non-empty"

    { minx, miny, maxx, maxy }

expect
    input = [
        { dir: Up, distance: 1, hexColor: "" },
        { dir: Left, distance: 2, hexColor: "" },
        { dir: Down, distance: 2, hexColor: "" },
        { dir: Right, distance: 2, hexColor: "" },
        { dir: Up, distance: 1, hexColor: "" },
    ]
    result = makeBorderList input
    exptResult = [
        { x: 0, y: 1 },
        { x: -1, y: 1 },
        { x: -2, y: 1 },
        { x: -2, y: 0 },
        { x: -2, y: -1 },
        { x: -1, y: -1 },
        { x: 0, y: -1 },
        { x: 0, y: 0 },
    ]
    result == exptResult
makeBorderList : DigPlan -> List Position
makeBorderList = \plan ->
    (borderFinal, _) =
        posFirst = { x: 0, y: 0 }
        (borderMid, posMid), { dir, distance, hexColor: _ } <-
            plan |> List.walk ([], posFirst)

        borderNext =
            makeLine posMid dir (Num.toNat distance)
            |> List.walk borderMid List.append
        posNext = borderNext |> List.last |> Result.withDefault posMid
        (borderNext, posNext)
    borderFinal

makeLine : Position, Direction, Nat -> List Position
makeLine = \{ x, y }, dir, distance ->
    List.range { start: At 1, end: Length distance }
    |> List.map
        (
            when dir is
                Up -> \n -> { x, y: y + n }
                Down -> \n -> { x, y: y - n }
                Left -> \n -> { x: x - n, y }
                Right -> \n -> { x: x + n, y }
        )

getAdjPoss : Position -> List Position
getAdjPoss = \pos ->
    [Up, Down, Left, Right] |> List.map (\dir -> getAdjPos pos dir)

getAdjPos : Position, Direction -> Position
getAdjPos = \{ x, y }, dir ->
    when dir is
        Up -> { x, y: y + 1 }
        Down -> { x, y: y - 1 }
        Left -> { x: x - 1, y }
        Right -> { x: x + 1, y }

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
