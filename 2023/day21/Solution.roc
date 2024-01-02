interface Solution
    exposes [solve]
    imports [
        ParseInput.{ parse, Map, Tile },
    ]

Position : { x : Nat, y : Nat }
Cardinal : [North, South, East, West]

expect
    testInput =
        """
        ...........
        .....###.#.
        .###.##..#.
        ..#.#...#..
        ....#.#....
        .##..S####.
        .##..#...#.
        .......##..
        .##.#.####.
        .##..##.##.
        ...........

        """
    result =
        testInput
        |> parse
        |> Result.try (\x -> solve x 6)
    exptResult = 16
    result == Ok exptResult
solve : Map, Nat -> Result Nat [GridWasEmpty, StartNotPresent]
solve = \map, stepCount ->
    _bounds <- getBounds map |> Result.try
    posStart <- getMapStart map |> Result.map

    postionsFinal =
        positionsMid, _loopCount <-
            List.range { start: At 0, end: Length stepCount }
            |> List.walk [posStart]
        bfsStep map positionsMid
        |> okOrCrash "already checked map has bounds"
        |> Set.toList

    List.len postionsFinal

bfsStep : Map, List Position -> Result (Set Position) [GridWasEmpty]
bfsStep = \map, poss ->
    resultInit = Set.empty {}
    resultMid, pos <- poss |> List.walkTry resultInit
    posAdjs <- bfsFromPos map pos |> Result.map
    posAdjs |> List.walk resultMid Set.insert

bfsFromPos : Map, Position -> Result (List Position) [GridWasEmpty]
bfsFromPos = \map, pos ->
    bounds <- getBounds map |> Result.map
    { x, y } <- getPosAdjs bounds pos |> List.dropIf

    map
    |> List.get y
    |> okOrCrash "already checked pos is within bounds"
    |> List.get x
    |> okOrCrash "already checked pos is within bounds"
    == Rock

getMapStart : Map -> Result Position [StartNotPresent]
getMapStart = \map ->
    rows <- loop map
    when rows is
        [] -> StartNotPresent |> Err |> Break
        [.., lastRow] ->
            rowsRest = rows |> List.dropLast 1
            y = rows |> List.len |> Num.sub 1
            when lastRow |> List.findFirstIndex (\tile -> tile == Start) is
                Ok x -> { x, y } |> Ok |> Break
                Err _ -> Continue rowsRest

getBounds : Map -> Result Position [GridWasEmpty]
getBounds = \map ->
    when map is
        [row1, ..] -> Ok { x: List.len row1, y: List.len map }
        [] -> Err GridWasEmpty

# ##############################################################################

getPosAdjs : Position, Position -> List Position
getPosAdjs = \bounds, pos ->
    [North, South, East, West]
    |> List.keepOks (\dir -> posToThe bounds pos dir)

posToThe : Position, Position, Cardinal -> Result Position [OutOfBounds]
posToThe = \{ x: boundX, y: boundY }, { x, y }, dir ->
    when dir is
        North ->
            newY <- Num.subChecked y 1 |> Result.mapErr (\_ -> OutOfBounds) |> Result.map
            { y: newY, x }

        South ->
            newY <- Num.addChecked y 1 |> Result.mapErr (\_ -> OutOfBounds) |> Result.try
            if newY >= boundY then Err OutOfBounds else Ok { y: newY, x }

        West ->
            newX <- Num.subChecked x 1 |> Result.mapErr (\_ -> OutOfBounds) |> Result.map
            { y, x: newX }

        East ->
            newX <- Num.addChecked x 1 |> Result.mapErr (\_ -> OutOfBounds) |> Result.try
            if newX >= boundX then Err OutOfBounds else Ok { y, x: newX }

# ##############################################################################

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

