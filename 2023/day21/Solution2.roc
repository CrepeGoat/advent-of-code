interface Solution2
    exposes [solve]
    imports [
        ParseInput.{ parse, Map, Tile },
    ]

Position : { x : I32, y : I32 }
Bounds : { x : Nat, y : Nat }
Cardinal : [North, South, East, West]

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
    |> parse
    |> okOrCrash "broken test input"
expect
    stepCount = 6
    result = testInput |> solve stepCount
    result == Ok 16
expect
    stepCount = 10
    result = testInput |> solve stepCount
    result == Ok 50
expect
    stepCount = 50
    result = testInput |> solve stepCount
    result == Ok 1594
expect
    stepCount = 100
    result = testInput |> solve stepCount
    result == Ok 6536
expect
    stepCount = 500
    result = testInput |> solve stepCount
    result == Ok 167004
expect
    stepCount = 1000
    result = testInput |> solve stepCount
    result == Ok 668697
expect
    stepCount = 5000
    result = testInput |> solve stepCount
    result == Ok 16733044
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
    |> List.get (y |> remEuclid (bounds.y |> Num.toI32) |> Num.toNat)
    |> okOrCrash "already checked pos is within bounds"
    |> List.get (x |> remEuclid (bounds.x |> Num.toI32) |> Num.toNat)
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
                Ok x -> { x: x |> Num.toI32, y: y |> Num.toI32 } |> Ok |> Break
                Err _ -> Continue rowsRest

getBounds : Map -> Result Bounds [GridWasEmpty]
getBounds = \map ->
    when map is
        [row1, ..] -> Ok { x: List.len row1, y: List.len map }
        [] -> Err GridWasEmpty

# ##############################################################################

expect remEuclid 10 3 == 1
expect remEuclid -10 3 == 2
remEuclid : Int a, Int a -> Int a
remEuclid = \x, mod ->
    x - (x |> divFloor mod |> Num.mul mod)

divFloor : Int a, Int a -> Int a
divFloor = \x, d ->
    x |> Num.neg |> Num.divCeil d |> Num.neg

getPosAdjs : Bounds, Position -> List Position
getPosAdjs = \bounds, pos ->
    [North, South, East, West] |> List.map (\dir -> posToThe bounds pos dir)

posToThe : Bounds, Position, Cardinal -> Position
posToThe = \{ x: boundX, y: boundY }, { x, y }, dir ->
    when dir is
        North -> { y: y - 1, x }
        South -> { y: y + 1, x }
        West -> { y, x: x - 1 }
        East -> { y, x: x + 1 }

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

