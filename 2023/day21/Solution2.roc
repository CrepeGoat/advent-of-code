interface Solution2
    exposes [solve]
    imports [
        ParseInput.{ parse, Map, Tile },
    ]

ValidatedMap := {map: Map, bounds: MapIndex, start: MapIndex}

validateMap : Map
    -> Result
        ValidatedMap
        [
            GridWasEmpty,
            StartNotPresent,
            CardinalPathsFromStartAreBlocked,
            PerimeterPathIsBlocked,
        ]
validateMap = \map ->
    bounds <- getBounds map |> Result.try
    start <- getMapStart map |> Result.try

    # Check that cardinal tiles from start are all plots
    isValid1 = 
        List.range { start: At 0, end: Length bounds.y }
        |> List.map (\y -> { start & y })
        |> List.all (\pos -> getTile map pos |> okOrCrash "is valid pos" != Rock)

    resultMid1 = if isValid1 then Ok {} else
        Err CardinalPathsFromStartAreBlocked
    _ <- resultMid1 |> Result.try

    isValid2 = 
        List.range { start: At 0, end: Length bounds.x }
        |> List.map (\x -> { start & x })
        |> List.all (\pos -> getTile map pos |> okOrCrash "is valid pos" != Rock)

    resultMid2 = if isValid2 then Ok {} else
        Err CardinalPathsFromStartAreBlocked
    _ <- resultMid2 |> Result.try

    # Check that perimeter tiles are all plots
    isValid3 = 
        List.range { start: At 0, end: Length bounds.y }
        |> List.joinMap (\y -> [{ y, x: 0 }, {y, x: bounds.x - 1}])
        |> List.all (\pos -> getTile map pos |> okOrCrash "is valid pos" != Rock)

    resultMid3 = if isValid3 then Ok {} else
        Err PerimeterPathIsBlocked
    _ <- resultMid3 |> Result.try
        
    isValid4 = 
        List.range { start: At 0, end: Length bounds.x }
        |> List.joinMap (\x -> [{ x, y: 0 }, {x, y: bounds.y - 1}])
        |> List.all (\pos -> getTile map pos |> okOrCrash "is valid pos" != Rock)

    resultMid4 = if isValid4 then Ok {} else
        Err PerimeterPathIsBlocked
    _ <- resultMid4 |> Result.try

    {start, bounds, map} |> @ValidatedMap |> Ok


Position : { x : I32, y : I32 }
MapIndex : { x : Nat, y : Nat }
Cardinal : [North, South, East, West]

mapIndexToPos : MapIndex -> Position
mapIndexToPos = \{ x, y } -> { x: Num.toI32 x, y: Num.toI32 y }

posToMapIndex : Position -> MapIndex
posToMapIndex = \{ x, y } -> { x: Num.toNat x, y: Num.toNat y }

testInput =
    """
    ...........
    ......##.#.
    .###..#..#.
    ..#.#...#..
    ....#.#....
    .....S.....
    .##......#.
    .......##..
    .##.#.####.
    .##...#.##.
    ...........

    """
    |> parse
    |> okOrCrash "broken test input"
expect
    stepCount = 6
    result = testInput |> solve stepCount
    result == Ok 36
expect
    stepCount = 10
    result = testInput |> solve stepCount
    result == Ok 90
expect
    stepCount = 50
    result = testInput |> solve stepCount
    result == Ok 1940
expect
    stepCount = 100
    result = testInput |> solve stepCount
    result == Ok 7645
expect
    stepCount = 500
    result = testInput |> solve stepCount
    result == Ok 188756
expect
    stepCount = 1000
    result = testInput |> solve stepCount
    result == Ok 753480
expect
    stepCount = 5000
    result = testInput |> solve stepCount
    result == Ok 18807440
solve : Map, Nat -> Result Nat _
solve = \mapRaw, stepCount ->
    vmap <- validateMap mapRaw |> Result.map
    countTotalFillFromStart vmap stepCount

countTotalFillFromStart : ValidatedMap, Nat -> Nat
countTotalFillFromStart = \vmap, stepCount ->
    centerFill = countCenterFill vmap stepCount
    (_, fillToEdges) = 
        (vm, fillMid), _ <- [0, 1, 2, 3] |> List.walk (vmap, 0)

        (
            vm |> rotateMap,
            fillMid + countFillInSouthEastFromStart vm stepCount,
        )
    centerFill + fillToEdges

countFillInSouthEastFromStart : ValidatedMap, Nat -> Nat

countCenterFill : ValidatedMap, Nat -> Nat

rotateMap : ValidatedMap -> ValidatedMap

countStepsToCrossBorder : ValidatedMap, MapIndex, ([North, South, Middle], [East, West, Middle]) -> Nat

countFillsAndStepsToFill : ValidatedMap, MapIndex -> ({ even : Nat, odd : Nat }, Nat)

countSingleFillAfterSteps : ValidatedMap, MapIndex, Nat -> Nat

getCardinalPos : ValidatedMap, Cardinal -> MapIndex

bfsStep : ValidatedMap, List Position -> Set Position
bfsStep = \vmap, poss ->
    resultInit = Set.empty {}
    resultMid, pos <- poss |> List.walk resultInit
    posAdjs = bfsFromPos vmap pos
    posAdjs |> List.walk resultMid Set.insert

bfsFromPos : ValidatedMap, Position -> List Position
bfsFromPos = \@ValidatedMap {map, bounds, start: _}, pos ->
    { x, y } <- getPosAdjs bounds pos |> List.dropIf

    map
    |> getTile {
        x: x |> remEuclid (bounds.x |> Num.toI32) |> Num.toNat,
        y: y |> remEuclid (bounds.y |> Num.toI32) |> Num.toNat,
    }
    |> okOrCrash "already checked pos is within bounds"
    == Rock

getTile : Map, MapIndex -> Result Tile _
getTile = \map, { x, y } ->
    List.get map y |> Result.try (\row -> row |> List.get x)

getMapStart : Map -> Result MapIndex [StartNotPresent]
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

getBounds : Map -> Result MapIndex [GridWasEmpty]
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

getPosAdjs : MapIndex, Position -> List Position
getPosAdjs = \bounds, pos ->
    [North, South, East, West] |> List.map (\dir -> posToThe bounds pos dir)

posToThe : MapIndex, Position, Cardinal -> Position
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

