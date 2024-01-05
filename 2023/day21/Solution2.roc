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
countFillInSouthEastFromStart = \vmap, stepCount ->
    @ValidatedMap{start, bounds, map} = vmap

    stepsToSouth = countStepsToCrossBorder bounds start South
    posNorthEntrance = getCardinalPos vmap North
    stepsToSouthFromNorth =
        countStepsToCrossBorder bounds posNorthEntrance South
    
    stepsToEast = countStepsToCrossBorder bounds posNorthEntrance East
    posNorthwestEntrance = {x: 0, y: 0}
    stepsToEastFromNorthwest =
        countStepsToCrossBorder bounds posNorthwestEntrance East

    if stepCount < stepsToSouth then
        0
    else
        (stepsLeftMid, fillCountMid) <- loop (stepCount - stepsToSouth, 0)
        
        fillCountHere =
            countSingleFillAfterSteps vmap posNorthEntrance stepsLeftMid
        
        fillCountTotalEast =
            if stepsLeftMid < stepsToEast then
                0
            else
                (stepsLeftMid2, fillCountMid2) <- loop (stepCount - stepsToEast, 0)
                fillCountHere2 =
                    countSingleFillAfterSteps vmap posNorthwestEntrance stepsLeftMid2
                fillCountNext2 = fillCountMid2 + fillCountHere2

                if stepsLeftMid2 < stepsToEastFromNorthwest then
                    Break fillCountNext2
                else
                    Continue (stepsLeftMid2 - stepsToEastFromNorthwest, fillCountNext2)

        
        fillCountNext = fillCountMid + fillCountHere + fillCountTotalEast
        if stepsLeftMid < stepsToSouthFromNorth then
            Break fillCountNext
        else
            Continue (stepsLeftMid - stepsToSouthFromNorth, fillCountNext)


countCenterFill : ValidatedMap, Nat -> Nat
countCenterFill = \vmap, stepCount ->
    @ValidatedMap {start, map: _, bounds: _} = vmap
    countSingleFillAfterSteps vmap start stepCount

rotateMap : ValidatedMap -> ValidatedMap
rotateMap = \vmap -> vmap |> transposeMap |> flipMapAlongY

transposeMap = \@ValidatedMap{map, start, bounds} ->
    @ValidatedMap {start: {x: start.y, y: start.x}, bounds: {x: bounds.y, y: bounds.x}, map: transpose map}



flipMapAlongY = \@ValidatedMap{map, start, bounds} ->
    @ValidatedMap{
        bounds,
        start: {start & y: bounds.y - 1 - start.y},
        map: List.reverse map
    }

countStepsToCrossBorder : MapIndex, MapIndex, ([South, East]) -> Nat
countStepsToCrossBorder = \bounds, {x, y}, dirToBorder ->
    when dirToBorder is
        East -> bounds.x - x
        South -> bounds.y - y

countSingleFillAfterSteps : ValidatedMap, MapIndex, Nat -> Nat
countSingleFillAfterSteps = \vmap, posStart, stepCount ->
    (loopCount, nextPositions, positionSetNext, positionSetMid, positionSetPrev) <-
    bfsHelper vmap posStart

    if loopCount == stepCount then
        positionSetMid |> Set.len |> Break
    else if nextPositions |> List.isEmpty then
        (
            if loopCount % 2 == stepCount % 2 then
                positionSetMid
            else positionSetPrev
        )
        |> Set.len |> Break
    else
        Continue

countFillsAndStepsToFill : ValidatedMap, MapIndex -> ({ even : Nat, odd : Nat }, Nat)
countFillsAndStepsToFill = \vmap, posStart ->
    (loopCount, nextPositions, positionSetNext, positionSetMid, positionSetPrev) <-
    bfsHelper vmap posStart

    if List.isEmpty nextPositions then
        fillCount = positionSetMid |> Set.len
        prevFillCount = positionSetPrev |> Set.len
        fillCounts =
            if loopCount % 2 == 1 then
                {odd: fillCount, even: prevFillCount}
            else
                {even: fillCount, odd: prevFillCount}
        Break (fillCounts, loopCount)
    else Continue

getCardinalPos : ValidatedMap, Cardinal -> MapIndex
getCardinalPos = \@ValidatedMap {map, start, bounds}, dir ->
    when dir is
        North -> {start & y: 0}
        South -> {start & y: bounds.y - 1}
        West -> {start & x: 0}
        East -> {start & x: bounds.x - 1}

# ##############################################################################

bfsHelper : ValidatedMap, MapIndex, _ -> _
bfsHelper = \vmap, posStart, innerFunc ->
    (loopCount, positionsMid, positionSetMid, positionSetPrev) <-
        loop (
            0
            [posStart],
            Set.empty {} |> Set.insert posStart,
            Set.empty {},
        )
    nextPositions =
        bfsStep map positionsMid
        |> okOrCrash "already checked map has bounds"
        |> Set.toList
        |> List.dropIf (\p -> Set.contains positionSetPrev p)
    positionSetNext = nextPositions |> List.walk positionSetPrev Set.insert

    when innerFunc
        loopCount nextPositions positionSetNext positionSetMid positionSetPrev
    is
        Continue ->
            (loopCount + 1, nextPositions, positionSetNext, positionSetMid)
            |> Continue
        Break x -> Break x

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

