interface Solution2
    exposes [solve]
    imports [
        ParseInput.{ parse, TileGrid, Tile, Cardinal },
    ]

expect
    result =
        testInput1
        |> parse
        |> Result.try solve
    result == Ok 4
expect
    result =
        testInput2
        |> parse
        |> Result.try solve
    result == Ok 4
expect
    result =
        testInput3
        |> parse
        |> Result.try solve
    result == Ok 8
expect
    result =
        testInput4
        |> parse
        |> Result.try solve
    result == Ok 10

Graph : Dict Coord (List Coord)
Coord : (Nat, Nat)

solve : TileGrid -> Result Nat _
solve = \grid ->
    (graph, startCoordOrErr) = tileGridToGraph grid
    startCoord <- startCoordOrErr |> Result.try
    filledGrid <- fillInStart grid startCoord |> Result.try

    loopScans <-
        graph
        |> connectStart startCoord
        |> bfs startCoord
        |> Result.map

    loopCoords = loopScans |> List.joinMap (\x -> x) |> Set.fromList
    enclosedTiles filledGrid loopCoords |> List.len

enclosedTiles : TileGrid, Set Coord -> List Coord
enclosedTiles = \grid, loopCoords ->
    resultInner, (row, i) <-
        grid
        |> List.mapWithIndex (\row, i -> (row, i))
        |> List.walk []

    (result, _, _) =
        (resultMid, prevNorthsCount, prevSouthsCount), (tile, j) <-
            row
            |> List.mapWithIndex (\tile, j -> (tile, j))
            |> List.walk (resultInner, 0, 0)

        coord = (i, j)

        if Set.contains loopCoords coord then
            when tile is
                PIPE (way1, way2) ->
                    northsCount = [way1, way2] |> List.countIf (\c -> c == NORTH)
                    southsCount = [way1, way2] |> List.countIf (\c -> c == SOUTH)
                    (resultMid, prevNorthsCount + northsCount, prevSouthsCount + southsCount)

                _ -> (resultMid, prevNorthsCount, prevSouthsCount)
        else if prevNorthsCount % 2 == 1 || prevSouthsCount % 2 == 1 then
            (resultMid |> List.append coord, prevNorthsCount, prevSouthsCount)
        else
            (resultMid, prevNorthsCount, prevSouthsCount)

    result

bfs : Graph, Coord -> Result (List (List Coord)) [NoCoordInGraph]
bfs = \graph, coord ->
    startCoordEdges <-
        graph
        |> Dict.get coord
        |> Result.mapErr (\_ -> NoCoordInGraph)
        |> Result.map
    (resultMid, visited) <- loop ([[coord]], {} |> Set.empty |> Set.insert coord)
    prevEdges = resultMid |> List.last |> okOrCrash "starts non-empty & grows"

    nextEdges =
        prevEdges
        |> List.keepOks \edgeCoord -> Dict.get graph edgeCoord
        |> List.joinMap (\x -> x)
        |> List.keepIf (\edgeCoord -> visited |> Set.contains edgeCoord |> Bool.not)

    when nextEdges is
        [] -> Break resultMid
        _ ->
            nextResult = resultMid |> List.append nextEdges
            nextVisited =
                visitedMid, edgeCoord <- List.walk nextEdges visited
                Set.insert visitedMid edgeCoord
            Continue (nextResult, nextVisited)

connectStart : Graph, Coord -> Graph
connectStart = \graph, startCoord ->
    graphMid, adjCoord <-
        graph
        |> Dict.toList
        |> List.keepIf (\(coord, edgesOut) -> List.contains edgesOut startCoord)
        |> List.map (\(coord, edgesOut) -> coord)
        |> List.walk graph

    graphEntry <- graphMid |> Dict.update startCoord
    prevEdges =
        when graphEntry is
            Missing -> []
            Present s -> s
    prevEdges |> List.append adjCoord |> Present

tileGridToGraph : TileGrid -> (Graph, Result Coord [NoStartTile, MultipleStartTiles Coord Coord])
tileGridToGraph = \grid ->
    (graphInner, startCoordInner), (row, i) <-
        grid
        |> List.mapWithIndex (\row, i -> (row, i))
        |> List.walk (Dict.empty {}, Err NoStartTile)
    (graphMid, startCoordMid), (tile, j) <-
        row
        |> List.mapWithIndex (\tile, j -> (tile, j))
        |> List.walk (graphInner, startCoordInner)

    coord = (Num.toNat i, Num.toNat j)
    when tile is
        GROUND -> (graphMid, startCoordMid)
        START ->
            startCoordMidResult =
                when startCoordMid is
                    Err NoStartTile -> Ok coord
                    Ok sc1 -> MultipleStartTiles sc1 coord |> Err
                    other -> other
            (graphMid, startCoordMidResult)

        PIPE (way1, way2) ->
            graphMidResult =
                graphMid2, adjCoord <-
                    [way1, way2]
                    |> List.keepOks \way -> mapToAdj coord (List.len grid, List.len row) way
                    |> List.walk graphMid
                graphEntry <- graphMid2 |> Dict.update coord
                edges =
                    when graphEntry is
                        Missing -> []
                        Present s -> s
                edges |> List.append adjCoord |> Present
            (graphMidResult, startCoordMid)

fillInStart : TileGrid, Coord -> Result TileGrid _
fillInStart = \grid, startCoord ->
    (i, j) = startCoord

    when getAdjPipeDirections grid startCoord is
        [way1, way2] ->
            row = grid |> List.get i |> okOrCrash "should exist"
            newRow = row |> List.set j (PIPE (way1, way2))

            grid |> List.set i newRow |> Ok

        _ -> Err PipeWithNotTwoConnections

getAdjPipeDirections : TileGrid, Coord -> List Cardinal
getAdjPipeDirections = \grid, startCoord ->
    gridDims = dims grid

    (way, wayBack) <-
        [(NORTH, SOUTH), (EAST, WEST), (SOUTH, NORTH), (WEST, EAST)]
        |> List.keepOks

    (adjI, adjJ) <- mapToAdj startCoord gridDims way |> Result.try
    adjRow <- grid |> List.get adjI |> Result.try
    adjTile <- adjRow |> List.get adjJ |> Result.try

    when adjTile is
        PIPE (wb, _) | PIPE (_, wb) if wb == wayBack -> Ok way
        _ -> Err Whatever

findStartCoord : TileGrid -> Result Coord _
findStartCoord = \grid ->
    result =
        (tile, (i, j)) <- grid |> tileCoords |> List.findFirst
        tile == START
    result
    |> Result.map \(_, coord) -> coord
    |> Result.mapErr \_ -> Err [NoStartTile]

tileCoords : TileGrid -> List (Tile, Coord)
tileCoords = \grid ->
    (row, i) <-
        grid
        |> List.mapWithIndex (\row, i -> (row, i))
        |> List.joinMap
    row |> List.mapWithIndex (\tile, j -> (tile, (i, j)))

dims : List (List a) -> (Nat, Nat)
dims = \listlist ->
    when listlist is
        [] -> (0, 0)
        [list, ..] -> (List.len listlist, List.len list)

mapToAdj : Coord, Coord, Cardinal -> Result Coord _
mapToAdj = \(i, j), (boundX, boundY), way ->
    when way is
        NORTH -> i |> Num.subChecked 1 |> Result.map \newI -> (newI, j)
        WEST -> j |> Num.subChecked 1 |> Result.map \newJ -> (i, newJ)
        SOUTH -> if i + 1 < boundX then Ok (i + 1, j) else Err OutOfBounds
        EAST -> if j + 1 < boundY then Ok (i, j + 1) else Err OutOfBounds

loop : state, (state -> [Break b, Continue state]) -> b
loop = \stateStart, stateStep ->
    when stateStep stateStart is
        Continue state -> loop state stateStep
        Break result -> result

okOrCrash : Result a *, Str -> a
okOrCrash = \result, crashMsg ->
    when result is
        Ok value -> value
        _ -> crash crashMsg

testInput1 =
    """
    ...........
    .S-------7.
    .|F-----7|.
    .||.....||.
    .||.....||.
    .|L-7.F-J|.
    .|..|.|..|.
    .L--J.L--J.
    ...........

    """

testInput2 =
    """
    ..........
    .S------7.
    .|F----7|.
    .||....||.
    .||....||.
    .|L-7F-J|.
    .|..||..|.
    .L--JL--J.
    ..........

    """

testInput3 =
    """
    .F----7F7F7F7F-7....
    .|F--7||||||||FJ....
    .||.FJ||||||||L7....
    FJL7L7LJLJ||LJ.L-7..
    L--J.L7...LJS7F-7L7.
    ....F-J..F7FJ|L7L7L7
    ....L7.F7||L7|.L7L7|
    .....|FJLJ|FJ|F7|.LJ
    ....FJL-7.||.||||...
    ....L---J.LJ.LJLJ...

    """

testInput4 =
    """
    FF7FSF7F7F7F7F7F---7
    L|LJ||||||||||||F--J
    FL-7LJLJ||||||LJL-77
    F--JF--7||LJLJ7F7FJ-
    L---JF-JLJ.||-FJLJJ7
    |F|F-JF---7F7-L7L|7|
    |FFJF7L7F-JF7|JL---7
    7-L-JL7||F7|L7F-7F7|
    L.L7LFJ|||||FJL7||LJ
    L7JLJL-JLJLJL--JLJ.L

    """
