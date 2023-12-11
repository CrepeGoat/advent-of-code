interface Solution
    exposes [solve]
    imports [
        ParseInput.{ parse, TileGrid, Cardinal },
    ]

expect
    testInput1
    |> parse
    |> Result.try solve
    == Ok 4
expect
    testInput2
    |> parse
    |> Result.try solve
    == Ok 4
expect
    testInput3
    |> parse
    |> Result.try solve
    == Ok 8
expect
    testInput4
    |> parse
    |> Result.try solve
    == Ok 8

Graph : Dict Coord (List Coord)
Coord : (Nat, Nat)

solve : TileGrid -> Result Nat _
solve = \grid ->
    (graph, startCoordOrErr) = grid |> tileGridToGraph
    startCoord <- startCoordOrErr |> Result.try

    scans <-
        graph
        |> connectStart startCoord
        |> bfs startCoord
        |> Result.map

    scans |> List.len |> Num.sub 1

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
    .....
    .S-7.
    .|.|.
    .L-J.
    .....

    """

testInput2 =
    """
    -L|F7
    7S-7|
    L|7||
    -L-J|
    L|-JF

    """

testInput3 =
    """
    ..F7.
    .FJ|.
    SJ.L7
    |F--J
    LJ...

    """

testInput4 =
    """
    7-F7-
    .FJ|7
    SJLL7
    |F--J
    LJ.LJ

    """
