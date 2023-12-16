interface Solution2
    exposes [solve]
    imports [
        ParseInput.{ parse, Contraption, Tile },
    ]

expect
    testInput =
        """
        .|...\\....
        |.-.\\.....
        .....|-...
        ........|.
        ..........
        .........\\
        ..../.\\\\..
        .-.-/..|..
        .|....-|.\\
        ..//.|....

        """
    result =
        testInput
        |> parse
        |> Result.try solve
    exptResult = 51
    result == Ok exptResult

solve : Contraption -> Result Nat _
solve = \contraption ->
    contraption |> maxEnergized

LightState : { dir : Cardinal, pos : Position }
Position : { x : Nat, y : Nat }
Cardinal : [North, South, East, West]

maxEnergized : Contraption -> Result Nat _
maxEnergized = \contraption ->
    contRow <- contraption |> List.first |> Result.try
    maxX = List.len contRow
    maxY = List.len contraption

    lightStarts = List.join [
        List.range { start: At 0, end: Length maxX } |> List.map (\x -> { pos: { x, y: 0 }, dir: South }),
        List.range { start: At 0, end: Length maxX } |> List.map (\x -> { pos: { x, y: maxY - 1 }, dir: North }),
        List.range { start: At 0, end: Length maxY } |> List.map (\y -> { pos: { x: 0, y }, dir: East }),
        List.range { start: At 0, end: Length maxY } |> List.map (\y -> { pos: { x: maxX - 1, y }, dir: West }),
    ]

    lightStarts
    |> List.map (\light -> calculateEnergized contraption light |> Set.len)
    |> walkFromFirst Num.max

calculateEnergized : Contraption, LightState -> Set Position
calculateEnergized = \contraption, firstLight ->
    allLights =
        (lightsMid, energizedMid) <-
            loop ([firstLight], Set.empty {} |> Set.insert firstLight)

        lights =
            lightsMid
            |> List.keepOks (\light -> advanceLight contraption light)
            |> List.joinMap (\x -> x)
            |> List.dropIf (\light -> Set.contains energizedMid light)
        energized = lights |> List.walk energizedMid Set.insert

        if List.isEmpty lights then
            Break energized
        else
            Continue (lights, energized)

    allLights |> Set.toList |> List.map .pos |> Set.fromList

expect
    contraption = [[Empty, Splitter NorthSouth], [Splitter NorthSouth, Empty]]
    light = { pos: { x: 1, y: 0 }, dir: East }

    result = advanceLight contraption light
    exptResult = Ok [{ pos: { x: 1, y: 1 }, dir: South }]
    result == exptResult
advanceLight : Contraption, LightState -> Result (List LightState) _
advanceLight = \contraption, light ->
    contRow <- List.get contraption light.pos.y |> Result.try
    tile <- List.get contRow light.pos.x |> Result.map

    newDirs = bendLight tile light.dir
    newDir <- newDirs |> List.keepOks
    newPos <- posToThe { y: List.len contraption, x: List.len contRow } light.pos newDir |> Result.map
    { pos: newPos, dir: newDir }

expect
    tile = Splitter NorthSouth
    dir = East

    result = bendLight tile dir
    exptResult = [North, South]
    result == exptResult
bendLight : Tile, Cardinal -> List Cardinal
bendLight = \tile, dir ->
    when (tile, dir) is
        (Splitter NorthSouth, East) | (Splitter NorthSouth, West) -> [North, South]
        (Splitter EastWest, North) | (Splitter EastWest, South) -> [West, East]
        (Mirror NorthwestSoutheast, North) -> [West]
        (Mirror NorthwestSoutheast, West) -> [North]
        (Mirror NorthwestSoutheast, South) -> [East]
        (Mirror NorthwestSoutheast, East) -> [South]
        (Mirror NortheastSouthwest, North) -> [East]
        (Mirror NortheastSouthwest, East) -> [North]
        (Mirror NortheastSouthwest, South) -> [West]
        (Mirror NortheastSouthwest, West) -> [South]
        (_, _) -> [dir]

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
