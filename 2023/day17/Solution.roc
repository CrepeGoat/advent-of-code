interface Solution
    exposes [solve]
    imports [
        ParseInput.{ parse, HeatLossMap },
    ]

TraversalState : { pos : Position, cost : U32, dir : Cardinal, dirStreak : U8 }
CostCache : Dict { pos : Position, dir : Cardinal, dirStreak : U8 } U32
Position : { x : Nat, y : Nat }
Cardinal : [North, South, East, West]

expect
    testInput =
        """
        2413432311323
        3215453535623
        3255245654254
        3446585845452
        4546657867536
        1438598798454
        4457876987766
        3637877979653
        4654967986887
        4564679986453
        1224686865563
        2546548887735
        4322674655533

        """
    result =
        testInput
        |> parse
        |> Result.try solve
    exptResult = 102
    result == Ok exptResult

solve : HeatLossMap -> Result U32 _
solve = \map ->
    bounds = {
        y: List.len map,
        x: List.first map |> okOrCrash "TODO handle empty case" |> List.len,
    }
    targetPos = {
        y: bounds.y |> Num.sub 1,
        x: bounds.x |> Num.sub 1,
    }

    cityStates : CostCache
    cityStates =
        Dict.empty {}
        |> Dict.insert { pos: { x: 0, y: 0 }, dir: South, dirStreak: 0 } 0

    queue : List TraversalState
    queue = [{ pos: { x: 0, y: 0 }, dir: South, dirStreak: 0, cost: 0 }]

    (cityStatesMid, queueMid) <- loop (cityStates, queue)
    when List.first queueMid is
        Err e -> e |> Err |> Break
        Ok state ->
            queueRest = List.dropFirst queueMid 1

            if state.pos == targetPos then
                state.cost |> Ok |> Break
            else
                addedStates =
                    nextPossDirs =
                        dir <-
                            state.dir
                            |> validNextDirs
                            |> List.dropIf (\dir -> state.dirStreak >= 3 && state.dir == dir)
                            |> List.keepOks
                        posToThe bounds state.pos dir
                        |> Result.map (\pos -> (pos, dir))

                    nextPossStates =
                        (pos, dir) <- nextPossDirs |> List.map
                        cityCost =
                            map
                            |> List.get pos.y
                            |> okOrCrash "checked"
                            |> List.get pos.x
                            |> okOrCrash "checked"
                        {
                            pos,
                            cost: state.cost + (Num.toU32 cityCost),
                            dir,
                            dirStreak: if dir == state.dir then state.dirStreak + 1 else 1,
                        }

                    possibleState <- nextPossStates |> List.dropIf
                    key = { pos: possibleState.pos, dir: possibleState.dir, dirStreak: possibleState.dirStreak }
                    when cityStatesMid |> Dict.get key is
                        Ok cost -> cost <= possibleState.cost
                        Err _ -> Bool.false

                nextCityStates =
                    cityStatesMid2, addedState <- addedStates |> List.walk cityStatesMid
                    key = { pos: addedState.pos, dir: addedState.dir, dirStreak: addedState.dirStreak }
                    Dict.insert cityStatesMid2 key addedState.cost
                nextQueue =
                    queueRest
                    |> List.concat addedStates
                    |> List.sortWith compareTraversalStates

                Continue (nextCityStates, nextQueue)

compareTraversalStates =
    state <- makeCmpFunc
    state.cost
    |> Num.toNat
    |> Num.subSaturated (state.pos.x + state.pos.y) # use pos in A* algorithm

validNextDirs : Cardinal -> List Cardinal
validNextDirs = \dir ->
    when dir is
        North -> [North, West, East]
        South -> [South, West, East]
        West -> [North, South, West]
        East -> [North, South, East]

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

makeCmpFunc : (a -> Num b) -> (a, a -> [LT, EQ, GT])
makeCmpFunc = \getMeasure ->
    \x1, x2 -> Num.compare (getMeasure x1) (getMeasure x2)

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
