interface Solution
    exposes [solve]
    imports [
        ParseInput.{ parse, Documents },
    ]

expect
    testInput1
    |> parse
    |> Result.try solve
    == Ok 2

expect
    testInput2
    |> parse
    |> Result.try solve
    == Ok 6

solve : Documents -> Result Nat [MissingNode Str]
solve = \docs ->
    network = Dict.fromList docs.network

    (prevNode, i), step <-
        docs.turns
        |> walkListCircleUntil ("AAA", 0)

    fork =
        network
        |> Dict.get prevNode
        |> Result.mapErr \_ -> MissingNode prevNode
    nextNodeOrErr =
        when step is
            LEFT -> fork |> Result.map .left
            RIGHT -> fork |> Result.map .right
    when nextNodeOrErr is
        Ok "ZZZ" -> Break (Ok (i + 1))
        Err e -> Break (Err e)
        Ok node -> Continue (node, i + 1)

walkListCircleUntil : List a, state, (state, a -> [Break b, Continue state]) -> b
walkListCircleUntil = \list, initState, func ->
    (prevList, prevState) <- while (list, initState)

    nextList = List.dropFirst prevList 1
    when prevList is
        [item, ..] ->
            when func prevState item is
                Continue nextState -> Continue (nextList, nextState)
                Break b -> Break b

        [] -> Continue (list, prevState)

while : state, (state -> [Break b, Continue state]) -> b
while = \stateStart, stateStep ->
    when stateStep stateStart is
        Continue state -> while state stateStep
        Break result -> result

okOrCrash : Result a *, Str -> a
okOrCrash = \result, crashMsg ->
    when result is
        Ok value -> value
        _ -> crash crashMsg

testInput1 =
    """
    RL

    AAA = (BBB, CCC)
    BBB = (DDD, EEE)
    CCC = (ZZZ, GGG)
    DDD = (DDD, DDD)
    EEE = (EEE, EEE)
    GGG = (GGG, GGG)
    ZZZ = (ZZZ, ZZZ)

    """

testInput2 =
    """
    LLR

    AAA = (BBB, BBB)
    BBB = (AAA, ZZZ)
    ZZZ = (ZZZ, ZZZ)

    """
