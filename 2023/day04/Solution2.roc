interface Solution2
    exposes [solve]
    imports [
        ParseInput.{
            parse,
            Card,
        },
    ]

expect
    testInput
    |> parse
    |> Result.map solve
    == Ok 30

solve : List Card -> U32
solve = \cards ->
    cardCopies =
        prevCardCopies, card <- cards |> List.walkBackwards []
        matchesCount = countMatches card
        copiesWonCount = prevCardCopies |> List.takeLast matchesCount |> List.sum

        prevCardCopies |> List.append (copiesWonCount + 1)

    cardCopies |> List.sum

countMatches : Card -> Nat
countMatches = \card ->
    Set.intersection (Set.fromList card.winners) (Set.fromList card.haves)
    |> Set.len

okOrCrash : Result a *, Str -> a
okOrCrash = \result, crashMsg ->
    when result is
        Ok value -> value
        _ -> crash crashMsg

testInput =
    """
    Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11

    """
