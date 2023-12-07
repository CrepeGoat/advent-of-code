interface Solution
    exposes [solve]
    imports [
        ParseInput.{ parse, HandBid, Card },
    ]

expect
    testInput
    |> parse
    |> Result.map solve
    == Ok 6440

solve : List HandBid -> U32
solve = \handsBids ->
    handsBids
    |> sortHandsBidsByRanks
    |> List.map2
        (List.range { start: At 1, end: handsBids |> List.len |> At } |> List.map Num.toU32)
        \handBid, rank -> rank * Num.toU32 handBid.bid
    |> List.sum

sortHandsBidsByRanks : List HandBid -> List HandBid
sortHandsBidsByRanks = \handsBids ->
    handsBids |> List.sortWith \hb1, hb2 -> compareHands hb1.hand hb2.hand

compareHands : List Card, List Card -> [LT, EQ, GT]
compareHands = \hand1, hand2 ->
    when Num.compare (calculateHandType hand1) (calculateHandType hand2) is
        EQ ->
            compareElementwise
                (List.map hand1 cardToNum)
                (List.map hand2 cardToNum)
                Num.compare

        neq -> neq

calculateHandType : List Card -> U8
calculateHandType = \cards ->
    cardCountsSorted = cards |> countItems |> Dict.values |> List.sortDesc
    # expect
    #     List.sum cardCountsSorted == 5

    when cardCountsSorted is
        [1, ..] -> 0
        [2, 1, ..] -> 1
        [2, 2, ..] -> 2
        [3, 1, ..] -> 3
        [3, 2] -> 4
        [4, ..] -> 5
        [5] -> 6
        _ -> crash "list of card counts must sum to 5"

countItems : List a -> Dict a Nat
countItems = \items ->
    itemCounts, item <- items |> List.walk (Dict.empty {})
    currentItemCount <- Dict.update itemCounts item

    result =
        when currentItemCount is
            Present count -> Present (count + 1)
            Missing -> Present 1

    # expect
    #     result |> Dict.values |> List.sum == items |> List.len
    result

compareElementwise : List a, List a, (a, a -> [LT, EQ, GT]) -> [LT, EQ, GT]
compareElementwise = \list1, list2, cmp ->
    when List.map2 list1 list2 cmp |> List.findFirst \rel -> rel != EQ is
        Ok rel -> rel
        Err _ -> EQ

cardToNum = \card ->
    when card is
        ACE -> 14
        KING -> 13
        QUEEN -> 12
        JACK -> 11
        TEN -> 10
        NINE -> 9
        EIGHT -> 8
        SEVEN -> 7
        SIX -> 6
        FIVE -> 5
        FOUR -> 4
        THREE -> 3
        TWO -> 2

okOrCrash : Result a *, Str -> a
okOrCrash = \result, crashMsg ->
    when result is
        Ok value -> value
        _ -> crash crashMsg

testInput =
    """
    32T3K 765
    T55J5 684
    KK677 28
    KTJJT 220
    QQQJA 483

    """
