interface Solution2
    exposes [solve]
    imports [
        ParseInput.{ parse, Image, Body },
    ]

expect
    testInput =
        """
        ...#......
        .......#..
        #.........
        ..........
        ......#...
        .#........
        .........#
        ..........
        .......#..
        #...#.....

        """
    result =
        testInput
        |> parse
        |> Result.map solve
    result == Ok 374
expect
    testInput =
        """
        ...#
        ....
        #...

        """
    result =
        testInput
        |> parse
        |> Result.map solve
    result == Ok 8

solve : Image -> Nat
solve = \image ->
    imageNums = imageToNat image

    [rowSum, colSum]
    |> List.map (\f -> f imageNums)
    |> List.map calculateDistancesBetween
    |> List.sum

calculateDistancesBetween : List Nat -> Nat
calculateDistancesBetween = \line ->
    lineEnumerate =
        line
        |> List.mapWithIndex (\x, i -> (x, i))
        |> List.keepIf (\(x, i) -> x > 0)
        |> List.mapWithIndex (\(x, i), j -> (x, i, j))

    distanceInner, (sum1, i1, j1) <- lineEnumerate |> List.walk 0
    distanceMid, (sum2, i2, j2) <- lineEnumerate |> List.walkFrom (j1 + 1) distanceInner

    singlePairDistance = (i2 - i1) * 2 - (j2 - j1)
    pairsCount = sum1 * sum2
    distanceMid + (singlePairDistance * pairsCount)

expect
    value = [[1, 2, 3], [4, 5, 6]]
    value |> colSum == [5, 7, 9]
colSum : List (List (Num a)) -> List (Num a)
colSum = \listlist ->
    listlist
    |> walkFromFirst (\row1, row2 -> List.map2 row1 row2 Num.add)
    |> Result.withDefault []

expect
    value = [[1, 2, 3], [4, 5, 6]]
    value |> rowSum == [6, 15]
rowSum : List (List (Num a)) -> List (Num a)
rowSum = \listlist ->
    listlist |> List.map List.sum

imageToNat : Image -> List (List Nat)
imageToNat = \image ->
    row <- List.map image
    List.map row bodyToNum

bodyToNum : Body -> Num *
bodyToNum = \body ->
    when body is
        Galaxy -> 1
        Space -> 0

walkFromFirst : List a, (a, a -> a) -> Result a [ListWasEmpty]
walkFromFirst = \list, fold ->
    when list is
        [] -> Err ListWasEmpty
        [item, ..] -> Ok (List.walkFrom list 1 item fold)

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
