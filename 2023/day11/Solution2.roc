interface Solution2
    exposes [solve]
    imports [
        ParseInput.{ parse, Image, Body },
    ]

solve = \image -> solveWithExpansion image 1000000

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
expect
    result =
        testInput
        |> parse
        |> Result.map \image -> solveWithExpansion image 2
    result == Ok 374
expect
    result =
        testInput
        |> parse
        |> Result.map \image -> solveWithExpansion image 10
    result == Ok 1030
expect
    result =
        testInput
        |> parse
        |> Result.map \image -> solveWithExpansion image 100
    result == Ok 8410
solveWithExpansion : Image, Nat -> Nat
solveWithExpansion = \image, expansionFactor ->
    imageNums = imageToNat image

    [rowSum, colSum]
    |> List.map (\f -> f imageNums)
    |> List.map \sums -> calculateDistancesBetween sums expansionFactor
    |> List.sum

calculateDistancesBetween : List Nat, Nat -> Nat
calculateDistancesBetween = \line, expansionFactor ->
    lineEnumerate =
        line
        |> List.mapWithIndex (\x, i -> (x, i))
        |> List.keepIf (\(x, i) -> x > 0)
        |> List.mapWithIndex (\(x, i), j -> (x, i, j))
    lineAccumulate =
        acc, x <-
            lineEnumerate
            |> List.map (\(x, i, j) -> x)
            |> List.walk [0]
        next =
            acc
            |> List.last
            |> okOrCrash "starts with one member & grows"
            |> Num.add x
        List.append acc next

    lineSum = lineAccumulate |> List.last |> okOrCrash "has at least one element"

    sums =
        (x1, i1, j1), (x2, i2, j2), acc1 <-
            List.map3
                lineEnumerate
                (List.dropFirst lineEnumerate 1)
                (List.dropFirst lineAccumulate 1)

        countSpace = i2 - i1
        countNonBlankSpace = j2 - j1
        countBlankSpace = countSpace - countNonBlankSpace
        singlePairDistance = countBlankSpace * expansionFactor + countNonBlankSpace

        acc2 = lineSum - acc1

        pairsCount = acc1 * acc2
        (singlePairDistance * pairsCount)
    sums |> List.sum

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
