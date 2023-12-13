interface Solution2
    exposes [solve]
    imports [
        ParseInput.{ parse, Patterns, Pattern, Ground },
    ]

testInput =
    """
    #.##..##.
    ..#.##.#.
    ##......#
    ##......#
    ..#.##.#.
    ..##..##.
    #.#.##.#.

    #...##..#
    #....#..#
    ..##..###
    #####.##.
    #####.##.
    ..##..###
    #....#..#

    """
expect
    result =
        testInput
        |> parse
        |> Result.map solve
    exptResult = 400
    result == Ok exptResult

solve : Patterns -> Nat
solve = \patterns ->
    rowReflsSum =
        patterns
        |> List.joinMap findVerticalSmudgedRefls
        |> List.sum
    colReflsSum =
        patterns
        |> List.map transpose
        |> List.joinMap findVerticalSmudgedRefls
        |> List.sum

    rowReflsSum + (100 * colReflsSum)

expect
    pattern =
        testInput
        |> parse
        |> okOrCrash "bad test"
        |> List.first
        |> okOrCrash "bad test"
    result = pattern |> findVerticalSmudgedRefls
    exptResult = []
    result == exptResult
expect
    pattern =
        testInput
        |> parse
        |> okOrCrash "bad test"
        |> List.first
        |> okOrCrash "bad test"
        |> transpose
    result = pattern |> findVerticalSmudgedRefls
    exptResult = [3]
    result == exptResult
expect
    pattern =
        testInput
        |> parse
        |> okOrCrash "bad test"
        |> List.get 1
        |> okOrCrash "bad test"
    result = pattern |> findVerticalSmudgedRefls
    exptResult = []
    result == exptResult
expect
    pattern =
        testInput
        |> parse
        |> okOrCrash "bad test"
        |> List.get 1
        |> okOrCrash "bad test"
        |> transpose
    result = pattern |> findVerticalSmudgedRefls
    exptResult = [1]
    result == exptResult
findVerticalSmudgedRefls : Pattern -> List Nat
findVerticalSmudgedRefls = \pattern ->
    pattern
    |> List.map countReflMismatchesInRow
    |> walkFromFirst (\acc, x -> List.map2 acc x Num.add)
    |> okOrCrash "TODO handle errors"
    |> List.mapWithIndex (\x, i -> (x, i + 1))
    |> List.keepIf (\(x, i) -> x == 1)
    |> List.map (\(x, i) -> i)

expect
    list = [1, 2, 3, 4, 3, 3, 1]
    result = countReflMismatchesInRow list
    List.len result + 1 == List.len list
expect
    list = [1, 2, 3, 4, 4, 3, 3, 1]
    result = countReflMismatchesInRow list
    exptResult = [1, 2, 3, 1, 3, 1, 1]
    result == exptResult
countReflMismatchesInRow : List a -> List Nat where a implements Eq
countReflMismatchesInRow = \list ->
    len = list |> List.len
    indexMid = len |> Num.divTrunc 2
    listReverse = list |> List.reverse

    frontRefls =
        indexRefl <-
            List.range { start: After 0, end: At indexMid } |> List.map
        sublist = list |> List.takeFirst (2 * indexRefl)
        sublistRev = listReverse |> List.takeLast (2 * indexRefl)

        countDiffs sublist sublistRev |> Num.divTrunc 2

    backRefls =
        indexReflReverse <-
            List.range { start: After indexMid, end: Before len } |> List.map
        indexRefl = len - indexReflReverse
        sublist = list |> List.takeLast (2 * indexRefl)
        sublistRev = listReverse |> List.takeFirst (2 * indexRefl)

        countDiffs sublist sublistRev |> Num.divTrunc 2

    List.concat frontRefls backRefls

expect
    list1 = [1, 1, 1, 1, 1, 1, 1, 1]
    list2 = [1, 1, 0, 1, 1, 1, 1, 0]
    result = countDiffs list1 list2
    exptResult = 2
    result == exptResult
countDiffs = \list1, list2 ->
    List.map2 list1 list2 (\x1, x2 -> if x1 == x2 then 0 else 1) |> List.sum

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
