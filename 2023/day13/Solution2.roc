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
    exptResult = 405
    result == Ok exptResult

solve : Patterns -> Nat
solve = \patterns ->
    rowReflsSum =
        patterns
        |> List.joinMap findVerticalRefls
        |> List.sum
    colReflsSum =
        patterns
        |> List.map transpose
        |> List.joinMap findVerticalRefls
        |> List.sum

    rowReflsSum + (100 * colReflsSum)

expect
    pattern =
        testInput
        |> parse
        |> okOrCrash "bad test"
        |> List.first
        |> okOrCrash "bad test"
    result = pattern |> findVerticalRefls
    exptResult = [5]
    result == exptResult
expect
    pattern =
        testInput
        |> parse
        |> okOrCrash "bad test"
        |> List.first
        |> okOrCrash "bad test"
        |> transpose
    result = pattern |> findVerticalRefls
    exptResult = []
    result == exptResult
expect
    pattern =
        testInput
        |> parse
        |> okOrCrash "bad test"
        |> List.get 1
        |> okOrCrash "bad test"
    result = pattern |> findVerticalRefls
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
    result = pattern |> findVerticalRefls
    exptResult = [4]
    result == exptResult
findVerticalRefls : Pattern -> List Nat
findVerticalRefls = \pattern ->
    pattern
    |> List.map findReflsInRow
    |> List.map Set.fromList
    |> walkFromFirst Set.intersection
    |> okOrCrash "TODO handle errors"
    |> Set.toList

findReflsInRow : List Ground -> List Nat
findReflsInRow = \patternRow ->
    len = patternRow |> List.len
    indexMid = len |> Num.divTrunc 2
    patternRowReverse = patternRow |> List.reverse

    frontRefls =
        indexRefl <-
            List.range { start: After 0, end: At indexMid } |> List.keepIf
        sublist = patternRow |> List.takeFirst (2 * indexRefl)
        sublistRev = patternRowReverse |> List.takeLast (2 * indexRefl)
        sublist == sublistRev

    backRefls =
        indexReflReverse <-
            List.range { start: After indexMid, end: Before len } |> List.keepIf
        indexRefl = len - indexReflReverse
        sublist = patternRow |> List.takeLast (2 * indexRefl)
        sublistRev = patternRowReverse |> List.takeFirst (2 * indexRefl)
        sublist == sublistRev

    List.concat frontRefls backRefls

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
