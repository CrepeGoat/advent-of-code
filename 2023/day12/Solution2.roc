interface Solution2
    exposes [solve]
    imports [
        ParseInput.{ parse, Records, Record, Condition },
    ]

expect
    testInput =
        """
        ???.### 1,1,3
        .??..??...?##. 1,1,3
        ?#?#?#?#?#?#?#? 1,3,1,6
        ????.#...#... 4,1,1
        ????.######..#####. 1,6,5
        ?###???????? 3,2,1

        """
    result =
        testInput
        |> parse
        |> Result.map solve
    result == Ok 525152
solve : Records -> Nat
solve = \records ->
    counts =
        record <- records |> unfoldRecords |> List.map
        countRecordArrangements record

    counts |> List.sum

unfoldRecords : Records -> Records
unfoldRecords = \records ->
    { conditions, damageStreakLens } <- records |> List.map

    {
        conditions: conditions
        |> List.repeat 5
        |> List.intersperse [Unknown]
        |> List.join,
        damageStreakLens: damageStreakLens |> List.repeat 5 |> List.join,
    }

countRecordArrangements : Record -> Nat
countRecordArrangements = \{ conditions: conditionsOrig, damageStreakLens } ->
    conditions = conditionsOrig |> List.append Operational
    conditionsLen = List.len conditions
    damageStreaksLen = List.len damageStreakLens

    maybeOperational = List.map conditions \cond -> cond != Damaged
    maybeDamaged = List.map conditions \cond -> cond != Operational

    countsFinal =
        countsFirst =
            maybeOperational
            |> accumulateList Bool.true Bool.and
            |> List.dropLast 1
            |> List.map (\b -> if b then 1 else 0)

        prevCounts, damageStreakLen <- damageStreakLens |> List.walk countsFirst
        countsMid, i <-
            List.range { start: At 1, end: At conditionsLen }
            |> List.walk (List.repeat 0 (conditionsLen + 1))

        if maybeOperational |> List.get (i - 1) |> okOrCrash "valid index" |> Bool.not then
            countsMid
        else
            count1 = countsMid |> List.get (i - 1) |> okOrCrash "valid index"
            count2 =
                if (i - 1 >= damageStreakLen) then
                    if
                        (
                            List.sublist
                                maybeDamaged
                                { start: (i - 1) - damageStreakLen, len: damageStreakLen }
                            |> List.all (\b -> b)
                        )
                    then
                        prevCounts |> List.get ((i - 1) - damageStreakLen) |> okOrCrash "valid index"
                    else
                        0
                else
                    0
            countsMid |> List.set i (count1 + count2)

    countsFinal |> List.last |> okOrCrash "has at least one item"

accumulateList : List a, a, (a, a -> a) -> List a
accumulateList = \list, default, merge ->
    acc, x <- list |> List.walk [default]
    next = acc |> List.last |> okOrCrash "starts with one value and grows" |> merge x
    List.append acc next

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
