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
    result == Ok 21
expect
    testInput =
        """
        ???.### 1,1,3
        
        """
    result =
        testInput
        |> parse
        |> Result.map solve
    result == Ok 1
solve : Records -> Nat
solve = \records ->
    counts =
        record <- records |> List.map
        countRecordArrangements record.springConditions record.damageStreaks

    counts |> List.sum

countRecordArrangements : List Condition, List Nat -> Nat
countRecordArrangements = \springConditions, damageStreaks ->
    if List.isEmpty springConditions then
        if List.isEmpty damageStreaks then 1 else 0
    else
        when getLeadingCompleteDamageStreak springConditions is
            Streak reducedConds leadingStreak ->
                if Ok leadingStreak == List.first damageStreaks then
                    reducedStreaks = List.dropFirst damageStreaks 1
                    countRecordArrangements reducedConds reducedStreaks
                else
                    0

            NoDamage -> if List.isEmpty damageStreaks then 1 else 0
            StreakPending unknownIndex ->
                conditionsFilledOperational =
                    springConditions |> List.set unknownIndex Operational
                conditionsFilledDamaged =
                    springConditions |> List.set unknownIndex Damaged

                [conditionsFilledOperational, conditionsFilledDamaged]
                |> List.map (\conds -> countRecordArrangements conds damageStreaks)
                |> List.sum

expect
    conditions = [Unknown, Unknown, Unknown, Operational, Damaged, Damaged, Damaged]
    result = getLeadingCompleteDamageStreak conditions
    exptResult = StreakPending 0
    result == exptResult
expect
    conditions = [Operational, Damaged, Damaged, Damaged]
    result = getLeadingCompleteDamageStreak conditions
    exptResult = Streak [] 3
    result == exptResult
expect
    conditions = [Operational, Operational]
    result = getLeadingCompleteDamageStreak conditions
    exptResult = NoDamage
    result == exptResult
getLeadingCompleteDamageStreak : List Condition -> [Streak (List Condition) Nat, StreakPending Nat, NoDamage]
getLeadingCompleteDamageStreak = \conditions ->
    finalState =
        state, (condition, i) <-
            conditions
            |> List.mapWithIndex (\x, i -> (x, i))
            |> List.walkUntil BeforeStreak
        when (state, condition) is
            (_, Unknown) -> i |> EncounteredUnknown |> Break
            (BeforeStreak, Operational) -> Continue BeforeStreak
            (BeforeStreak, Damaged) -> 1 |> MidStreak |> Continue
            (MidStreak len, Damaged) -> len + 1 |> MidStreak |> Continue
            (MidStreak len, Operational) -> (i, len) |> EndOfStreak |> Break
            _ -> crash "nahhhh bro"
    when finalState is
        EndOfStreak (i, len) -> Streak (List.dropFirst conditions i) len
        EncounteredUnknown i -> StreakPending i
        MidStreak len -> Streak [] len
        BeforeStreak -> NoDamage

callCached : Dict state result, state, (state -> result) -> (Dict state result, result)
callCached = \cache, state, fn ->
    when Dict.get cache state is
        Ok value -> (cache, value)
        Err _ ->
            result = fn state
            newCache = Dict.insert cache state result
            (newCache, result)

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
