interface Solution
    exposes [solve]
    imports [
        ParseInput.{ parse, Module },
    ]

expect
    testInput =
        """
        broadcaster -> a, b, c
        %a -> b
        %b -> c
        %c -> inv
        &inv -> a

        """
    result =
        testInput
        |> parse
        |> Result.map solve
    exptResult = 32000000
    result == Ok exptResult

expect
    testInput =
        """
        broadcaster -> a
        %a -> inv, con
        &inv -> b
        %b -> con
        &con -> output
        """
    result =
        testInput
        |> parse
        |> Result.map solve
    exptResult = 11687500
    result == Ok exptResult

solve : List Module -> U32
solve = \modules ->
    crash "TODO"

# ##############################################################################

walkFromFirst : List a, (a, a -> a) -> Result a [ListWasEmpty]
walkFromFirst = \list, fold ->
    when list is
        [] -> Err ListWasEmpty
        [item, ..] -> Ok (List.walkFrom list 1 item fold)

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

okOr : Result a err, (err -> a) -> a
okOr = \result, xformErr ->
    when result is
        Ok a -> a
        Err e -> xformErr e

okOrTry : Result a err, (err -> Result a err) -> Result a err
okOrTry = \result, xformErr ->
    when result is
        Ok a -> Ok a
        Err e -> xformErr e
