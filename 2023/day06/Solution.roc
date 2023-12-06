interface Solution
    exposes [solve]
    imports [
        ParseInput.{ parse, Document },
    ]

expect
    testInput
    |> parse
    |> Result.map solve
    == Ok 288

solve : Document -> U32
solve = \document ->
    List.map2 document.times document.distances countWinStrats
    |> List.product

countWinStrats : U32, U32 -> U32
countWinStrats = \timeLimit, minDistance ->
    t1Underestimate =
        timeLimit
        - (sqrtCeil (timeLimit * timeLimit - 4 * minDistance))
        |> Num.divTrunc 2

    t1OrNone =
        t <-
            List.range {
                start: At t1Underestimate,
                end: At (Num.divCeil timeLimit 2),
            }
            |> List.findFirst
        calculateDistance { time: t, timeLimit } > minDistance

    when t1OrNone is
        Ok value -> timeLimit + 1 - 2 * value
        Err _ -> timeLimit + 1

expect
    233
    * 233
    + 5
    |> sqrtCeil
    == 234

sqrtCeil : U32 -> U32
sqrtCeil = \n ->
    if n == 0 then 0 else n |> Num.sub 1 |> sqrtTrunc |> Num.add 1

expect
    233
    * 233
    + 5
    |> sqrtTrunc
    == 233

# https://en.wikipedia.org/wiki/Integer_square_root#Digit-by-digit_algorithm
sqrtTrunc : U32 -> U32
sqrtTrunc = \n ->
    if n <= 1 then
        n
    else
        rootGuess = n |> Num.divTrunc 4 |> sqrtTrunc |> Num.mul 2
        rootGuess2 = rootGuess + 1

        if rootGuess2 * rootGuess2 <= n then rootGuess2 else rootGuess

calculateDistance : { time : U32, timeLimit : U32 } -> U32
calculateDistance = \{ time, timeLimit } ->
    time * (timeLimit - time)

okOrCrash : Result a *, Str -> a
okOrCrash = \result, crashMsg ->
    when result is
        Ok value -> value
        _ -> crash crashMsg

testInput =
    """
    Time:      7  15   30
    Distance:  9  40  200

    """
