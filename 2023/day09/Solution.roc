interface Solution
    exposes [solve]
    imports [
        ParseInput.{ parse, Histories },
    ]

expect
    testInput
    |> parse
    |> Result.try solve
    == Ok 114

solve : Histories -> Result I32 [ListWasEmpty]
solve = \histories ->
    histories |> List.mapTry extrapolate |> Result.map List.sum

extrapolate : List I32 -> Result I32 [ListWasEmpty]
extrapolate = \nums ->
    numLast <- nums |> List.last |> Result.try

    if List.all nums (\num -> num == 0) then
        Ok 0
    else
        nums |> diff |> extrapolate |> Result.map \delta -> delta + numLast

diff : List I32 -> List I32
diff = \nums ->
    List.map2 (List.dropFirst nums 1) (List.dropLast nums 1) Num.sub

testInput =
    """
    0 3 6 9 12 15
    1 3 6 10 15 21
    10 13 16 21 30 45

    """
