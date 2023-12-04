# references
# - https://github.com/roc-lang/examples/tree/main/examples/CommandLineArgs
app "day2"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parse: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Stderr,
        pf.File,
        pf.Path,
        pf.Task,
        pf.Arg,
        ParseInput.{
            Game,
            parseInput,
        },
        ReadInput.{ readFileAndThen },
    ]
    provides [main] to pf

main : Task.Task {} I32
main =
    fileContentStr <- readFileAndThen
    parsedInput =
        when parseInput fileContentStr is
            Ok value -> value
            Err _ -> crash "failed to parse input"
    result =
        when solve parsedInput is
            Ok value -> value
            _ -> crash "failed to solve for input"
    Stdout.line "result: \(Num.toStr result)"

# ###############################################################################
# Day 2 Logic
# ###############################################################################
expect
    """
    Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
    Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
    Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
    Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
    
    """
    |> parseInput
    |> Result.try solve
    == Ok 2286

solve : List Game -> Result U32 [DuplicateColorInRound Str]
solve = \games ->
    prevSum, game <- games |> List.walkTry 0u32

    cubeMaxesOrErr =
        prevCubeMaxes, round <- game.rounds |> List.walkTry { red: 0u32, green: 0u32, blue: 0u32 }

        cubesInRoundOrErr =
            prevCubesInRound, colorCubes <- round |> List.walkTry (Dict.empty {})

            (colorName, count) =
                when colorCubes is
                    Red value -> ("red", value)
                    Green value -> ("green", value)
                    Blue value -> ("blue", value)

            if Dict.contains prevCubesInRound colorName then
                Err (DuplicateColorInRound colorName)
            else
                Ok (Dict.insert prevCubesInRound colorName count)

        cubesInRound <- cubesInRoundOrErr |> Result.map

        {
            red: cubesInRound |> dictGetWithDefault "red" 0 |> Num.max prevCubeMaxes.red,
            green: cubesInRound |> dictGetWithDefault "green" 0 |> Num.max prevCubeMaxes.green,
            blue: cubesInRound |> dictGetWithDefault "blue" 0 |> Num.max prevCubeMaxes.blue,
        }

    cubeMaxes <- cubeMaxesOrErr |> Result.map
    cubeProduct = cubeMaxes.red * cubeMaxes.green * cubeMaxes.blue
    dbg cubeProduct

    prevSum + cubeProduct

dictGetWithDefault = \dict, key, default ->
    if Dict.contains dict key then
        Dict.get dict key |> okOrCrash "we already confirmed that this key is present"
    else
        default

okOrCrash : Result a *, Str -> a
okOrCrash = \result, crashMsg ->
    when result is
        Ok value -> value
        _ -> crash crashMsg
