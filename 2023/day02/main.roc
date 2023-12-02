# references
# - https://github.com/roc-lang/examples/tree/main/examples/CommandLineArgs
app "day1"
    packages {
        # pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Stderr,
        pf.File,
        pf.Path,
        pf.Task,
        pf.Arg,
    ]
    provides [main] to pf

main : Task.Task {} I32
main =
    finalTask =
        # try to read the first command line argument
        pathArg <- Task.await readFirstArgT

        readFileToStr (Path.fromStr pathArg)

    finalResult <- Task.attempt finalTask

    when finalResult is
        Err ZeroArgsGiven ->
            {} <- Stderr.line "Error ZeroArgsGiven:\n\tI expected one argument, but I got none.\n\tRun the app like this: `roc command-line-args.roc -- path/to/input.txt`" |> Task.await

            Task.err 1 # 1 is an exit code to indicate failure

        Err (ReadFileErr errMsg) ->
            indentedErrMsg = indentLines errMsg
            {} <- Stderr.line "Error ReadFileErr:\n\(indentedErrMsg)" |> Task.await

            Task.err 1 # 1 is an exit code to indicate failure

        Ok fileContentStr ->
            parsedInput = parseInput fileContentStr
            result =
                when calibrateTrebuchet parsedInput is
                    Ok value -> value
                    _ -> crash "invalid input"
            Stdout.line "result: \(Num.toStr result)"

# Task to read the first CLI arg (= Str)
readFirstArgT : Task.Task Str [ZeroArgsGiven]_
readFirstArgT =
    # read all command line arguments
    args <- Arg.list |> Task.await

    # get the second argument, the first is the executable's path
    List.get args 1 |> Result.mapErr (\_ -> ZeroArgsGiven) |> Task.fromResult

# reads a file and puts all lines in one Str
readFileToStr : Path.Path -> Task.Task Str [ReadFileErr Str]_
readFileToStr = \path ->
    path
    |> File.readUtf8 # Make a nice error message
    |> Task.mapErr
        (\fileReadErr ->
            pathStr = Path.display path
            # TODO use FileReadErrToErrMsg when it is implemented: https://github.com/roc-lang/basic-cli/issues/44
            when fileReadErr is
                FileReadErr _ readErr ->
                    readErrStr = File.readErrToStr readErr
                    ReadFileErr "Failed to read file at:\n\t\(pathStr)\n\(readErrStr)"

                FileReadUtf8Err _ _ ->
                    ReadFileErr "I could not read the file:\n\t\(pathStr)\nIt contains charcaters that are not valid UTF-8:\n\t- Check if the file is encoded using a different format and convert it to UTF-8.\n\t- Check if the file is corrupted.\n\t- Find the characters that are not valid UTF-8 and fix or remove them."
        )

# indent all lines in a Str with a single tab
indentLines : Str -> Str
indentLines = \inputStr ->
    inputStr
    |> Str.split "\n"
    |> List.map (\line -> Str.concat "\t" line)
    |> Str.joinWith "\n"

# ###############################################################################
# Day 2 Logic
# ###############################################################################
expect
    [
        "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
        "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
        "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
        "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
        "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green",
    ]
    |> Str.joinWith "\n"
    |> parseInput
    |> 
    == Ok 8

Game: {id: U32, handfuls: List CubeHandful}
CubeHandful: {red: U32, green: U32, blue: U32}

parseInput : Str -> List Game
parseInput = \input ->
    inputLine <- input |> Str.split "\n" |> List.dropIf Str.isEmpty |> List.map
    inputLine


okOrCrash : Result a *, Str -> a
okOrCrash = \result, crashMsg ->
    when result is
        Ok value -> value
        _ -> crash crashMsg
