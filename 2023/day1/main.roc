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
# Day 1 Logic
# ###############################################################################
expect
    "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"
    |> parseInput
    |> calibrateTrebuchet
    == Ok 142
expect
    "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"
    |> parseInput
    |> calibrateTrebuchet
    == Ok 281

parseInput : Str -> List (List Str)
parseInput = \input ->
    input
    |> Str.split "\n"
    |> List.dropIf Str.isEmpty
    |> List.map Str.graphemes

calibrateTrebuchet : List (List Str) -> Result U32 [NoDigitsInLine]
calibrateTrebuchet = \input ->
    input
    |> List.mapTry calculateLine
    |> Result.map List.sum

calculateLine : List Str -> Result U32 [NoDigitsInLine]
calculateLine = \line ->
    lineStripped = line |> dropLeadingNonDigits |> dropTrailingNonDigits
    firstDigit <-
        digits
        |> List.findFirst (\digit -> List.startsWith lineStripped digit)
        |> Result.mapErr (\_ -> NoDigitsInLine)
        |> Result.try  # `try` here because we will generate `Err`'s later
    lastDigit <-
        digits
        |> List.findFirst (\digit -> List.endsWith lineStripped digit)
        |> Result.mapErr (\_ -> NoDigitsInLine)
        |> Result.map  # `map` here because we will only generate `U32`'s from here on out

    firstValue = Dict.get digitsToValues firstDigit |> okOrCrash "digit validity already confirmed"
    lastValue = Dict.get digitsToValues lastDigit |> okOrCrash "digit validity already confirmed"

    (firstValue * 10) + lastValue

dropLeadingNonDigits : List Str -> List Str
dropLeadingNonDigits = \graphemes ->
    if List.isEmpty graphemes || List.any digits \digit -> List.startsWith graphemes digit then
        graphemes
    else
        graphemes |> List.dropFirst 1 |> dropLeadingNonDigits

dropTrailingNonDigits : List Str -> List Str
dropTrailingNonDigits = \graphemes ->
    if List.isEmpty graphemes || List.any digits \digit -> List.endsWith graphemes digit then
        graphemes
    else
        graphemes |> List.dropLast 1 |> dropTrailingNonDigits

expect List.all digits \digit -> Dict.contains digitsToValues digit
digits =
    [
        "0",
        "1",
        "2",
        "3",
        "4",
        "5",
        "6",
        "7",
        "8",
        "9",
        "zero",
        "one",
        "two",
        "three",
        "four",
        "five",
        "six",
        "seven",
        "eight",
        "nine",
    ]
    |> List.map Str.graphemes

digitsToValues =
    Dict.empty {}
    |> Dict.insert (Str.graphemes "0") 0
    |> Dict.insert (Str.graphemes "1") 1
    |> Dict.insert (Str.graphemes "2") 2
    |> Dict.insert (Str.graphemes "3") 3
    |> Dict.insert (Str.graphemes "4") 4
    |> Dict.insert (Str.graphemes "5") 5
    |> Dict.insert (Str.graphemes "6") 6
    |> Dict.insert (Str.graphemes "7") 7
    |> Dict.insert (Str.graphemes "8") 8
    |> Dict.insert (Str.graphemes "9") 9
    |> Dict.insert (Str.graphemes "zero") 0
    |> Dict.insert (Str.graphemes "one") 1
    |> Dict.insert (Str.graphemes "two") 2
    |> Dict.insert (Str.graphemes "three") 3
    |> Dict.insert (Str.graphemes "four") 4
    |> Dict.insert (Str.graphemes "five") 5
    |> Dict.insert (Str.graphemes "six") 6
    |> Dict.insert (Str.graphemes "seven") 7
    |> Dict.insert (Str.graphemes "eight") 8
    |> Dict.insert (Str.graphemes "nine") 9

okOrCrash : Result a *, Str -> a
okOrCrash = \result, crashMsg ->
    when result is
        Ok value -> value
        _ -> crash crashMsg
