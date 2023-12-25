# references
# - https://github.com/roc-lang/examples/tree/main/examples/CommandLineArgs
app "day"
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
        ParseInput.{ parse },
        ReadInput.{ readFileAndThen },
        Solution.{ solve },
    ]
    provides [main] to pf

main : Task.Task {} I32
main =
    fileContentStr <- readFileAndThen
    parsedInput =
        when parse fileContentStr is
            Ok value -> value
            Err (ParsingFailure msg) -> crash "failed to parse input: \(msg)"
            Err (ParsingIncomplete msg) -> crash "only parsed partial input: \(msg)"

    result = solve parsedInput
    Stdout.line "result: \(Num.toStr result)"

