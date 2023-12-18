# references
# - https://github.com/roc-lang/examples/tree/main/examples/CommandLineArgs
app "main"
    packages {
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
    Stdout.line "hello"

expect
    result = solve 0
    exptResult = 1
    result == Ok exptResult

solve : U8 -> Result U32 []
solve = \x -> Ok 0
