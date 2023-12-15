# references
# - https://github.com/roc-lang/examples/tree/main/examples/CommandLineArgs
app "day"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task,
    ]
    provides [main] to pf

main : Task.Task {} I32
main =
    Stdout.line "result: big shrug my guy"

expect
    # v Problem here!
    _range = List.range { start: At 1, end: Length 536870911 }
    # ^
    0 == 0
