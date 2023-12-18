# references
# - https://github.com/roc-lang/examples/tree/main/examples/CommandLineArgs
app "main"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [pf.Stdout, pf.Task]
    provides [main] to pf

main : Task.Task {} I32
main =
    Stdout.line "w/e"

expect
    n = 86897
    list = List.range { start: At n, end: Before 0 }
    List.sortWith list Num.compare == List.range { start: At 1, end: At n }
