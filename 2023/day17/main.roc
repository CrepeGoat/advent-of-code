app "main"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [pf.Stdout, pf.Task]
    provides [main] to pf

main : Task.Task {} I32
main =
    n = 86897
    list = List.range { start: At n, end: Before 0 }
    listSorted = List.sortAsc list
    Stdout.line "w/e"
