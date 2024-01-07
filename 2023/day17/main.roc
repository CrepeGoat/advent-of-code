app "main"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [pf.Stdout, pf.Task, Heap.{ heapSort }]
    provides [main] to pf

main : Task.Task {} I32
main =
    n = 86897
    list = List.range { start: At n, end: Before 0 }
    listSorted = Heap.heapSort list Num.compare
    # listSorted |> List.map Num.toStr |> Str.joinWith "\n" |> Stdout.line
    Stdout.line "heap sorting done!"

loop : state, (state -> [Break b, Continue state]) -> b
loop = \stateInit, runIteration ->
    when runIteration stateInit is
        Continue state -> loop state runIteration
        Break result -> result
