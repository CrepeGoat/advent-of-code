app "main"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [pf.Stdout, pf.Task, Heap.{ Heap, empty, push, pop }]
    provides [main] to pf

main : Task.Task {} I32
main =
    n = 86897
    list = List.range { start: At n, end: Before 0 }
    listSorted = heapSort list
    # listSorted |> List.map Num.toStr |> Str.joinWith "\n" |> Stdout.line
    Stdout.line "heap sorting done!"

heapSort : List (Num a) -> List (Num a)
heapSort = \nums ->
    heap = nums |> List.walk (Heap.empty Num.compare) Heap.push
    (heapMid, resultMid) <- loop (heap, [])
    when Heap.pop heapMid is
        Ok (heapNext, n) -> Continue (heapNext, List.append resultMid n)
        Err _ -> Break resultMid

loop : state, (state -> [Break b, Continue state]) -> b
loop = \stateInit, runIteration ->
    when runIteration stateInit is
        Continue state -> loop state runIteration
        Break result -> result
