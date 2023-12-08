# references
# - https://github.com/roc-lang/examples/tree/main/examples/CommandLineArgs
app "day4"
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

brokenFunc : Dict U32 Nat -> Dict U32 Nat
brokenFunc = \itemCounts ->
    (itemCountsNo11s, count11s) = # error
        count11s = # error
            when Dict.get itemCounts 11 is
                Ok count -> count
                Err _ -> 0

        (Dict.remove itemCounts 11, count11s)

    majorityItem =
        itemCountsNo11s
        |> Dict.toList
        |> List.sortWith \(i1, c1), (i2, c2) -> Num.compare c2 c1

    majorityItemEntry <- Dict.update itemCountsNo11s majorityItem
    when majorityItemEntry is
        Present count -> count + count11s
        Missing -> crash "already checked that this item is present"
