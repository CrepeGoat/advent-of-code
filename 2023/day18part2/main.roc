app "main"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [pf.Stdout, pf.Task]
    provides [main] to pf

main : Task.Task {} I32
main =
    Stdout.line "w/e"

# Make a type with no type parameters...
Position : { x : I32, y : I32 }
# ...then make a mew type, using the first incorrectly by providing a single
# type parameter.
Polyline : List (Position I64) # mistake: should be `List Position`

# Then make an expectation using the invalid type
id : Polyline -> Polyline
id = \line -> line

expect
    id [] |> List.isEmpty
