# references
# - https://github.com/roc-lang/examples/tree/main/examples/CommandLineArgs
app "day2"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        deps: "deps/main.roc",
    }
    imports [
        pf.Stdout,
        deps.ParsedText.{ text },
    ]
    provides [main] to pf

main =
    Stdout.line text