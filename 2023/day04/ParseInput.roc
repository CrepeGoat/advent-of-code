interface ParseInput
    exposes [parse]
    imports [
        # if I keep some import here, `roc check ParseInput.roc` hangs
        # but if I *remove* this import, everything works
        # (the actual thing imported doesn't seem to matter)
        parse.Core.{ Parser },
    ]

parse = "hello"
