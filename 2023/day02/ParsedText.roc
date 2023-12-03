interface ParsedText
    exposes [text]
    imports [
        parse.String.{ parseStr, string },
    ]

text =
    when parseStr (string "text") "text" is
        Ok value -> value
        Err _ -> crash "not today!"
