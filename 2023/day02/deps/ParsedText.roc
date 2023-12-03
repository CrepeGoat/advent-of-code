interface ParsedText
    exposes [text]
    imports [
        parse.parseStr,
        parse.string,
    ]

text = parseStr (string "text") "text" == Ok "text"
