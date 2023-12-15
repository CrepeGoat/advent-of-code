interface ParseInput
    exposes [parse, Steps, Step]
    imports [
        parse.Core.{
            Parser,
            const,
            many,
            keep,
            skip,
            oneOf,
            oneOrMore,
            sepBy,
            sepBy1,
            map,
            between,
        },
        parse.String.{
            anyCodeunit,
            codeunit,
            codeunitSatisfies,
            digit,
            digits,
            string,
            strFromUtf8,
            parseStr,
        },
    ]

expect
    testInput =
        "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7\n"
    result =
        testInput
        |> parse
        |> Result.map List.len
    result == Ok 11

Input : Steps
Steps : List Step
Step : List U8

parse : Str -> Result Input _
parse = \text -> parseStr parseInput (text |> Str.replaceEach "\n" "")

parseInput : Parser _ Input
parseInput =
    newline = codeunit '\n'
    isStepChar = \unit -> unit != ','
    step = codeunitSatisfies isStepChar |> oneOrMore
    steps = step |> sepBy1 (codeunit ',')

    steps |> skip (newline |> many)
