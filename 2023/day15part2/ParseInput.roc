interface ParseInput
    exposes [parse, Steps, Operation, LabelledLens]
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
Steps : List Operation
Operation : [Remove (List U8), Append LabelledLens]
LabelledLens : { number : U8, label : List U8 }

parse : Str -> Result Input _
parse = \text -> parseStr parseInput (text |> Str.replaceEach "\n" "")

parseInput : Parser _ Input
parseInput =
    newline = codeunit '\n'

    isAlpha = \unit -> 'a' <= unit && unit <= 'z'
    label = codeunitSatisfies isAlpha |> oneOrMore
    lensNum = const Num.toU8 |> keep digit
    remove = const Remove |> keep label |> skip (codeunit '-')
    append =
        const (\l -> \number -> Append { label: l, number })
        |> keep label
        |> skip (codeunit '=')
        |> keep lensNum

    op = oneOf [remove, append]
    steps = op |> sepBy1 (codeunit ',')

    steps |> skip (newline |> many)
