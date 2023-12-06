interface ParseInput
    exposes [parse, Document]
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
            map,
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
    testInput
    |> parse
    == Ok { times: [7, 15, 30], distances: [9, 40, 200] }

Input : Document
Document : { times : List U64, distances : List U64 }

parse : Str -> Result Input _
parse = \text -> parseStr parseInput text

parseInput : Parser _ Input
parseInput =
    space = codeunit ' '
    anySpace = space |> many
    newline = codeunit '\n'
    u64Digits = const Num.toU64 |> keep digits

    document =
        const (\times -> \distances -> { times, distances })
        |> skip (string "Time:")
        |> skip anySpace
        |> keep (u64Digits |> sepBy anySpace)
        |> skip newline
        |> skip (string "Distance:")
        |> skip anySpace
        |> keep (u64Digits |> sepBy anySpace)

    document |> skip (newline |> many)

testInput =
    """
    Time:      7  15   30
    Distance:  9  40  200

    """
