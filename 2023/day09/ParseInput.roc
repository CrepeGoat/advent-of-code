interface ParseInput
    exposes [parse, Histories]
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
    testInput
    |> parse
    == Ok [[0, 3, 6, 9, 12, 15], [1, 3, 6, 10, 15, 21], [10, 13, 16, 21, 30, 45]]

Input : Histories
Histories : List History
History : List I32

parse : Str -> Result Input _
parse = \text -> parseStr parseInput text

parseInput : Parser _ Input
parseInput =
    space = codeunit ' '
    anySpace = space |> many
    newline = codeunit '\n'
    isDigit = \unit -> ('0' <= unit && unit <= '9')

    positiveDigitsI32 = const Num.toI32 |> keep digits
    negativeDigitsI32 =
        const \x -> -(Num.toI32 x)
        |> skip (codeunit '-')
        |> keep digits
    digitsI32 = oneOf [positiveDigitsI32, negativeDigitsI32]

    history = digitsI32 |> sepBy1 space
    histories = history |> sepBy1 newline

    histories |> skip (newline |> many)

testInput =
    """
    0 3 6 9 12 15
    1 3 6 10 15 21
    10 13 16 21 30 45

    """
