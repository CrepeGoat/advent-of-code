interface ParseInput
    exposes [parse, Schematic]
    imports [
        parse.Core.{
            Parser,
            const,
            many,
            keep,
            skip,
            oneOf,
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
            parseStr,
        },
    ]

testInput =
    """
    467..114..
    ...*......
    ..35..633.
    ......#...
    617*......
    .....+.58.
    ..592.....
    ......755.
    ...$.*....
    .664.598..
    """

expect
    testInput
    |> parse
    |> Result.isOk

expect
    testInput
    |> parse
    |> okOrCrash "TEST FAILED"
    |> List.len
    == 10

expect
    testInput
    |> parse
    |> okOrCrash "TEST FAILED"
    |> List.all (\row -> List.len row == 10)

Schematic : List (List Element)
Element : [Digit U8, Symbol U8, Blank]

parse : Str -> Result Schematic _
parse = \text -> parseStr parseInput text

parseInput : Parser _ Schematic
parseInput =
    u8Digit = const Digit |> keep (digit |> map Num.toU8)
    blank = const Blank |> skip (codeunit '.')
    symbol = const Symbol |> keep (codeunitSatisfies \unit -> unit != '\n')

    element = oneOf [u8Digit, blank, symbol]
    row = element |> many
    schematic = row |> sepBy (codeunit '\n')

    schematic

okOrCrash : Result a *, Str -> a
okOrCrash = \result, crashMsg ->
    when result is
        Ok value -> value
        _ -> crash crashMsg
