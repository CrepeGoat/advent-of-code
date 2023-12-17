interface ParseInput
    exposes [parse, HeatLossMap]
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
        """
        2413432311323
        3215453535623
        3255245654254
        3446585845452
        4546657867536
        1438598798454
        4457876987766
        3637877979653
        4654967986887
        4564679986453
        1224686865563
        2546548887735
        4322674655533

        """
    result =
        testInput
        |> parse
        |> Result.map (\grid -> List.map grid List.len)
    result == Ok (List.repeat 13 13)

Input : HeatLossMap
HeatLossMap : List (List U8)

parse : Str -> Result Input _
parse = \text -> parseStr parseInput text

parseInput : Parser _ Input
parseInput =
    newline = codeunit '\n'

    digitU8 = const Num.toU8 |> keep digit
    row = digitU8 |> oneOrMore
    heatlossmap = row |> sepBy1 newline
    heatlossmap |> skip (newline |> many)
