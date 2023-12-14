interface ParseInput
    exposes [parse, Platform, Space]
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
        O....#....
        O.OO#....#
        .....##...
        OO.#O....O
        .O.....O#.
        O.#..O.#.#
        ..O..#O..O
        .......O..
        #....###..
        #OO..#....

        """
    result =
        platform <-
            testInput
            |> parse
            |> Result.map
        platformRow <- platform |> List.map
        List.len platformRow
    result == Ok (List.repeat 10 10)

Input : Platform
Platform : List (List Space)
Space : [Round, Square, Empty]

parse : Str -> Result Input _
parse = \text -> parseStr parseInput text

parseInput : Parser _ Input
parseInput =
    newline = codeunit '\n'

    round = const Round |> skip (codeunit 'O')
    square = const Square |> skip (codeunit '#')
    empty = const Empty |> skip (codeunit '.')

    space = oneOf [round, square, empty]
    platformRow = space |> oneOrMore
    platform = platformRow |> sepBy1 newline

    platform |> skip (newline |> many)
