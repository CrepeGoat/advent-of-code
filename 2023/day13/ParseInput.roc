interface ParseInput
    exposes [parse, Patterns, Pattern, Ground]
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

testInput =
    """
    #.##..##.
    ..#.##.#.
    ##......#
    ##......#
    ..#.##.#.
    ..##..##.
    #.#.##.#.

    #...##..#
    #....#..#
    ..##..###
    #####.##.
    #####.##.
    ..##..###
    #....#..#

    """
expect
    result =
        patterns <-
            testInput
            |> parse
            |> Result.map
        pattern <- patterns |> List.map
        groundRow <- pattern |> List.map
        List.len groundRow
    result == Ok [List.repeat 9 7, List.repeat 9 7]

Input : Patterns
Patterns : List Pattern
Pattern : List (List Ground)
Ground : [Ash, Rock]

parse : Str -> Result Input _
parse = \text -> parseStr parseInput text

parseInput : Parser _ Input
parseInput =
    newline = codeunit '\n'

    ash = const Ash |> skip (codeunit '.')
    rock = const Rock |> skip (codeunit '#')

    ground = oneOf [ash, rock]
    groundRow = ground |> oneOrMore
    pattern = groundRow |> sepBy1 newline
    patterns = pattern |> sepBy1 (string "\n\n")

    patterns |> skip (newline |> many)
