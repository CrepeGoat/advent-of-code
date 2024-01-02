interface ParseInput
    exposes [parse, Map, Tile]
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
            flatten,
        },
        parse.String.{
            anyCodeunit,
            anyString,
            codeunit,
            codeunitSatisfies,
            digit,
            digits,
            string,
            strFromUtf8,
            parseStr,
        },
    ]

Input : Map
Map : List (List Tile)
Tile : [GardenPlot, Rock, Start]

expect
    testInput =
        """
        ...........
        .....###.#.
        .###.##..#.
        ..#.#...#..
        ....#.#....
        .##..S####.
        .##..#...#.
        .......##..
        .##.#.####.
        .##..##.##.
        ...........

        """
    result =
        testInput
        |> parse
        |> Result.map (\grid -> List.map grid List.len)
    result == Ok (List.repeat 11 11)

parse : Str -> Result Input _
parse = \text -> parseStr parseInput text

parseInput : Parser _ Input
parseInput =
    newline = codeunit '\n'
    space = codeunit ' '
    isAlphaLower = \unit -> 'a' <= unit && unit <= 'z'
    strAlphaLower =
        codeunitSatisfies isAlphaLower |> oneOrMore |> map strFromUtf8

    start = const Start |> skip (codeunit 'S')
    rock = const Rock |> skip (codeunit '#')
    plot = const GardenPlot |> skip (codeunit '.')

    tile = oneOf [start, rock, plot]

    tileRow = tile |> oneOrMore
    tileGrid = tileRow |> sepBy1 newline

    tileGrid |> skip (newline |> many)
