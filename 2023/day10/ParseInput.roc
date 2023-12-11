interface ParseInput
    exposes [parse, TileGrid, Tile, Cardinal]
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
    testInput1
    |> parse
    |> Result.map (\rows -> rows |> List.map List.len)
    == Ok [5, 5, 5, 5, 5]
expect
    testInput2
    |> parse
    |> Result.map (\rows -> rows |> List.map List.len)
    == Ok [5, 5, 5, 5, 5]
expect
    testInput3
    |> parse
    |> Result.map (\rows -> rows |> List.map List.len)
    == Ok [5, 5, 5, 5, 5]
expect
    testInput4
    |> parse
    |> Result.map (\rows -> rows |> List.map List.len)
    == Ok [5, 5, 5, 5, 5]

Input : TileGrid
TileGrid : List (List Tile)
Tile : [GROUND, PIPE (Cardinal, Cardinal), START]
Cardinal : [NORTH, EAST, SOUTH, WEST]

parse : Str -> Result Input _
parse = \text -> parseStr parseInput text

parseInput : Parser _ Input
parseInput =
    newline = codeunit '\n'

    fromNorthToEast = const (NORTH, EAST) |> skip (codeunit 'L')
    fromNorthToSouth = const (NORTH, SOUTH) |> skip (codeunit '|')
    fromNorthToWest = const (NORTH, WEST) |> skip (codeunit 'J')
    fromEastToSouth = const (EAST, SOUTH) |> skip (codeunit 'F')
    fromEastToWest = const (EAST, WEST) |> skip (codeunit '-')
    fromSouthToWest = const (SOUTH, WEST) |> skip (codeunit '7')

    start = const START |> skip (codeunit 'S')
    ground = const GROUND |> skip (codeunit '.')
    pipe =
        const PIPE
        |> keep
            (
                oneOf [
                    fromNorthToEast,
                    fromNorthToSouth,
                    fromNorthToWest,
                    fromEastToSouth,
                    fromEastToWest,
                    fromSouthToWest,
                ]
            )
    tile = oneOf [ground, pipe, start]
    tileRow = tile |> oneOrMore
    tileGrid = tileRow |> sepBy1 newline

    tileGrid |> skip (newline |> many)

testInput1 =
    """
    .....
    .S-7.
    .|.|.
    .L-J.
    .....

    """

testInput2 =
    """
    -L|F7
    7S-7|
    L|7||
    -L-J|
    L|-JF

    """

testInput3 =
    """
    ..F7.
    .FJ|.
    SJ.L7
    |F--J
    LJ...

    """

testInput4 =
    """
    7-F7-
    .FJ|7
    SJLL7
    |F--J
    LJ.LJ

    """
