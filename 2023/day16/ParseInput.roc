interface ParseInput
    exposes [parse, Contraption, Tile, CardinalLines, DiagonalLines]
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
        .|...\\....
        |.-.\\.....
        .....|-...
        ........|.
        ..........
        .........\\
        ..../.\\\\..
        .-.-/..|..
        .|....-|.\\
        ..//.|....

        """
    result =
        testInput
        |> parse
        |> Result.map (\grid -> List.map grid List.len)
    result == Ok (List.repeat 10 10)

Input : Contraption
Contraption : List (List Tile)
Tile : [Empty, Splitter CardinalLines, Mirror DiagonalLines]
CardinalLines : [NorthSouth, EastWest]
DiagonalLines : [NorthwestSoutheast, NortheastSouthwest]

parse : Str -> Result Input _
parse = \text -> parseStr parseInput text

parseInput : Parser _ Input
parseInput =
    newline = codeunit '\n'

    northwest = const (Mirror NorthwestSoutheast) |> skip (codeunit '\\')
    northeast = const (Mirror NortheastSouthwest) |> skip (codeunit '/')
    northsouth = const (Splitter NorthSouth) |> skip (codeunit '|')
    eastwest = const (Splitter EastWest) |> skip (codeunit '-')
    empty = const Empty |> skip (codeunit '.')

    tile = oneOf [northwest, northeast, northsouth, eastwest, empty]

    tileRow = tile |> oneOrMore
    tileGrid = tileRow |> sepBy1 newline

    tileGrid |> skip (newline |> many)
