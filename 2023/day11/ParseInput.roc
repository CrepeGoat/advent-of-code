interface ParseInput
    exposes [parse, Image, Body]
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
    |> Result.map (\rows -> rows |> List.map List.len)
    == Ok (List.repeat 10 10)

Input : Image
Image : List (List Body)
Body : [Space, Galaxy]

parse : Str -> Result Input _
parse = \text -> parseStr parseInput text

parseInput : Parser _ Input
parseInput =
    newline = codeunit '\n'

    space = const Space |> skip (codeunit '.')
    galaxy = const Galaxy |> skip (codeunit '#')

    body = oneOf [space, galaxy]
    imageRow = body |> oneOrMore
    image = imageRow |> sepBy1 newline

    image |> skip (newline |> many)

testInput =
    """
    ...#......
    .......#..
    #.........
    ..........
    ......#...
    .#........
    .........#
    ..........
    .......#..
    #...#.....

    """
