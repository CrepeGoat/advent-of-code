interface ParseInput
    exposes [parse, DigPlan, Instruction, Direction]
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

Input : DigPlan
DigPlan : List Instruction
Instruction : { dir : Direction, distance : U8, hexColor : Str }
Direction : [Up, Down, Left, Right]

expect
    testInput =
        """
        R 6 (#70c710)
        D 5 (#0dc571)
        L 2 (#5713f0)
        D 2 (#d2c081)
        R 2 (#59c680)
        D 2 (#411b91)
        L 5 (#8ceee2)
        U 2 (#caa173)
        L 1 (#1b58a2)
        U 2 (#caa171)
        R 2 (#7807d2)
        U 3 (#a77fa3)
        L 2 (#015232)
        U 2 (#7a21e3)

        """
    result =
        testInput
        |> parse
        |> Result.map List.len
    result == Ok 14

parse : Str -> Result Input _
parse = \text -> parseStr parseInput text

parseInput : Parser _ Input
parseInput =
    space = codeunit ' '
    newline = codeunit '\n'
    isHexNum = \unit ->
        ('0' <= unit && unit <= '9')
        || ('a' <= unit && unit <= 'f')

    up = const Up |> skip (codeunit 'U')
    down = const Down |> skip (codeunit 'D')
    left = const Left |> skip (codeunit 'L')
    right = const Right |> skip (codeunit 'R')

    direction = oneOf [up, down, left, right]
    digitsU8 = const Num.toU8 |> keep digits
    color =
        codeunitSatisfies isHexNum
        |> oneOrMore
        |> between (string "(#") (codeunit ')')
        |> map strFromUtf8

    instruction =
        const (\dir -> \distance -> \hexColor -> { dir, distance, hexColor })
        |> keep direction
        |> skip space
        |> keep digitsU8
        |> skip space
        |> keep color

    digPlan = instruction |> sepBy1 newline
    digPlan |> skip (newline |> many)
