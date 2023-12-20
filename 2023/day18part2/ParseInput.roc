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

Input : DigPlan
DigPlan : List { given : Instruction U8, encoded : Instruction U32 }
Instruction num : { dir : Direction, distance : num }
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
expect
    testInput =
        """
        R 6 (#70c710)
        D 5 (#0dc571)
        L 5 (#8ceee2)
        U 2 (#caa173)

        """
    result =
        testInput
        |> parse
        |> okOrCrash "test failed"
        |> List.map .encoded
    exptEncoded = [
        { dir: Right, distance: 461937 },
        { dir: Down, distance: 56407 },
        { dir: Left, distance: 577262 },
        { dir: Up, distance: 829975 },
    ]
    result == exptEncoded
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

    instruction =
        const (\dir -> \distance -> { dir, distance })
        |> keep direction
        |> skip space
        |> keep digitsU8

    instructionLine =
        const \given -> \encoded -> { given, encoded }
        |> keep instruction
        |> skip space
        |> keep encodedInstruction

    digPlan = instructionLine |> sepBy1 newline
    digPlan |> skip (newline |> many)

encodedInstruction : Parser _ (Instruction U32)
encodedInstruction =
    hexDigitsU32
    |> map decodeInstruction
    |> flatten
    |> between (string "(#") (codeunit ')')

decodeInstruction : U32 -> Result (Instruction U32) Str
decodeInstruction = \num ->
    distance = Num.shiftRightZfBy num 4
    dir <- Result.map
            (
                when Num.bitwiseXor num (Num.shiftLeftBy distance 4) is
                    0 -> Ok Right
                    1 -> Ok Down
                    2 -> Ok Left
                    3 -> Ok Up
                    _ -> Err "invalid direction label"
            )
    { dir, distance }

# ##############################################################################

hexDigitsU32 =
    const (\ds -> ds |> List.walk 0 (\result, d -> 16 * result + (Num.toU32 d)))
    |> keep (hexDigit |> oneOrMore)

hexDigit : Parser _ U8
hexDigit =
    hexDigitMid =
        unit <- const
        if '0' <= unit && unit <= '9' then
            unit - '0' |> Ok
        else if 'a' <= unit && unit <= 'f' then
            (unit - 'a' + 10) |> Ok
        else
            "invalid hexadecimal code unit" |> Err

    hexDigitMid |> keep anyCodeunit |> flatten

okOrCrash : Result a *, Str -> a
okOrCrash = \result, crashMsg ->
    when result is
        Ok value -> value
        _ -> crash crashMsg
