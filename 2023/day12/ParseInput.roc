interface ParseInput
    exposes [parse, Records, Record, Condition]
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
    ???.### 1,1,3
    .??..??...?##. 1,1,3
    ?#?#?#?#?#?#?#? 1,3,1,6
    ????.#...#... 4,1,1
    ????.######..#####. 1,6,5
    ?###???????? 3,2,1

    """
expect
    testInput
    |> parse
    |> Result.map (\rows -> rows |> List.map (\record -> (List.len record.springConditions, List.len record.damageStreaks)))
    == Ok [(7, 3), (14, 3), (15, 4), (13, 3), (19, 3), (12, 3)]

Input : Records
Records : List Record
Record : { springConditions : List Condition, damageStreaks : List Nat }
Condition : [Operational, Damaged, Unknown]

parse : Str -> Result Input _
parse = \text -> parseStr parseInput text

parseInput : Parser _ Input
parseInput =
    newline = codeunit '\n'
    space = codeunit ' '

    operational = const Operational |> skip (codeunit '.')
    damaged = const Damaged |> skip (codeunit '#')
    unknown = const Unknown |> skip (codeunit '?')
    conditions = oneOf [operational, damaged, unknown] |> oneOrMore

    streaks = digits |> sepBy1 (codeunit ',')

    record =
        const \springConditions -> \damageStreaks -> { springConditions, damageStreaks }
        |> keep conditions
        |> skip space
        |> keep streaks
    records = record |> sepBy1 newline

    records |> skip (newline |> many)
