interface ParseInput
    exposes [parse, Almanac, SuppliesMap, Range, MapRange]
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
            map,
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
    |> Result.isOk

expect
    testInput
    |> parse
    |> okOrCrash "TEST FAILED"
    |> .seedsRanges
    == [{ start: 79, len: 14 }, { start: 55, len: 13 }]

expect
    testInput
    |> parse
    |> okOrCrash "TEST FAILED"
    |> .maps
    |> List.len
    == 7

Almanac : { seedsRanges : List Range, maps : List SuppliesMap }
SuppliesMap : {
    givenSupply : Str,
    requiredSupply : Str,
    ranges : List MapRange,
}
MapRange : {
    mapStart : U64,
    start : U64,
    len : U64,
}
Range : { start : U64, len : U64 }

parse : Str -> Result Almanac _
parse = \text -> parseStr parseInput text

parseInput : Parser _ Almanac
parseInput =
    isAlpha = \unit -> ('a' <= unit && unit <= 'z') || ('A' <= unit && unit <= 'Z')

    space = codeunit ' '
    newline = codeunit '\n'
    newlinex2 = string "\n\n"
    u32Digits = const Num.toU64 |> keep digits
    strAlphas = codeunitSatisfies isAlpha |> oneOrMore |> map strFromUtf8

    seedRange =
        const \start -> \len -> { start, len }
        |> keep u32Digits
        |> skip space
        |> keep u32Digits

    mapRange =
        const \mapStart -> \start -> \len -> { start, mapStart, len }
        |> keep u32Digits
        |> skip space
        |> keep u32Digits
        |> skip space
        |> keep u32Digits

    supMap =
        const \givenSupply -> \requiredSupply -> \ranges -> {
                    givenSupply,
                    requiredSupply,
                    ranges,
                }
        |> keep strAlphas
        |> skip (string "-to-")
        |> keep strAlphas
        |> skip (string " map:\n")
        |> keep (mapRange |> sepBy newline)

    almanac =
        const \seedsRanges -> \maps -> { seedsRanges, maps }
        |> skip (string "seeds: ")
        |> keep (seedRange |> sepBy space)
        |> skip newlinex2
        |> keep (supMap |> sepBy newlinex2)

    almanac |> skip (newline |> many)

okOrCrash : Result a *, Str -> a
okOrCrash = \result, crashMsg ->
    when result is
        Ok value -> value
        _ -> crash crashMsg

testInput =
    """
    seeds: 79 14 55 13

    seed-to-soil map:
    50 98 2
    52 50 48

    soil-to-fertilizer map:
    0 15 37
    37 52 2
    39 0 15

    fertilizer-to-water map:
    49 53 8
    0 11 42
    42 0 7
    57 7 4

    water-to-light map:
    88 18 7
    18 25 70

    light-to-temperature map:
    45 77 23
    81 45 19
    68 64 13

    temperature-to-humidity map:
    0 69 1
    1 0 69

    humidity-to-location map:
    60 56 37
    56 93 4

    """
