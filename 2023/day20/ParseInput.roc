interface ParseInput
    exposes [parse, Module]
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

Input : List Module
Module : { type : [FlipFlop, Conjunction, Broadcaster], name : Str, connectsTo : List Str }

expect
    testInput =
        """
        broadcaster -> a, b, c
        %a -> b
        %b -> c
        %c -> inv
        &inv -> a

        """
    result =
        testInput
        |> parse
        |> Result.map List.len
    result == Ok 5

parse : Str -> Result Input _
parse = \text -> parseStr parseInput text

parseInput : Parser _ Input
parseInput =
    newline = codeunit '\n'
    space = codeunit ' '
    isAlphaLower = \unit -> 'a' <= unit && unit <= 'z'
    strAlphaLower =
        codeunitSatisfies isAlphaLower |> oneOrMore |> map strFromUtf8

    flipflop = const (\name -> (FlipFlop, name)) |> skip (codeunit '%') |> keep strAlphaLower
    conjunction = const (\name -> (Conjunction, name)) |> skip (codeunit '&') |> keep strAlphaLower
    broadcaster = const (\name -> (Broadcaster, name)) |> keep (string "broadcaster")
    typesAndName = oneOf [flipflop, conjunction, broadcaster]

    connections = strAlphaLower |> sepBy1 (string ", ")

    module =
        const (\(type, name) -> \connectsTo -> { type, name, connectsTo })
        |> keep typesAndName
        |> skip (string " -> ")
        |> keep connections

    modules = module |> sepBy1 newline

    modules |> skip (newline |> many)
