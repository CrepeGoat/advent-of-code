interface ParseInput
    exposes [parse, Documents]
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
    |> Result.map \docs -> (List.len docs.turns, List.len docs.network)
    == Ok (2, 7)

expect
    testInput2
    |> parse
    |> Result.map \docs -> (List.len docs.turns, List.len docs.network)
    == Ok (3, 3)

Input : Documents
Documents : { turns : List [LEFT, RIGHT], network : List (Node, Fork) }
Fork : { left : Node, right : Node }
Node : Str

parse : Str -> Result Input _
parse = \text -> parseStr parseInput text

parseInput : Parser _ Input
parseInput =
    space = codeunit ' '
    anySpace = space |> many
    newline = codeunit '\n'
    isUpperAlpha = \unit -> ('A' <= unit && unit <= 'Z')

    leftTurn = const LEFT |> skip (codeunit 'L')
    rightTurn = const RIGHT |> skip (codeunit 'R')
    turn = oneOf [leftTurn, rightTurn]

    parseNode = codeunitSatisfies isUpperAlpha |> oneOrMore |> map strFromUtf8
    parseFork =
        const (\left -> \right -> { left, right })
        |> keep parseNode
        |> skip (string ", ")
        |> keep parseNode
        |> between (codeunit '(') (codeunit ')')
    nodeFork =
        const (\node -> \fork -> (node, fork))
        |> keep parseNode
        |> skip (string " = ")
        |> keep parseFork

    documents =
        const (\turns -> \network -> { turns, network })
        |> keep (turn |> oneOrMore)
        |> skip (string "\n\n")
        |> keep (nodeFork |> sepBy (codeunit '\n'))

    documents |> skip (newline |> many)

testInput1 =
    """
    RL

    AAA = (BBB, CCC)
    BBB = (DDD, EEE)
    CCC = (ZZZ, GGG)
    DDD = (DDD, DDD)
    EEE = (EEE, EEE)
    GGG = (GGG, GGG)
    ZZZ = (ZZZ, ZZZ)

    """

testInput2 =
    """
    LLR

    AAA = (BBB, BBB)
    BBB = (AAA, ZZZ)
    ZZZ = (ZZZ, ZZZ)

    """
