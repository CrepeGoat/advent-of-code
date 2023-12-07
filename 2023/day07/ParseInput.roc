interface ParseInput
    exposes [parse, HandBid, Card]
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
    |> Result.map List.len
    == Ok 5

Input : List HandBid
HandBid : { hand : Hand, bid : U16 }
Hand : List Card
Card : [ACE, KING, QUEEN, JACK, TEN, NINE, EIGHT, SEVEN, SIX, FIVE, FOUR, THREE, TWO]

parse : Str -> Result Input _
parse = \text -> parseStr parseInput text

parseInput : Parser _ Input
parseInput =
    space = codeunit ' '
    newline = codeunit '\n'
    digitsU16 = const Num.toU16 |> keep digits

    handBid =
        const (\hand -> \bid -> { hand, bid })
        |> keep (parseCard |> many)
        |> skip space
        |> keep digitsU16

    handsBidsList = handBid |> sepBy newline

    handsBidsList |> skip (newline |> many)

parseCard =
    ace = const ACE |> skip (codeunit 'A')
    king = const KING |> skip (codeunit 'K')
    queen = const QUEEN |> skip (codeunit 'Q')
    jack = const JACK |> skip (codeunit 'J')
    ten = const TEN |> skip (codeunit 'T')
    nine = const NINE |> skip (codeunit '9')
    eight = const EIGHT |> skip (codeunit '8')
    seven = const SEVEN |> skip (codeunit '7')
    six = const SIX |> skip (codeunit '6')
    five = const FIVE |> skip (codeunit '5')
    four = const FOUR |> skip (codeunit '4')
    three = const THREE |> skip (codeunit '3')
    two = const TWO |> skip (codeunit '2')

    oneOf [ace, king, queen, jack, ten, nine, eight, seven, six, five, four, three, two]

testInput =
    """
    32T3K 765
    T55J5 684
    KK677 28
    KTJJT 220
    QQQJA 483

    """
