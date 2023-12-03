interface ParseInput
    exposes [parseInput, Game, ColorCubes]
    imports [
        parse.const,
        parse.codepoint,
        parse.many,
        parse.keep,
        parse.digits,
        parse.skip,
        parse.string,
        parse.oneOf,
        parse.sepBy
    ]

Game := { id : U32, rounds : List ColorCubes }
ColorCubes := [Red U32, Green U32, Blue U32]

parseInput: Str -> List Game


parseGame: Str -> Result Game [ParsingError]
parseGame = \line ->
    anySpace = codepoint ' ' |> many
    
    parseRed = const Red |> keep digits |> skip anySpace |> skip string "red"
    parseGreen = const Green |> keep digits |> skip anySpace |> skip string "green"
    parseBlue = const Blue |> keep digits |> skip anySpace |> skip string "blue"

    parseHandfulOfCubes = (oneOf [parseRed, parseGreen, parseBlue]) |> sepBy (string ", ")
    parseRounds = parseHandfulOfCubes |> sepBy (string "; ")

    game = const (\id -> \rounds -> )
