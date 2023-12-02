package ParseInput
    exposes [
        parseInput,
        Game,
        ColorCubes,
    ]
    packages {
        parse: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        parse.*,
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
