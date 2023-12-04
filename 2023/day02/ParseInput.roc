interface ParseInput
    exposes [parseInput, Game, ColorCubes]
    imports [
        parse.Core.{
            Parser,
            const,
            many,
            keep,
            skip,
            oneOf,
            sepBy,
            map,
        },
        parse.String.{
            codeunit,
            digits,
            string,
            parseStr,
        },
    ]

Game : { id : U32, rounds : List (List ColorCubes) }
ColorCubes : [Red U32, Green U32, Blue U32]

parseInput : Str -> Result (List Game) _
parseInput = \text ->
    anySpace = codeunit ' ' |> many
    u32Digits = (digits |> map Num.toU32)

    red = const Red |> keep u32Digits |> skip anySpace |> skip (string "red")
    green = const Green |> keep u32Digits |> skip anySpace |> skip (string "green")
    blue = const Blue |> keep u32Digits |> skip anySpace |> skip (string "blue")

    Round : List ColorCubes
    round : Parser _ Round
    round = (oneOf [red, green, blue]) |> sepBy (string ", ")

    # try to restrict duplicate colors in same round?
    # HandfulOfCubes: {red ? 0u32, green ? 0u32, blue ? 0u32}
    # handfulOfCubes =
    #    colors <- map colorCubes

    rounds : Parser _ (List Round)
    rounds = round |> sepBy (string "; ")

    game =
        const (\id -> \r -> { id, rounds: r })
        |> skip (string "Game")
        |> skip anySpace
        |> keep u32Digits
        |> skip (string ":")
        |> skip anySpace
        |> keep rounds

    input = game |> skip anySpace |> skip (codeunit '\n') |> many
    parseStr input text
