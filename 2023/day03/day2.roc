# references
# - https://github.com/roc-lang/examples/tree/main/examples/CommandLineArgs
app "day3"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parse: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Stderr,
        pf.File,
        pf.Path,
        pf.Task,
        pf.Arg,
        ParseInput.{
            parse,
            Schematic,
        },
        ReadInput.{ readFileAndThen },
    ]
    provides [main] to pf

main : Task.Task {} I32
main =
    fileContentStr <- readFileAndThen
    parsedInput =
        when parse fileContentStr is
            Ok value -> value
            Err _ -> crash "failed to parse input"

    result = solve parsedInput
    Stdout.line "result: \(Num.toStr result)"

# ###############################################################################
# Day 3 Logic
# ###############################################################################

testInput =
    """
    467..114..
    ...*......
    ..35..633.
    ......#...
    617*......
    .....+.58.
    ..592.....
    ......755.
    ...$.*....
    .664.598..
    """

expect
    testInput
    |> parse
    |> Result.map solve
    == Ok 467835

solve : Schematic -> U32
solve = \schematic ->
    numbersAtSpaces =
        schematic
        |> getSchematicNumbers
        |> getSpacesOccupiedByNumbers
    schematicGearSymbols =
        schematic
        |> getSchematicSymbols
        |> List.keepIf \sym -> sym.symbol == '*'

    adjNumbers =
        sym <- schematicGearSymbols |> List.map
        adjPos <- sym |> .pos |> getAdjacentPositions |> List.keepOks
        numbersAtSpaces |> Dict.get adjPos

    gearRatios =
        adjNums <- adjNumbers |> List.keepOks
        when adjNums |> Set.fromList |> Set.toList is
            [num1, num2] -> Ok (num1.value * num2.value)
            _ -> Err {}

    gearRatios |> List.sum

getAdjacentNumbers : Dict Position U32, Position -> List U32
getAdjacentNumbers = \numbersAtSpaces, pos ->
    adjPos <- pos |> getAdjacentPositions |> List.keepOks

    Dict.get numbersAtSpaces adjPos

expect
    (1, 1)
    |> getAdjacentPositions
    |> List.sortWith comparePos
    == [(0, 0), (0, 1), (0, 2), (1, 0), (1, 2), (2, 0), (2, 1), (2, 2)]

expect
    (0, 1)
    |> getAdjacentPositions
    |> List.sortWith comparePos
    == [(0, 0), (0, 2), (1, 0), (1, 1), (1, 2)]

getAdjacentPositions : Position -> List Position
getAdjacentPositions = \(rowPos, colPos) ->
    colRange = List.range { start: At colPos, end: Before (colPos + 1) }

    List.join [
        [
            (Num.subChecked rowPos 1, Ok colPos),
            (Ok (rowPos + 1), Ok colPos),
        ],
        [
            (Num.subChecked rowPos 1, Num.subChecked colPos 1),
            (Ok rowPos, Num.subChecked colPos 1),
            (Ok (rowPos + 1), Num.subChecked colPos 1),
        ],
        [
            (Num.subChecked rowPos 1, Ok (colPos + 1)),
            (Ok rowPos, Ok (colPos + 1)),
            (Ok (rowPos + 1), Ok (colPos + 1)),
        ],
    ]
    |> List.keepIf (\(rowNumOrErr, colNumOrErr) -> List.all [rowNumOrErr, colNumOrErr] Result.isOk)
    |> List.map \(rowNumOk, colNumOk) -> (okOrCrash rowNumOk "confirmed ok", okOrCrash colNumOk "confirmed ok")

getSpacesOccupiedByNumbers : List SchematicNumber -> Dict Position SchematicNumber
getSpacesOccupiedByNumbers = \schematicNumbers ->
    prevSpacesMid, { pos: (rowPos, colPos), len, value } <-
        schematicNumbers |> List.walk (Dict.empty {})

    prevSpaces, numPos <-
        List.range { start: At colPos, end: Length len }
        |> List.map \colNum -> (rowPos, colNum)
        |> List.walk prevSpacesMid

    Dict.insert prevSpaces numPos { pos: (rowPos, colPos), len, value }

expect
    testInput
    |> parse
    |> okOrCrash ""
    |> getSchematicNumbers
    |> List.map .pos
    |> List.sortWith comparePos
    == [(0, 0), (0, 5), (2, 2), (2, 6), (4, 0), (5, 7), (6, 2), (7, 6), (9, 1), (9, 5)]

expect
    testInput
    |> parse
    |> okOrCrash ""
    |> getSchematicNumbers
    |> List.map .value
    |> List.sortAsc
    == List.sortAsc [467, 114, 35, 633, 617, 58, 592, 755, 664, 598]

getSchematicNumbers : Schematic -> List SchematicNumber
getSchematicNumbers = \schematic ->
    WalkState : {
        state : [None, PartialNumber Position U32],
        index : List SchematicNumber,
    }

    endState : WalkState
    endState =
        prevStateMid, row, rowNum <-
            schematic
            |> List.walkWithIndex { state: None, index: [] }

        endOfRowState : WalkState
        endOfRowState =
            prevState, element, colNum <-
                row
                |> List.walkWithIndex { prevStateMid & state: None }

            when element is
                Digit digit ->
                    when prevState.state is
                        None ->
                            { prevState &
                                state: PartialNumber (rowNum, colNum) (digit |> Num.toU32),
                            }

                        PartialNumber pos partial ->
                            { prevState &
                                state: PartialNumber pos (partial * 10 + (digit |> Num.toU32)),
                            }

                _ ->
                    when prevState.state is
                        None -> prevState
                        PartialNumber (rowPos, colPos) partial ->
                            {
                                state: None,
                                index: prevState.index
                                |> List.append {
                                    pos: (rowPos, colPos),
                                    len: colNum - colPos,
                                    value: partial,
                                },
                            }

        when endOfRowState.state is
            None -> endOfRowState
            PartialNumber (rowPos, colPos) value ->
                { endOfRowState &
                    index: endOfRowState.index
                    |> List.append {
                        pos: (rowPos, colPos),
                        len: (List.len row) - colPos,
                        value,
                    },
                }

    endState.index

expect
    testInput
    |> parse
    |> okOrCrash ""
    |> getSchematicSymbols
    |> List.map .symbol
    |> List.sortAsc
    == List.sortAsc ['*', '#', '*', '+', '$', '*']

getSchematicSymbols : Schematic -> List SchematicSymbol
getSchematicSymbols = \schematic ->
    prevPositionsMid, row, rowNum <- schematic |> List.walkWithIndex []
    prevPositions, element, colNum <- row |> List.walkWithIndex prevPositionsMid

    when element is
        Symbol symbol -> prevPositions |> List.append { pos: (rowNum, colNum), symbol }
        _ -> prevPositions

SchematicNumber : { pos : Position, len : Nat, value : U32 }
SchematicSymbol : { pos : Position, symbol : U8 }
SymbolsByPosition : Dict Position U8

comparePos = \(r1, c1), (r2, c2) ->
    when (Num.compare r1 r2, Num.compare c1 c2) is
        (EQ, cmp) -> cmp
        (cmp, _) -> cmp

Position : (Nat, Nat)

okOrCrash : Result a *, Str -> a
okOrCrash = \result, crashMsg ->
    when result is
        Ok value -> value
        _ -> crash crashMsg
