interface Solution
    exposes [solve]
    imports [
        ParseInput.{
            parse,
            Schematic,
        },
    ]

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
    == Ok 4361

solve : Schematic -> U32
solve = \schematic ->
    schematicNumbers = getSchematicNumbers schematic
    symbolsByPosition = getSymbolsByPosition schematic

    schematicNumbers
    |> List.dropIf (\numPos -> symbolsByPosition |> getAdjacentSymbols numPos |> List.isEmpty)
    |> List.map .value
    |> List.sum

getAdjacentSymbols : SymbolsByPosition, SchematicNumber -> List ((Nat, Nat), U8)
getAdjacentSymbols = \symbolsByPosition, schematicNumber ->
    adjPos <-
        schematicNumber
        |> getAdjacentPositions
        |> List.keepOks

    symbol <-
        symbolsByPosition
        |> Dict.get adjPos
        |> Result.map

    (adjPos, symbol)

expect
    { pos: (1, 1), len: 1, value: 0 }
    |> getAdjacentPositions
    |> List.sortWith comparePos
    == [(0, 0), (0, 1), (0, 2), (1, 0), (1, 2), (2, 0), (2, 1), (2, 2)]

expect
    { pos: (1, 1), len: 2, value: 0 }
    |> getAdjacentPositions
    |> List.sortWith comparePos
    == [(0, 0), (0, 1), (0, 2), (0, 3), (1, 0), (1, 3), (2, 0), (2, 1), (2, 2), (2, 3)]

expect
    { pos: (0, 1), len: 1, value: 0 }
    |> getAdjacentPositions
    |> List.sortWith comparePos
    == [(0, 0), (0, 2), (1, 0), (1, 1), (1, 2)]

getAdjacentPositions : SchematicNumber -> List (Nat, Nat)
getAdjacentPositions = \{ pos: (rowPos, colPos), len, value } ->
    colRange = List.range { start: At colPos, end: Before (colPos + len) }

    List.join [
        List.map colRange \colNum -> (Num.subChecked rowPos 1, Ok colNum),
        List.map colRange \colNum -> (Ok (rowPos + 1), Ok colNum),
        [
            (Num.subChecked rowPos 1, Num.subChecked colPos 1),
            (Ok rowPos, Num.subChecked colPos 1),
            (Ok (rowPos + 1), Num.subChecked colPos 1),
        ],
        [
            (Num.subChecked rowPos 1, Ok (colPos + len)),
            (Ok rowPos, Ok (colPos + len)),
            (Ok (rowPos + 1), Ok (colPos + len)),
        ],
    ]
    |> List.keepIf (\(rowNumOrErr, colNumOrErr) -> List.all [rowNumOrErr, colNumOrErr] Result.isOk)
    |> List.map \(rowNumOk, colNumOk) -> (okOrCrash rowNumOk "confirmed ok", okOrCrash colNumOk "confirmed ok")

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
        state : [None, PartialNumber (Nat, Nat) U32],
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

getSymbolsByPosition : Schematic -> SymbolsByPosition
getSymbolsByPosition = \schematic ->
    schematic
    |> getSchematicSymbols
    |> List.map \ss -> (ss.pos, ss.symbol)
    |> Dict.fromList

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

SchematicNumber : { pos : (Nat, Nat), len : Nat, value : U32 }
SchematicSymbol : { pos : (Nat, Nat), symbol : U8 }
SymbolsByPosition : Dict (Nat, Nat) U8

comparePos = \(r1, c1), (r2, c2) ->
    when (Num.compare r1 r2, Num.compare c1 c2) is
        (EQ, cmp) -> cmp
        (cmp, _) -> cmp

okOrCrash : Result a *, Str -> a
okOrCrash = \result, crashMsg ->
    when result is
        Ok value -> value
        _ -> crash crashMsg
