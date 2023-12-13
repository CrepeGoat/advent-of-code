interface Solution
    exposes [solve]
    imports [
        ParseInput.{ parse, Documents, Fork, Node, Step },
    ]

expect
    testInput
    |> parse
    |> Result.try solve
    == Ok 6

SuperNetwork : { superNodeLen : Nat, graph : Dict Node { postCycleNode : Node, terminalIndices : List Nat } }
SuperNetworkCycle : { leading : List Node, cycle : List Node, network : SuperNetwork }
TerminalNumbers : [CYCLIC CyclicNumberPattern, NUM Nat]
CyclicNumberPattern : { modQuotient : Nat, modValue : Nat, mod : Nat }

solve : Documents -> Result Nat [ListWasEmpty]
solve = \docs ->
    network = Dict.fromList docs.network |> preprocessNetwork docs.turns
    startNodes = network.graph |> Dict.keys |> List.keepIf isStartNode
    startNodesCycle = startNodes |> List.map \node -> calculateCycle network node
    startNodesTerminalPatterns = List.map startNodesCycle getTerminalLcmsFromCycle

    terminalLcmsOrErr =
        lcms1, lcms2 <- walkFromFirst startNodesTerminalPatterns
        lcm1 <- lcms1 |> List.joinMap
        lcm2 <- lcms2 |> List.keepOks
        mergeTerminalNumbers lcm1 lcm2

    terminalLcms <- terminalLcmsOrErr |> Result.try
    terminalLcms |> List.map smallestNumberIn |> walkFromFirst Num.min

smallestNumberIn : TerminalNumbers -> Nat
smallestNumberIn = \terminalNum ->
    when terminalNum is
        CYCLIC { modQuotient, modValue, mod } -> modQuotient * mod + modValue
        NUM num -> num

mergeTerminalNumbers : TerminalNumbers, TerminalNumbers -> Result TerminalNumbers [NonIntersectingConstraints]
mergeTerminalNumbers = \num1, num2 ->
    when (num1, num2) is
        (NUM n1, NUM n2) -> if n1 == n2 then n1 |> NUM |> Ok else Err NonIntersectingConstraints
        (NUM n1, CYCLIC cycle2) -> mergeNumberAndPattern n1 cycle2 |> Result.map NUM
        (CYCLIC cycle1, NUM n2) -> mergeNumberAndPattern n2 cycle1 |> Result.map NUM
        (CYCLIC cycle1, CYCLIC cycle2) -> mergeCyclicNumberPatterns cycle1 cycle2 |> Result.map CYCLIC

mergeNumberAndPattern : Nat, CyclicNumberPattern -> Result Nat [NonIntersectingConstraints]
mergeNumberAndPattern = \num, { modQuotient, modValue, mod } ->
    if num % mod == modValue && num // mod >= modQuotient then
        Ok num
    else
        Err NonIntersectingConstraints

expect
    cycle1 = { mod: 5, modQuotient: 2, modValue: 2 }
    cycle2 = { mod: 7, modQuotient: 1, modValue: 3 }
    result = mergeCyclicNumberPatterns cycle1 cycle2
    exptResult = Ok { mod: 35, modQuotient: 0, modValue: 17 }
    result == exptResult
expect
    cycle1 = { mod: 5, modQuotient: 2, modValue: 2 }
    cycle2 = { mod: 7, modQuotient: 4, modValue: 3 }
    result = mergeCyclicNumberPatterns cycle1 cycle2
    exptResult = Ok { mod: 35, modQuotient: 1, modValue: 17 }
    result == exptResult
expect
    cycle1 = { mod: 2, modQuotient: 0, modValue: 1 }
    cycle2 = { mod: 6, modQuotient: 0, modValue: 4 }
    result = mergeCyclicNumberPatterns cycle1 cycle2
    exptResult = Err NonIntersectingConstraints
    result == exptResult
mergeCyclicNumberPatterns : CyclicNumberPattern, CyclicNumberPattern -> Result CyclicNumberPattern [NonIntersectingConstraints]
mergeCyclicNumberPatterns = \num1, num2 ->
    modsGcd = gcd num1.mod num2.mod
    if (num1.modValue % modsGcd) != (num2.modValue % modsGcd) then
        Err NonIntersectingConstraints
    else
        modsLcm = lcm num1.mod num2.mod
        modValue1 = num1.modValue // modsGcd
        modValue2 = num2.modValue // modsGcd
        mod1 = num1.mod // modsGcd
        mod2 = num2.mod // modsGcd

        value <- loop num1.modValue
        if value % num2.mod != num2.modValue then
            Continue ((value + num1.mod) % modsLcm)
        else
            {
                modQuotient: Num.max
                    (num1.modQuotient * num1.mod + num1.modValue)
                    (num2.modQuotient * num2.mod + num2.modValue)
                |> Num.subSaturated value
                |> Num.divCeil modsLcm,
                mod: modsLcm,
                modValue: value,
            }
            |> Ok
            |> Break

lcm : Nat, Nat -> Nat
lcm = \n1, n2 ->
    (n1 // (gcd n1 n2)) * n2

expect gcd 22 4 == 2
expect gcd 21 34 == 1
expect gcd 9 18 == 9
gcd : Nat, Nat -> Nat
gcd = \n1, n2 ->
    if n1 > n2 then
        gcd n2 n1
    else if n1 == 0 then
        n2
    else
        gcd (n2 % n1) n1

expect
    result =
        { leading: ["22A"], cycle: ["22C", "22B", "22Z"], network: testSuperNetwork }
        |> getTerminalLcmsFromCycle
    exptResult = [
        CYCLIC { modQuotient: 0, mod: 6, modValue: 3 },
        CYCLIC { modQuotient: 1, mod: 6, modValue: 0 },
    ]
    result == exptResult
getTerminalLcmsFromCycle : SuperNetworkCycle -> List TerminalNumbers
getTerminalLcmsFromCycle = \cycle ->
    leadingSuperLen = List.len cycle.leading
    cycleSuperLen = List.len cycle.cycle

    leadingNumbers =
        (node, index) <- cycle.leading |> List.mapWithIndex (\x, i -> (x, i)) |> List.joinMap

        terminals =
            Dict.get cycle.network.graph node
            |> okOrCrash "TODO handle error"
            |> .terminalIndices
        terminal <- terminals |> List.map
        terminal + index * cycle.network.superNodeLen

    cycleNumberPatterns =
        (node, index) <- cycle.cycle |> List.mapWithIndex (\x, i -> (x, i)) |> List.joinMap

        terminals =
            Dict.get cycle.network.graph node
            |> okOrCrash "TODO handle error"
            |> .terminalIndices
        terminal <- terminals |> List.map

        terminalNumber = terminal + (leadingSuperLen + index) * cycle.network.superNodeLen
        mod = cycleSuperLen * cycle.network.superNodeLen
        {
            mod,
            modValue: terminalNumber % mod,
            modQuotient: terminalNumber // mod,
        }

    List.concat (leadingNumbers |> List.map NUM) (cycleNumberPatterns |> List.map CYCLIC)

testSuperNetwork = {
    superNodeLen: 2,
    graph: Dict.fromList [
        ("11A", { postCycleNode: "11Z", terminalIndices: [] }),
        ("11B", { postCycleNode: "XXX", terminalIndices: [] }),
        ("11Z", { postCycleNode: "11Z", terminalIndices: [0] }),
        ("22A", { postCycleNode: "22C", terminalIndices: [] }),
        ("22B", { postCycleNode: "22Z", terminalIndices: [] }),
        ("22C", { postCycleNode: "22B", terminalIndices: [1] }),
        ("22Z", { postCycleNode: "22C", terminalIndices: [0] }),
        ("XXX", { postCycleNode: "XXX", terminalIndices: [] }),
    ],
}
expect
    result = calculateCycle testSuperNetwork "11A"
    exptResult = { leading: ["11A"], cycle: ["11Z"], network: testSuperNetwork }
    result == exptResult
expect
    result = calculateCycle testSuperNetwork "22A"
    exptResult = { leading: ["22A"], cycle: ["22C", "22B", "22Z"], network: testSuperNetwork }
    result == exptResult
calculateCycle : SuperNetwork, Node -> SuperNetworkCycle
calculateCycle = \network, startNode ->
    (prevNode, prevNodesByPos, prevPossByNode) <-
        loop (startNode, [startNode], {} |> Dict.empty |> Dict.insert startNode 0)

    nextNode = network.graph |> Dict.get prevNode |> okOrCrash "TODO handle errors" |> .postCycleNode
    if Dict.contains prevPossByNode nextNode then
        indexSplit = Dict.get prevPossByNode nextNode |> okOrCrash "confirmed key is present"

        Break {
            network,
            leading: prevNodesByPos |> List.takeFirst indexSplit,
            cycle: prevNodesByPos |> List.dropFirst indexSplit,
        }
    else
        nextNodesByPos = prevNodesByPos |> List.append nextNode
        nextPossByNode = prevPossByNode |> Dict.insert nextNode (List.len prevNodesByPos)

        Continue (nextNode, nextNodesByPos, nextPossByNode)

expect
    result =
        Dict.fromList [
            ("11A", { left: "11B", right: "XXX" }),
            ("11B", { left: "XXX", right: "11Z" }),
            ("11Z", { left: "11B", right: "XXX" }),
            ("22A", { left: "22B", right: "XXX" }),
            ("22B", { left: "22C", right: "22C" }),
            ("22C", { left: "22Z", right: "22Z" }),
            ("22Z", { left: "22B", right: "22B" }),
            ("XXX", { left: "XXX", right: "XXX" }),
        ]
        |> preprocessNetwork [LEFT, RIGHT]
    exptResult = {
        superNodeLen: 2,
        graph: Dict.fromList [
            ("11A", { postCycleNode: "11Z", terminalIndices: [] }),
            ("11B", { postCycleNode: "XXX", terminalIndices: [] }),
            ("11Z", { postCycleNode: "11Z", terminalIndices: [0] }),
            ("22A", { postCycleNode: "22C", terminalIndices: [] }),
            ("22B", { postCycleNode: "22Z", terminalIndices: [] }),
            ("22C", { postCycleNode: "22B", terminalIndices: [1] }),
            ("22Z", { postCycleNode: "22C", terminalIndices: [0] }),
            ("XXX", { postCycleNode: "XXX", terminalIndices: [] }),
        ],
    }
    result == exptResult
preprocessNetwork : Dict Node Fork, List Step -> SuperNetwork
preprocessNetwork = \network, steps ->
    newDictPairs =
        node <- network |> Dict.keys |> List.map
        newResult =
            { postCycleNode, terminalIndices }, step, index <-
                steps
                |> List.walkWithIndex { postCycleNode: node, terminalIndices: [] }

            nodeFork =
                Dict.get network postCycleNode
                |> okOrCrash "TODO handle errors"
            nextNode =
                when step is
                    LEFT -> nodeFork.left
                    RIGHT -> nodeFork.right
            newTerminals =
                if isDestinationNode postCycleNode then
                    List.append terminalIndices index
                else
                    terminalIndices
            { postCycleNode: nextNode, terminalIndices: newTerminals }
        (node, newResult)
    { superNodeLen: List.len steps, graph: newDictPairs |> Dict.fromList }

isStartNode = \node -> Str.endsWith node "A"

isDestinationNode = \node -> Str.endsWith node "Z"

walkFromFirst : List a, (a, a -> a) -> Result a [ListWasEmpty]
walkFromFirst = \list, fold ->
    when list is
        [] -> Err ListWasEmpty
        [item, ..] -> Ok (List.walkFrom list 1 item fold)

loop : state, (state -> [Break b, Continue state]) -> b
loop = \stateStart, stateStep ->
    when stateStep stateStart is
        Continue state -> loop state stateStep
        Break result -> result

okOrCrash : Result a *, Str -> a
okOrCrash = \result, crashMsg ->
    when result is
        Ok value -> value
        _ -> crash crashMsg

testInput =
    """
    LR

    11A = (11B, XXX)
    11B = (XXX, 11Z)
    11Z = (11B, XXX)
    22A = (22B, XXX)
    22B = (22C, 22C)
    22C = (22Z, 22Z)
    22Z = (22B, 22B)
    XXX = (XXX, XXX)

    """
