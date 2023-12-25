interface Solution
    exposes [solve]
    imports [
        ParseInput.{ parse, Module },
    ]

PulseCounts : { lows : Nat, highs : Nat }
ModuleIndex : Dict Str Module
ModuleStates : Dict Str ModuleState
ModuleState : [FF FlipFlopState, C ConjunctionState, BC BroadcasterState]
PulseQueue : List PulseQueueItem
PulseQueueItem : { from : Str, to : Str, pulse : Pulse }
BroadcasterState := {}
FlipFlopState := [On, Off]
ConjunctionState := Dict Str Pulse
Pulse : [Low, High]
OutputPulse : [Sends Pulse, None]

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
        |> Result.map solve
    exptResult = 32000000
    result == Ok exptResult

expect
    testInput =
        """
        broadcaster -> a
        %a -> inv, con
        &inv -> b
        %b -> con
        &con -> output
        """
    result =
        testInput
        |> parse
        |> Result.map solve
    exptResult = 11687500
    result == Ok exptResult

solve : List Module -> U32
solve = \modules ->
    circuit = initCircuit modules
    statesInit = initStates modules
    pulseCountsInit = { lows: 0, highs: 0 }

    (_, pulseCountsFinal) =
        (statesMid, pulseCountsMid) <-
            List.range { start: At 0, end: Length 1000 }
            |> List.walk (statesInit, pulseCountsInit)

        (nextStates, localPulseCounts) = runCircuit statesMid circuit
        nextPulseCounts = {
            lows: pulseCountsMid.lows + localPulseCounts.lows,
            highs: pulseCountsMid.highs + localPulseCounts.highs,
        }

        (nextStates, localPulseCounts)

    pulseCountsFinal

runCircuit : ModuleStates, ModuleIndex -> (ModuleStates, PulseCounts)
runCircuit = \statesInit, circuit ->
    queueInit = [{ from: "", to: "broadcaster", pulse: Low }]
    pulseCountsInit = { lows: 0, highs: 0 }

    (statesMid, pulseCountsMid, queueMid) <- loop (statesInit, pulseCountsInit, queueInit)
    when queueMid is
        [] -> Break (statesMid, pulseCountsMid)
        [{ from, to, pulse }, ..] ->
            queueRest = queueMid |> List.dropFirst 1
            prevState = statesMid |> Dict.get to |> okOrCrash "TODO handle error"
            toModule = Dict.get circuit to |> okOrCrash "TODO handle error"

            nextPulseCounts =
                when pulse is
                    Low -> { pulseCountsMid & lows: pulseCountsMid.lows + 1 }
                    High -> { pulseCountsMid & highs: pulseCountsMid.highs + 1 }

            (nextState, nextPulses) = runModule prevState toModule from pulse

            nextStates = statesMid |> Dict.insert to nextState
            nextQueue = nextPulses |> List.walk queueMid List.append

            Continue (nextStates, nextPulseCounts, nextQueue)

runModule : ModuleState, Module, Str, Pulse -> (ModuleState, List PulseQueueItem)
runModule = \state, module, from, inputPulse ->
    (nextState, nextPulse) =
        when (state, module) is
            (FF ffState, { type: FlipFlop, name: _, connectsTo: _ }) ->
                (nextFfState, nextFfPulse) = runFlipFlop ffState inputPulse
                (FF nextFfState, nextFfPulse)

            (C cState, { type: Conjunction, name, connectsTo: _ }) ->
                (nextCState, nextCPulse) = runConjunction cState inputPulse name
                (C nextCState, nextCPulse)

            (BC bcState, { type: Broadcaster, name: _, connectsTo: _ }) ->
                (nextBCState, nextBCPulse) = runBroadcaster bcState inputPulse
                (BC nextBCState, nextBCPulse)

    outputPulses =
        when nextPulse is
            None -> []
            Send outputPulse ->
                module.connectsTo
                |> List.map (\to -> { from: module.name, to, pulse: outputPulse })

    (nextState, outputPulses)

runFlipFlop : FlipFlopState, Pulse -> (FlipFlopState, OutputPulse)
runFlipFlop = \state, input ->
    when (state, input) is
        (_, High) -> (state, None)
        (@FlipFlopState On, Low) -> (@FlipFlopState Off, Sends Low)
        (@FlipFlopState Off, Low) -> (@FlipFlopState On, Sends High)

runConjunction : ConjunctionState, Pulse, Str -> (ConjunctionState, OutputPulse)
runConjunction = \@ConjunctionState prevPulses, input, index ->
    pulses = prevPulses |> List.set index input
    output = Sends (if pulses |> List.all (\p -> p == High) then Low else High)
    (@ConjunctionState pulses, output)

runBroadcaster : {}, Pulse -> ({}, OutputPulse)
runBroadcaster = \state, pulse -> (state, Sends pulse)

initStates : List Module -> ModuleStates
initStates = \modules ->
    statesList =
        { type, name, connectsTo } <- modules |> List.map
        when type is
            FlipFlop -> Off |> @FlipFlopState |> FF
            Conjunction -> List.repeat Low (List.len connectsTo) |> @ConjunctionState |> C
            Broadcaster -> {} |> @BroadcasterState |> BC
    statesList |> Dict.fromList

initCircuit = \modules ->
    modules
    |> List.walk (Dict.empty {}) (\module -> Dict.insert module.name module)

# ##############################################################################

loop : state, (state -> [Break b, Continue state]) -> b
loop = \stateInit, runIteration ->
    when runIteration stateInit is
        Continue state -> loop state runIteration
        Break result -> result

okOrCrash : Result a *, Str -> a
okOrCrash = \result, crashMsg ->
    when result is
        Ok value -> value
        _ -> crash crashMsg

