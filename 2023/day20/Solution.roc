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

solve : List Module -> Nat
solve = \modules ->
    circuit = initCircuit modules
    statesInit = initStates modules
    pulseCountsInit = { lows: 0, highs: 0 }

    (_, { lows: lowsTotal, highs: highsTotal }) =
        (statesMid, pulseCountsMid), _iterNum <-
            List.range { start: At 0, end: Length 1000 }
            |> List.walk (statesInit, pulseCountsInit)

        (nextStates, localQueue) = runCircuit statesMid circuit
        nextPulseCounts = {
            lows: pulseCountsMid.lows + (localQueue |> List.countIf (\m -> m.pulse == Low)),
            highs: pulseCountsMid.highs + (localQueue |> List.countIf (\m -> m.pulse == High)),
        }

        (nextStates, nextPulseCounts)

    lowsTotal * highsTotal

runCircuit : ModuleStates, ModuleIndex -> (ModuleStates, List PulseQueueItem)
runCircuit = \statesInit, circuit ->
    queueInit = [{ from: "button", to: "broadcaster", pulse: Low }]

    (statesMid, queueMid, indexMid) <- loop (statesInit, queueInit, 0)
    when queueMid |> List.get indexMid is
        Err _ -> Break (statesMid, queueMid)
        Ok { from, to, pulse } ->
            nextIndex = indexMid + 1

            toModuleOrErr = Dict.get circuit to
            if Result.isErr toModuleOrErr then
                Continue (statesMid, queueMid, nextIndex)
            else
                toModule = toModuleOrErr |> okOrCrash "TODO handle error"
                prevState = statesMid |> Dict.get to |> okOrCrash "TODO handle error \(to)"

                (nextState, nextPulses) = runModule prevState toModule from pulse

                nextStates = statesMid |> Dict.insert to nextState
                nextQueue = nextPulses |> List.walk queueMid List.append

                Continue (nextStates, nextQueue, nextIndex)

runModule : ModuleState, Module, Str, Pulse -> (ModuleState, List PulseQueueItem)
runModule = \state, module, from, inputPulse ->
    (nextState, nextPulse) =
        when (state, module) is
            (FF ffState, { type: FlipFlop, name: _, connectsTo: _ }) ->
                (nextFfState, nextFfPulse) = runFlipFlop ffState inputPulse
                (FF nextFfState, nextFfPulse)

            (C cState, { type: Conjunction, name, connectsTo: _ }) ->
                (nextCState, nextCPulse) = runConjunction cState inputPulse from
                (C nextCState, nextCPulse)

            (BC bcState, { type: Broadcaster, name: _, connectsTo: _ }) ->
                (nextBCState, nextBCPulse) = runBroadcaster bcState inputPulse
                (BC nextBCState, nextBCPulse)

            _ -> crash "module state doesn't match module type"

    outputPulses =
        when nextPulse is
            None -> []
            Sends outputPulse ->
                module.connectsTo
                |> List.map (\to -> { from: module.name, to, pulse: outputPulse })

    (nextState, outputPulses)

runFlipFlop : FlipFlopState, Pulse -> (FlipFlopState, OutputPulse)
runFlipFlop = \@FlipFlopState state, input ->
    when (state, input) is
        (_, High) -> (@FlipFlopState state, None)
        (On, Low) -> (@FlipFlopState Off, Sends Low)
        (Off, Low) -> (@FlipFlopState On, Sends High)

runConjunction : ConjunctionState, Pulse, Str -> (ConjunctionState, OutputPulse)
runConjunction = \@ConjunctionState prevPulses, input, index ->
    pulses = prevPulses |> Dict.insert index input
    output = if pulses |> Dict.values |> List.all (\p -> p == High) then Low else High
    (@ConjunctionState pulses, Sends output)

runBroadcaster : BroadcasterState, Pulse -> (BroadcasterState, OutputPulse)
runBroadcaster = \state, pulse -> (state, Sends pulse)

initStates : List Module -> ModuleStates
initStates = \modules ->
    statesList =
        { type, name, connectsTo } <- modules |> List.map
        state =
            when type is
                Broadcaster -> {} |> @BroadcasterState |> BC
                FlipFlop -> Off |> @FlipFlopState |> FF
                Conjunction ->
                    modules
                    |> List.keepIf (\m -> m.connectsTo |> List.contains name)
                    |> List.map (\m -> (m.name, Low))
                    |> Dict.fromList
                    |> @ConjunctionState
                    |> C
        (name, state)
    statesList |> Dict.fromList

initCircuit = \modules ->
    modules
    |> List.walk (Dict.empty {}) (\circuit, module -> Dict.insert circuit module.name module)

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

