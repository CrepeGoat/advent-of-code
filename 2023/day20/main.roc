# references
# - https://github.com/roc-lang/examples/tree/main/examples/CommandLineArgs
app "day"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Stderr,
        pf.File,
        pf.Path,
        pf.Task,
        pf.Arg,
    ]
    provides [main] to pf

main : Task.Task {} I32
main =
    Stdout.line "result: hello"

ModuleStates : Dict Str ModuleState
ModuleState : [FF FlipFlopState, C ConjunctionState, BC BroadcasterState]
PulseQueue : List PulseQueueItem
PulseQueueItem : { from : Str, to : Str, pulse : Pulse }
BroadcasterState := {}
FlipFlopState := [On, Off]
ConjunctionState := Dict Str Pulse
Pulse : [Low, High]
OutputPulse : [Sends Pulse, None]

solve : List Module -> U32
solve = \modules ->
    0

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

okOrCrash : Result a *, Str -> a
okOrCrash = \result, crashMsg ->
    when result is
        Ok value -> value
        _ -> crash crashMsg

