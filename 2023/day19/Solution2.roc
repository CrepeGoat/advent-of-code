interface Solution2
    exposes [solve]
    imports [
        ParseInput.{ parse, WorkflowsAndParts, Workflow, Part, Rule, WorkflowDestination },
    ]

PartRange : { x : NumRange, m : NumRange, a : NumRange, s : NumRange }
NumRange : { start : U16, end : U16 }

expect
    testInput =
        """
        px{a<2006:qkq,m>2090:A,rfg}
        pv{a>1716:R,A}
        lnx{m>1548:A,A}
        rfg{s<537:gd,x>2440:R,A}
        qs{s>3448:A,lnx}
        qkq{x<1416:A,crn}
        crn{x>2662:A,R}
        in{s<1351:px,qqz}
        qqz{s>2770:qs,m<1801:hdj,R}
        gd{a>3333:R,R}
        hdj{m>838:A,pv}

        {x=787,m=2655,a=1222,s=2876}
        {x=1679,m=44,a=2067,s=496}
        {x=2036,m=264,a=79,s=2244}
        {x=2461,m=1339,a=466,s=291}
        {x=2127,m=1623,a=2188,s=1013}

        """
    result =
        testInput
        |> parse
        |> Result.map solve
    exptResult = 167409079868000
    result == Ok exptResult

solve : WorkflowsAndParts -> U64
solve = \{ workflows, parts: _ } ->
    workflowIndex =
        index, wf <- workflows |> List.walk (Dict.empty {})
        Dict.insert index wf.name wf

    acceptedParts =
        (queue, acceptedMid) <- loop ([("in", fullPartRange {})], [])

        when queue is
            [] -> Break acceptedMid
            [(dest, partRange), ..] ->
                queueRest = queue |> List.dropFirst 1

                workflow = Dict.get workflowIndex dest |> okOrCrash "TODO handle errors"
                { processing, accepted } = runWorkflow workflow partRange

                Continue (
                    queueRest |> List.concat processing,
                    acceptedMid |> List.concat accepted,
                )

    acceptedParts
    |> List.map countPartsInRange
    |> List.sum

runWorkflow : Workflow, PartRange -> { processing : List (Str, PartRange), accepted : List PartRange }
runWorkflow = \{ name: _, rules, finalDest }, partRange ->
    (partsProcessingDefault, partsAssignedFinal) =
        (partsProcessing, partsAssigned), rule <- rules |> List.walkUntil (Some partRange, [])

        when partsProcessing is
            None -> Break (partsProcessing, partsAssigned)
            Some part ->
                { mapTo, doesNotMap } = runWorkflowRule rule part

                partsAssignedNext =
                    when mapTo is
                        Some a -> partsAssigned |> List.append a
                        None -> partsAssigned

                Continue (doesNotMap, partsAssignedNext)

    allParts =
        when partsProcessingDefault is
            None -> partsAssignedFinal
            Some p -> partsAssignedFinal |> List.append (finalDest, p)

    partsToProcess =
        (dest, part) <- allParts |> List.keepOks
        when dest is
            WorkflowName name -> Ok (name, part)
            _ -> Err "ignore accepteds and rejecteds"
    partsAccepted =
        (dest, part) <- allParts |> List.keepOks
        when dest is
            Accept -> Ok part
            _ -> Err "ignore processings and rejecteds"
    { processing: partsToProcess, accepted: partsAccepted }

runWorkflowRule : Rule, PartRange -> { mapTo : Option (WorkflowDestination, PartRange), doesNotMap : Option PartRange }
runWorkflowRule = \{ category, cmp, value, dest }, part ->
    getCat =
        when category is
            X -> .x
            M -> .m
            A -> .a
            S -> .s
    catRange = part |> getCat

    when (cmp, Num.compare catRange.start value, Num.compare catRange.end value) is
        (GT, GT, _) | (LT, _, LT) -> { mapTo: Some (dest, part), doesNotMap: None }
        (GT, _, GT) ->
            (mappedCatRange, unmappedCatRange) = (
                { catRange & start: value + 1 },
                { catRange & end: value },
            )
            (mappedPart, unmappedPart) =
                when category is
                    X -> ({ part & x: mappedCatRange }, { part & x: unmappedCatRange })
                    M -> ({ part & m: mappedCatRange }, { part & m: unmappedCatRange })
                    A -> ({ part & a: mappedCatRange }, { part & a: unmappedCatRange })
                    S -> ({ part & s: mappedCatRange }, { part & s: unmappedCatRange })

            { mapTo: Some (dest, mappedPart), doesNotMap: Some unmappedPart }

        (LT, LT, _) ->
            (mappedCatRange, unmappedCatRange) = (
                { catRange & end: value - 1 },
                { catRange & start: value },
            )
            (mappedPart, unmappedPart) =
                when category is
                    X -> ({ part & x: mappedCatRange }, { part & x: unmappedCatRange })
                    M -> ({ part & m: mappedCatRange }, { part & m: unmappedCatRange })
                    A -> ({ part & a: mappedCatRange }, { part & a: unmappedCatRange })
                    S -> ({ part & s: mappedCatRange }, { part & s: unmappedCatRange })

            { mapTo: Some (dest, mappedPart), doesNotMap: Some unmappedPart }

        (GT, _, _) | (LT, _, _) -> { mapTo: None, doesNotMap: Some part }

countPartsInRange : PartRange -> U64
countPartsInRange = \{ x, m, a, s } ->
    [x, m, a, s] |> List.map countNumsInRange |> List.product

countNumsInRange : NumRange -> U64
countNumsInRange = \{ start, end } ->
    end - start + 1 |> Num.toU64

fullPartRange : {} -> PartRange
fullPartRange = \_ ->
    x = fullNumRange {}
    { x, m: x, a: x, s: x }

fullNumRange : {} -> NumRange
fullNumRange = \_ ->
    { start: 1, end: 4000 }

# ##############################################################################

Option a : [Some a, None]

walkFromFirst : List a, (a, a -> a) -> Result a [ListWasEmpty]
walkFromFirst = \list, fold ->
    when list is
        [] -> Err ListWasEmpty
        [item, ..] -> Ok (List.walkFrom list 1 item fold)

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

okOr : Result a err, (err -> a) -> a
okOr = \result, xformErr ->
    when result is
        Ok a -> a
        Err e -> xformErr e

okOrTry : Result a err, (err -> Result a err) -> Result a err
okOrTry = \result, xformErr ->
    when result is
        Ok a -> Ok a
        Err e -> xformErr e
