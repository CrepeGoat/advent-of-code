interface Solution2
    exposes [solve]
    imports [
        ParseInput.{ parse, WorkflowsAndParts, Workflow, Part, Rule, WorkflowDestination },
    ]

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
    exptResult = 19114
    result == Ok exptResult

solve : WorkflowsAndParts -> U32
solve = \{ workflows, parts } ->
    workflowIndex =
        index, wf <- workflows |> List.walk (Dict.empty {})
        Dict.insert index wf.name wf

    acceptedParts =
        part <- parts |> List.keepIf
        wfDest <- loop (WorkflowName "in")

        when wfDest is
            Accept -> Break Bool.true
            Reject -> Break Bool.false
            WorkflowName name ->
                workflow =
                    Dict.get workflowIndex name
                    |> okOrCrash "TODO handle errors"

                workflow.rules
                |> List.walk
                    (Err DoesNotApplyToPart)
                    (\x, rule -> okOrTry x (\_ -> runWorkflowRule rule part))
                |> Result.withDefault workflow.finalDest
                |> Continue

    acceptedParts
    |> List.joinMap (\part -> [part.x, part.m, part.a, part.s])
    |> List.map Num.toU32
    |> List.sum

runWorkflowRule : Rule, Part -> Result WorkflowDestination [DoesNotApplyToPart]
runWorkflowRule = \{ category, cmp, value, dest }, part ->
    getCat =
        when category is
            X -> .x
            M -> .m
            A -> .a
            S -> .s
    catValue = part |> getCat

    when (Num.compare catValue value, cmp) is
        (LT, LT) | (GT, GT) -> Ok dest
        _ -> Err DoesNotApplyToPart

# ##############################################################################

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
