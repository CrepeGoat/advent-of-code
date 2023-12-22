interface ParseInput
    exposes [parse, WorkflowsAndParts, Workflow, Part, Rule, WorkflowDestination]
    imports [
        parse.Core.{
            Parser,
            const,
            many,
            keep,
            skip,
            oneOf,
            oneOrMore,
            sepBy,
            sepBy1,
            map,
            between,
            flatten,
        },
        parse.String.{
            anyCodeunit,
            anyString,
            codeunit,
            codeunitSatisfies,
            digit,
            digits,
            string,
            strFromUtf8,
            parseStr,
        },
    ]

Input : WorkflowsAndParts
WorkflowsAndParts : { workflows : List Workflow, parts : List Part }
Workflow : { name : Str, rules : List Rule, finalDest : WorkflowDestination }
Rule : {
    category : [X, M, A, S],
    cmp : [LT, GT],
    value : U16,
    dest : WorkflowDestination,
}
WorkflowDestination : [WorkflowName Str, Reject, Accept]
Part : { x : U16, m : U16, a : U16, s : U16 }

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
        |> Result.map (\input -> (input.workflows |> List.len, input.parts |> List.len))
    result == Ok (11, 5)

parse : Str -> Result Input _
parse = \text -> parseStr parseInput text

parseInput : Parser _ Input
parseInput =
    newline = codeunit '\n'
    digitsU16 = const Num.toU16 |> keep digits
    isAlphaLower = \unit -> 'a' <= unit && unit <= 'z'

    wfname = (codeunitSatisfies isAlphaLower |> oneOrMore |> map strFromUtf8)
    workflowName =
        const WorkflowName
        |> keep wfname

    workflowAccept = const Accept |> skip (codeunit 'A')
    workflowReject = const Reject |> skip (codeunit 'R')
    dests = oneOf [workflowName, workflowAccept, workflowReject]

    catX = const X |> skip (codeunit 'x')
    catM = const M |> skip (codeunit 'm')
    catA = const A |> skip (codeunit 'a')
    catS = const S |> skip (codeunit 's')
    categories = oneOf [catX, catM, catA, catS]

    cmpLesser = const LT |> skip (codeunit '<')
    cmpGreater = const GT |> skip (codeunit '>')
    cmps = oneOf [cmpLesser, cmpGreater]

    rule =
        const (\category -> \cmp -> \value -> \dest -> { category, cmp, value, dest })
        |> keep categories
        |> keep cmps
        |> keep digitsU16
        |> skip (codeunit ':')
        |> keep dests

    workflow =
        const (\name -> \rules -> \finalDest -> { name, rules, finalDest })
        |> keep wfname
        |> skip (codeunit '{')
        |> keep (rule |> sepBy (codeunit ','))
        |> skip (codeunit ',')
        |> keep dests
        |> skip (codeunit '}')

    part =
        const (\x -> \m -> \a -> \s -> { x, m, a, s })
        |> skip (string "x=")
        |> keep digitsU16
        |> skip (string ",m=")
        |> keep digitsU16
        |> skip (string ",a=")
        |> keep digitsU16
        |> skip (string ",s=")
        |> keep digitsU16
        |> between (codeunit '{') (codeunit '}')

    input =
        const (\workflows -> \parts -> { workflows, parts })
        |> keep (workflow |> sepBy1 newline)
        |> skip newline
        |> skip newline
        |> keep (part |> sepBy1 newline)

    input |> skip (newline |> many)
