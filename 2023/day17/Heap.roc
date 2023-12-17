interface Heap
    exposes [
        Heap,
        empty,
        len,
        toList,
        push,
        pop,
        popAll,
        toBinCmp,
    ]
    imports []

expect
    heap = empty Num.compare |> push 1 |> push 5 |> push 3 |> push 2 |> push 4
    heap |> popAll == [1, 2, 3, 4, 5]
expect
    heap = empty Num.compare |> push 1 |> push 3 |> push 3 |> push 1
    heap |> popAll == [1, 1, 3, 3]
Heap a := { heap : List a, cmp : a, a -> [LEQ, GT] }

empty : (a, a -> [LT, EQ, GT]) -> Heap a
empty = \cmp -> @Heap { heap: [], cmp: cmp |> toBinCmp }

len : Heap * -> Nat
len = \@Heap { heap, cmp: _ } -> List.len heap

toList : Heap a -> List a
toList = \@Heap { heap, cmp } -> heap

push : Heap a, a -> Heap a
push = \@Heap { heap, cmp }, item ->
    @Heap { cmp, heap: heap |> List.append item }
    |> bubbleTailUp

pop : Heap a -> Result (Heap a, a) _
pop = \@Heap { heap, cmp } ->
    itemFirst <- List.first heap |> Result.map
    newList =
        heap
        |> List.swap 0 ((List.len heap) - 1)
        |> List.dropLast 1
    (@Heap { cmp, heap: newList } |> bubbleHeadDown, itemFirst)

popAll : Heap a -> List a
popAll = \heap ->
    (heapMid, resultMid) <- loop (heap, [])
    when pop heapMid is
        Err _ -> Break resultMid
        Ok (heapNext, item) -> Continue (heapNext, resultMid |> List.append item)

toBinCmp : (a, a -> [LT, EQ, GT]) -> (a, a -> [LEQ, GT])
toBinCmp = \cmp ->
    \x1, x2 ->
        when cmp x1 x2 is
            LT | EQ -> LEQ
            GT -> GT

# ##############################################################################

bubbleTailUp : Heap a -> Heap a
bubbleTailUp = \heap ->
    indexTail = (len heap) - 1
    (heapMid, indexPrev) <- loop (heap, indexTail)

    when indexUp indexPrev is
        Err _ -> Break heapMid
        Ok indexNext ->
            heapNext =
                when bubbleAbout heapMid indexNext is
                    Ok Same -> heapMid
                    Ok (Swapped h _) -> h
                    Err _ ->
                        crash "started with valid index, moved up, confirmed non-negative"
            Continue (heapNext, indexNext)

bubbleHeadDown : Heap a -> Heap a
bubbleHeadDown = \heap ->
    (heapMid, indexPrev) <- loop (heap, 0)

    when bubbleAbout heapMid indexPrev is
        Ok Same | Err _ -> Break heapMid
        Ok (Swapped h i) -> Continue (h, i)

bubbleAbout : Heap a, Nat -> Result [Same, Swapped (Heap a) Nat] _
bubbleAbout = \@Heap { heap, cmp }, index ->
    node <- heap |> List.get index |> Result.map
    indexLeft = indexDownLeft index
    indexRight = indexDownRight index

    leafNodes = List.sublist heap { start: indexLeft, len: 2 }
    cmpToBase = leafNodes |> List.map (\x -> cmp node x)
    cmpBoth =
        nodeLeft <- List.first leafNodes |> Result.try
        nodeRight <- List.last leafNodes |> Result.map
        cmp nodeLeft nodeRight

    when (cmpToBase, cmpBoth) is
        ([], _) | ([LEQ], _) | ([LEQ, LEQ], _) -> Same
        ([GT], _) | ([GT, LEQ], _) | ([GT, GT], Ok LEQ) ->
            Swapped
                (@Heap { cmp, heap: heap |> List.swap index indexLeft })
                indexLeft

        ([LEQ, GT], _) | ([GT, GT], Ok GT) ->
            Swapped
                (@Heap { cmp, heap: heap |> List.swap index indexRight })
                indexRight

        # TODO compiler bug?
        # should be: ([_, _], Err _) -> crash "can't Err on 2 elements"
        ([_, _], _) -> crash "can't Err on 2 elements"
        ([_, _, _, ..], _) -> crash "at most 2 leaf elements"

indexUp : Nat -> Result Nat [NoHigherNode]
indexUp = \index ->
    index
    |> Num.add 1
    |> Num.divTrunc 2
    |> Num.subChecked 1
    |> Result.mapErr (\_ -> NoHigherNode)

indexDownLeft : Nat -> Nat
indexDownLeft = \index -> index * 2 + 1

indexDownRight : Nat -> Nat
indexDownRight = \index -> (index + 1) * 2

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

