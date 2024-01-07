interface Heap
    exposes [
        Heap,
        empty,
        len,
        toList,
        push,
        pop,
        heapify,
        deHeapify,
        heapSort,
        toBinCmp,
    ]
    imports []

expect
    heap = empty Num.compare |> push 1 |> push 5 |> push 3 |> push 2 |> push 4
    heap |> deHeapify == [1, 2, 3, 4, 5]
expect
    heap = empty Num.compare |> push 1 |> push 3 |> push 3 |> push 1
    heap |> deHeapify == [1, 1, 3, 3]
Heap a := { heap : List a, cmp : a, a -> [LEQ, GT] }

empty : (a, a -> [LT, EQ, GT]) -> Heap a
empty = \cmp -> @Heap { heap: [], cmp: cmp |> toBinCmp }

heapSort : List a, (a, a -> [LT, EQ, GT]) -> List a
heapSort = \items, cmp ->
    items |> heapify cmp |> deHeapify

len : Heap * -> Nat
len = \@Heap { heap, cmp: _ } -> List.len heap

toList : Heap a -> List a
toList = \@Heap { heap, cmp: _ } -> heap

push : Heap a, a -> Heap a
push = \@Heap { heap, cmp }, item ->
    index = List.len heap
    @Heap { cmp, heap: heap |> List.append item } |> bubbleUpFrom index

pop : Heap a -> Result (Heap a, a) _
pop = \@Heap { heap, cmp } ->
    itemFirst <- List.first heap |> Result.map
    newList =
        heap
        |> List.swap 0 ((List.len heap) - 1)
        |> List.dropLast 1
    (@Heap { cmp, heap: newList } |> bubbleDownFrom 0, itemFirst)

deHeapify : Heap a -> List a
deHeapify = \heap ->
    (heapMid, resultMid) <- loop (heap, [])
    when pop heapMid is
        Err _ -> Break resultMid
        Ok (heapNext, item) -> Continue (heapNext, resultMid |> List.append item)

heapify : List a, (a, a -> [LT, EQ, GT]) -> Heap a
heapify = \items, cmp ->
    heapInit = @Heap { heap: items, cmp: cmp |> toBinCmp }
    when heapInit |> len |> Num.subChecked 1 |> Result.try indexUp is
        Err _ -> heapInit
        Ok indexInit ->
            (heapMid, indexMid) <- loop (heapInit, indexInit)
            heapNext = heapMid |> bubbleDownFrom indexMid
            if indexMid == 0 then
                Break heapNext
            else
                Continue (heapNext, indexMid - 1)

toBinCmp : (a, a -> [LT, EQ, GT]) -> (a, a -> [LEQ, GT])
toBinCmp = \cmp ->
    \x1, x2 ->
        when cmp x1 x2 is
            LT | EQ -> LEQ
            GT -> GT

# ##############################################################################

bubbleUpFrom : Heap a, Nat -> Heap a
bubbleUpFrom = \heap, index ->
    (heapMid, indexPrev) <- loop (heap, index)

    when indexUp indexPrev is
        Err _ -> Break heapMid
        Ok indexNext ->
            heapNext =
                when bubbleAbout heapMid indexNext is
                    Same -> heapMid
                    Swapped h _ -> h
            Continue (heapNext, indexNext)

bubbleDownFrom : Heap a, Nat -> Heap a
bubbleDownFrom = \heap, index ->
    (heapMid, indexMid) <- loop (heap, index)

    when bubbleAbout heapMid indexMid is
        Same -> Break heapMid
        Swapped h indexNext -> Continue (h, indexNext)

bubbleAbout : Heap a, Nat -> [Same, Swapped (Heap a) Nat]
bubbleAbout = \@Heap { heap, cmp }, index ->
    indexLeft = getIndexDownLeft index
    indexRight = indexLeft + 1

    if indexLeft >= List.len heap then
        Same
    else
        node = heap |> List.get index |> okOrCrash "checked index"

        leafNodes = List.sublist heap { start: indexLeft, len: 2 }
        (indexCmp, leafCmp) =
            when leafNodes is
                [x1, x2] if cmp x1 x2 == GT -> (indexRight, x2)
                [x1] | [x1, _] -> (indexLeft, x1)
                [] -> crash "case already handled earlier"
                [_, _, ..] -> crash "unreachable"
        when cmp node leafCmp is
            LEQ -> Same
            GT ->
                Swapped
                    (@Heap { cmp, heap: heap |> List.swap index indexCmp })
                    indexCmp

indexUp : Nat -> Result Nat [NoHigherNode]
indexUp = \index ->
    index
    |> Num.add 1
    |> Num.divTrunc 2
    |> Num.subChecked 1
    |> Result.mapErr (\_ -> NoHigherNode)

getIndexDownLeft : Nat -> Nat
getIndexDownLeft = \index -> index * 2 + 1

# getIndexDownRight : Nat -> Nat
# getIndexDownRight = \index -> (index + 1) * 2

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

