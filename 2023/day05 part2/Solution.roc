interface Solution
    exposes [solve]
    imports [
        ParseInput.{
            parse,
            Almanac,
            SuppliesMap,
            Range,
            MapRange,
        },
    ]

expect
    testInput
    |> parse
    |> Result.try solve
    == Ok 46

solve : Almanac -> Result U64 [NoSeeds]
solve = \almanac ->
    locationRanges = almanac.maps |> List.walk almanac.seedsRanges mapSuppliesRanges
    when locationRanges is
        [r1, ..] -> Ok r1.start
        [] -> Err NoSeeds

mapSuppliesRanges : List Range, SuppliesMap -> List Range
mapSuppliesRanges = \rangesIn, suppliesMap ->
    (result, ranges) <- while ([], rangesIn)

    when ranges is
        [] -> result |> Break
        [range, ..] ->
            rangesRest = ranges |> List.dropFirst 1
            index =
                mapRange <- suppliesMap.ranges |> bisect
                Num.compare (range.start + range.len) mapRange.start

            (resultInner, rangesInner) <- while ([], [range])

            
            rMid, mapRange <- suppliesMap.ranges |> List.walkFromUntil index [r]

# mappedRanges, seedsRange <- seedsRanges |> List.walk []

# seedsRange
# |> mapSuppliesRange suppliesMap
# |> sortRangesByStart
# |> mergeSortedLists mappedRanges (compareNumKey .start)
# |> mergeAdjacentIntersectingRanges

mergeAdjacentIntersectingRanges : List Range -> List Range
mergeAdjacentIntersectingRanges = \rangesIn ->
    (result, ranges) <- while ([], rangesIn)
    when ranges is
        [] -> result |> Break
        [r] -> result |> List.append r |> Break
        [r1, r2, ..] ->
            rangesRest = ranges |> List.dropFirst 2
            r1Break = r1.start + r1.len
            r2Break = r2.start + r2.len
            isIntersecting = r1.start < r2Break && r2.start < r1Break

            if isIntersecting then
                start = Num.min r1.start r2.start
                mergedRange = { start, len: (Num.max r1Break r2Break) - start }

                (result, rangesRest |> List.prepend mergedRange) |> Continue
            else
                (result |> List.append r1, rangesRest) |> Continue

mergeRanges : Range, Range -> {
    first: [TWO Range Range, ONE Range, ZERO],
    intersection: [ONE Range, ZERO],
    second: [TWO Range Range, ONE Range, ZERO],
}
mergeRanges = \range1, range2 ->
    crash "TODO"

mergeSortedLists : List a, List a, (a, a -> [LT, EQ, GT]) -> List a
mergeSortedLists = \list1In, list2In, cmp ->
    (result, list1, list2) <- while ([], list1In, list2In)
    when (list1, list2) is
        ([], []) -> result |> Break
        (_, []) -> List.join [result, list1] |> Break
        ([], _) -> List.join [result, list2] |> Break
        ([l1, ..], [l2, ..]) ->
            list1Rest = list1 |> List.dropFirst 1
            list2Rest = list2 |> List.dropFirst 1

            when cmp l1 l2 is
                GT -> (result |> List.append l2, list1, list2Rest) |> Continue
                _ -> (result |> List.append l1, list1Rest, list2) |> Continue

sortRangesByStart : List Range -> List Range
sortRangesByStart = \ranges ->
    ranges |> List.sortWith (compareNumKey .start)

sortMapRangesByStart : SuppliesMap -> SuppliesMap
sortMapRangesByStart = \suppliesMap ->
    sortedRanges = suppliesMap.ranges |> List.sortWith (compareNumKey .start)
    { suppliesMap & ranges: sortedRanges }

compareNumKey : (a -> Num *) -> (a, a -> _)
compareNumKey = \map -> \a1, a2 -> Num.compare (map a1) (map a2)

bisect : List a, (a -> [LT EQ GT]) -> [Before Nat, At Nat]
bisect = \list, fn ->
    (start, end) <- while (0, list.len)
    if start == end then
        start |> Before |> Break
    else
        mid = (start + end) / 2
        when fn mid is
            LT -> (start, mid) |> Continue
            EQ -> (At mid) |> Break
            GT -> (mid + 1, end) |> Continue

bisectLeft = \list, fn ->
    when bisect list fn is
        At n -> n
        Before n -> n

while : state, (state -> [Break b, Continue state]) -> b
while = \stateStart, stateStep ->
    when stateStep stateStart is
        Continue state -> while state stateStep
        Break result -> result

okOrCrash : Result a *, Str -> a
okOrCrash = \result, crashMsg ->
    when result is
        Ok value -> value
        _ -> crash crashMsg

testInput =
    """
    seeds: 79 14 55 13

    seed-to-soil map:
    50 98 2
    52 50 48

    soil-to-fertilizer map:
    0 15 37
    37 52 2
    39 0 15

    fertilizer-to-water map:
    49 53 8
    0 11 42
    42 0 7
    57 7 4

    water-to-light map:
    88 18 7
    18 25 70

    light-to-temperature map:
    45 77 23
    81 45 19
    68 64 13

    temperature-to-humidity map:
    0 69 1
    1 0 69

    humidity-to-location map:
    60 56 37
    56 93 4

    """
