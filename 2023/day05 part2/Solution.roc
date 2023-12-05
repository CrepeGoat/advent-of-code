interface Solution
    exposes [solve]
    imports [
        ParseInput.{
            parse,
            Almanac,
            SuppliesMap,
            SeedsRange,
        },
    ]

expect
    testInput
    |> parse
    |> Result.try solve
    == Ok 46

solve : Almanac -> Result U64 [NoSeeds]
solve = \almanac ->
    locationRanges = almanac.maps |> List.walk seeds mapSuppliesRanges
    when locationRanges is
        [r1, ..] -> r1.start
        [] -> Err NoSeeds

mapSuppliesRanges : List SeedsRange, SuppliesMap -> List SeedsRange
mapSuppliesRanges = \seedsRanges, suppliesMap ->
    mappedRanges, seedsRange <- seedsRanges |> List.walk []

    seedsRange
    |> mapSuppliesRange suppliesMap
    |> sortSeedsRangesByStart
    |> mergeSortedLists mappedRanges compareRangesByStart
    |> mergeAdjacentIntersectingRanges

mapSuppliesRange : SeedsRange, SuppliesMap -> List SeedsRange
mapSuppliesRange = \seedsRange, suppliesMap ->
    crash "TODO"

mergeAdjacentIntersectingRanges : List SeedsRange -> List SeedsRange
mergeAdjacentIntersectingRanges = \ranges ->
    result, ranges <- while ([], ranges)

    when ranges is
        [] -> result |> End
        [r] -> result |> List.append r |> End
        [r1, r2, ..] ->
            rangesRest = ranges |> List.dropFirst 2
            r1End = r1.start + r1.len
            r2End = r2.start + r2.len
            isIntersecting = r1.start < r2End && r2.start < r1End

            if isIntersecting then
                mergedRange = {
                    start: Num.min r1.start r2.start,
                    len: (Num.max r1End r2End) - start,
                }
                (result, rangesRest |> List.prepend mergedRange) |> Continue
            else
                (result |> List.append r1, rangesRest) |> Continue

mergeSortedLists : List a, List a, (a, a -> [LT, EQ, GT]) -> List a
mergeSortedLists = \list1, list2, cmp ->
    result, list1, list2, cmp <- while ([], list1, list2)
    when (list1, list2) is
        ([], []) -> result |> End
        (_, []) -> List.join result list1 |> End
        ([], _) -> List.join result list2 |> End
        ([l1, ..], [l2, ..]) ->
            list1Rest = list1 |> List.dropFirst 1
            list2Rest = list2 |> List.dropFirst 1

            when cmp l1 l2 is
                GT -> (result |> List.append l2, list1, list2Rest) |> Continue
                _ -> (result |> List.append l1, list1Rest, list2) |> Continue

sortSeedsRangesByStart : List SeedsRange -> List SeedsRange
sortSeedsRangesByStart = \seedsRanges ->
    seedsRanges |> List.sortWith compareRangesByStart

sortMapRangesByStart : SuppliesMap -> SuppliesMap
sortMapRangesByStart = \suppliesMap ->
    sortedRanges = suppliesMap.ranges |> List.sortWith compareRangesByStart
    { suppliesMap & ranges: sortedRanges }

compareRangesByStart = \range1, range2 -> Num.compare range1.start range2.start

while : state, (state -> [End *, Continue state]) -> state
while = \stateStart, stateStep ->
    when stateStep stateStart is
        Continue state -> stateStep state
        End result -> result

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
