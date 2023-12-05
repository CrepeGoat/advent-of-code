interface Solution
    exposes [solve]
    imports [
        ParseInput.{
            parse,
            Almanac,
            SuppliesMap,
        },
    ]

expect
    testInput
    |> parse
    |> Result.map solve
    == Ok (ClosestSeed 35)

solve : Almanac -> [ClosestSeed U64, NoSeed]
solve = \almanac ->
    locations =
        almanac.seeds |> List.map \seed -> mapSeedToLocation almanac.maps seed

    when List.first locations is
        Err ListWasEmpty -> NoSeed
        Ok loc ->
            locations |> List.dropFirst 1 |> List.walk loc Num.min |> ClosestSeed

mapSeedToLocation : List SuppliesMap, U64 -> U64
mapSeedToLocation = \suppliesMaps, seed ->
    prevSupply, suppliesMap <- List.walk suppliesMaps seed

    containingRangeOrNone =
        range <- suppliesMap.ranges |> List.findFirst
        range.givenStart <= prevSupply && prevSupply < (range.givenStart + range.len)

    when containingRangeOrNone is
        Ok range -> (prevSupply - range.givenStart) + range.requiredStart
        Err _ -> prevSupply

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
