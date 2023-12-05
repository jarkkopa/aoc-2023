# time

open System.Text.RegularExpressions

type Route = { destStart: int64; sourceStart: int64; length: int64 }

let split (withStr: string) (s: string) = s.Split(withStr)
let trim (s: string) = s.Trim()

let mapRx title = Regex($@".*{title}:\s(((\d+\s*)+))")

let input = "inputs/day05.txt" |> System.IO.File.ReadAllText

let seeds =
    input |> (mapRx "seeds").Match |> (fun m -> m.Groups.[3].Captures) |> Seq.map (fun x -> x.Value |> int64)

let toRoutes mapLines = 
    mapLines
    |> Array.map (split " " >> Array.map int64)
    |> Array.map (fun numbers -> { destStart = numbers.[0]; sourceStart = numbers.[1]; length = numbers.[2] })

let toMap mapTitle mapData =
    mapData
    |> (mapRx mapTitle).Match
    |> (fun m -> m.Groups.[1].Captures)
    |> Seq.map (fun m -> m.Value)
    |> Seq.head
    |> trim
    |> split "\n"
    |> toRoutes

let seedToSoil =
    input |> toMap "seed-to-soil map"
let soilToFertilized =
    input |> toMap "soil-to-fertilizer map"
let fertilizerToWater =
    input |> toMap "fertilizer-to-water map"
let waterToLight =
    input |> toMap "water-to-light map"
let lightToTemperature =
    input |> toMap "light-to-temperature map"
let temperatureToHumidity =
    input |> toMap "temperature-to-humidity map"
let humidityToLocation =
    input |> toMap "humidity-to-location map"

let toDest  (routes: Route array) value =
    routes
    |> Array.tryFind (fun r ->
        match value with
        | x when x >= r.sourceStart && x < r.sourceStart + r.length -> true
        | _ -> false
    )
    |> (fun route ->
        match route with
        | Some r -> r.destStart + value - r.sourceStart
        | None -> value
    )

let part1 = 
    seeds
    |> Seq.map (toDest seedToSoil
        >> toDest soilToFertilized
        >> toDest fertilizerToWater
        >> toDest waterToLight
        >> toDest lightToTemperature
        >> toDest temperatureToHumidity
        >> toDest humidityToLocation
    )
    |> Seq.sortBy id
    |> Seq.head

part1 |> printfn "Part one: %A"