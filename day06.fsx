# time

open System
open System.Text.RegularExpressions

type Race = { time: int64; dist: int64 }

let raceRx title = Regex($@"{title}:\s+(((\d+)\s*)+)")

let input = "inputs/day06.txt" |> System.IO.File.ReadAllLines

let parseNumbers (title: string) line =
    let rx = raceRx title
    line
    |> rx.Match
    |> (fun m -> m.Groups.[3])
    |> (fun g -> g.Captures)
    |> Seq.map ((fun c -> c.Value))

let toRaces (lines: string array) = 
    let times = lines |> Array.head |> parseNumbers "Time" |> Seq.map int64
    let distances = lines |> Array.last |> parseNumbers "Distance" |> Seq.map int64

    (times, distances)
    ||> Seq.zip
    |> Seq.map (fun (time, dist) -> {time = time; dist = dist } )

let toOneRace lines = 
    let time = lines |> Array.head |> parseNumbers "Time" |> String.Concat |> int64
    let dist= lines |> Array.last |> parseNumbers "Distance" |> String.Concat |> int64

    {time = time; dist = dist}

let numOfWinningWays (time: int64) (dist: int64) =
    let a = -(float 1)
    let b = float time
    let c = -((float dist) + (float)1)
    let d = b*b - (float 4)*a*c
    
    let x1 = (-b + Math.Sqrt d) / (float 2)*a
    let x2 = (-b - Math.Sqrt d) / (float 2)*a
    let min = (Math.Ceiling x1) |> int
    let max =  (Math.Floor x2) |> int

    max - min + 1

let part1 =
    input
    |> toRaces
    |> Seq.map (fun race -> numOfWinningWays race.time race.dist )
    |> Seq.reduce (fun a c -> a * c)

part1 |> printfn "Part one: %A"

let part2 =
    input
    |> toOneRace
    |> (fun r -> numOfWinningWays r.time r.dist)

part2 |> printfn "Part two: %A"
