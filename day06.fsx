# time

open System
open System.Text.RegularExpressions

type Race = { time: int; dist: int }

let raceRx title = Regex($@"{title}:\s+(((\d+)\s*)+)")

let input = "inputs/day06.txt" |> System.IO.File.ReadAllLines

let parseNumbers (title: string) line =
    let rx = raceRx title
    line
    |> rx.Match
    |> (fun m -> m.Groups.[3])
    |> (fun g -> g.Captures)
    |> Seq.map ((fun c -> c.Value) >> int)

let toRaces (lines: string array) = 
    let times = lines |> Array.head |> parseNumbers "Time"
    let distances = lines |> Array.last |> parseNumbers "Distance"

    (times, distances)
    ||> Seq.zip
    |> Seq.map (fun (time, dist) -> {time = time; dist = dist} )

let winTimes time dist =
    Seq.init (time + 1) id
    |> Seq.map (fun t -> t * (time - t))
    |> Seq.filter (fun t -> t > dist)
    |> Seq.length

input
    |> toRaces
    |> Seq.map (fun race -> winTimes race.time race.dist) 
    |> Array.ofSeq
    |> Array.reduce (fun a c -> a * c)