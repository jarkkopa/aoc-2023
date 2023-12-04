# time

open System
open System.Text.RegularExpressions

let split (withStr: string) (s: string) = s.Split(withStr)

let numberRx = Regex(@"(\d+)")

let input = "inputs/day04.txt" |> System.IO.File.ReadAllLines

let toGame line =
    line
    |> split ":"
    |> (fun x -> x.[1])
    |> split "|"
    |> Array.map (fun numbers ->
        numberRx.Matches(numbers)
        |> Seq.map (fun m -> m.Value |> int)
        |> Set.ofSeq
    )

let calculatePoints (numbers: Set<int>) =
    match numbers.Count with
    | c when c = 1 -> 1
    | c when c = 0 -> 0
    | c -> Math.Pow(2 |> double, c - 1 |> double) |> int

let part1 =
    input
    |> Array.map toGame
    |> Array.map (fun x -> Set.intersect x.[0] x.[1])
    |> Array.map calculatePoints
    |> Array.sum

part1 |> printfn "Part one: %A"