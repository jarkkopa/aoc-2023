# time

open System.Text.RegularExpressions

type Instruction = L | R

let nodeRx = Regex(@"(\w{3})\s=\s\((\w{3}),\s(\w{3})\)")
let input = "inputs/day08.txt" |> System.IO.File.ReadAllLines

"BBB = (AAA, ZZZ)" |> nodeRx.Match |> (fun x -> (x.Groups.[1].Value, x.Groups.[2].Value, x.Groups.[3].Value))

let toInstructions line =
    line
    |> Seq.toArray
    |> Array.map (fun x -> match x with | 'L' -> L | 'R' -> R | _ -> failwith "Unknown direction")

let instructions = input |> Array.head |> toInstructions

let toMap lines =
    lines
    |> Seq.map (fun line ->
        line
        |> nodeRx.Match
        |> (fun x -> (x.Groups.[1].Value, (x.Groups.[2].Value, x.Groups.[3].Value)))
    )
    |> Map.ofSeq

let nodeMap =
    input
    |> Array.removeManyAt 0 2
    |> toMap

let rec nextNode (instr: Instruction array)  steps current (routeMap: Map<string, (string * string)>) =
    let nextRoute = routeMap[current]
    let direction = instr.[(steps - 1) % instr.Length]
    let next = match direction with | L -> fst nextRoute | R -> snd nextRoute
    match next with
    | "ZZZ" -> steps
    | _ -> nextNode instr (steps + 1) next routeMap

let part1 =
    nodeMap
    |> nextNode instructions 1 "AAA"

part1 |> printfn "Part one: %A"