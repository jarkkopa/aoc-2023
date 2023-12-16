# time

open System.Text.RegularExpressions

type Instruction = L | R

let nodeRx = Regex(@"(\w{3})\s=\s\((\w{3}),\s(\w{3})\)")
let input = "inputs/day08.txt" |> System.IO.File.ReadAllLines

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

let rec nextNode goal (instr: Instruction array)  steps current (routeMap: Map<string, (string * string)>) =
    let nextRoute = routeMap[current]
    let direction = instr.[(steps - 1) % instr.Length]
    let next = match direction with | L -> fst nextRoute | R -> snd nextRoute

    match goal next with
    | true -> steps
    | false -> nextNode goal instr (steps + 1) next routeMap

let part1 =
    nodeMap
    |> nextNode (fun n -> n = "ZZZ") instructions 1 "AAA"

part1 |> printfn "Part one: %A"

let startNodes = nodeMap |> Map.toArray |> Array.filter (fun (x,_) -> x.EndsWith "A") |> Array.map fst;; 

let stepsToGoal =
    startNodes
    |> Array.map (fun n ->
        nodeMap
        |> nextNode (fun n -> n.EndsWith "Z") instructions 1 n
        |> int64
    )

let rec gcd (a: int64) (b: int64) =
    match b with
    | 0L -> abs a
    | _ -> gcd b (a % b)

let lcmPair (a: int64) (b: int64) = a * b / (gcd a b)

let rec lcm = function
    | [a;b] -> lcmPair a b
    | head::tail -> lcmPair (head) (lcm (tail))
    | [] -> 1

let part2 =
    stepsToGoal
    |> List.ofArray
    |> lcm

part2 |> printfn "Part two: %A"