# time

open System

type Pos = { x: int; y: int }

let input = "inputs/day11.txt" |> System.IO.File.ReadAllLines |> List.ofArray

let calculateDist emptyRows emptyColumns a b =
    let xDist = (b.x - a.x) |> abs
    let yDist = (b.y - a.y) |> abs
    let additionalColumns =
        emptyColumns
        |> List.filter (fun c ->
            let ceil = Math.Max(a.x, b.x)
            let floor = Math.Min(a.x, b.x)
            c < ceil && c > floor
        )
        |> List.length
    let additionalRows =
        emptyRows
        |> List.filter (fun c ->
            let ceil = Math.Max(a.y, b.y)
            let floor = Math.Min(a.y, b.y)
            c < ceil && c > floor
        )
        |> List.length
    xDist + yDist + additionalColumns + additionalRows

let galaxies =
    input
    |> Seq.indexed
    |> Seq.map (fun (i, s) ->
        (i, s |> Seq.indexed |> List.ofSeq)
    )
    |> Seq.map (fun (y, line) ->
        line
        |> Seq.map (fun (x, c) -> ({x = x; y = y}, c))
    )
    |> Seq.collect id
    |> Seq.filter (fun (_, c) -> c = '#')
    |> Seq.mapi (fun i (pos, _) -> (pos, i + 1))
    |> List.ofSeq

let width = input.[0].Length
let height = input.Length

let containsGalaxy (galaxies: (Pos * int) list) (getProp: Pos -> int) rowOrCol=
    galaxies
    |> List.filter (fun (p, _) -> (getProp p) = rowOrCol )
    |> List.isEmpty

let getEmptyRange ceil getProp =
    [0..ceil-1]
    |> List.filter (fun i -> containsGalaxy galaxies getProp i )
let emptyColumns =
    getEmptyRange width (fun p -> p.x)
let emptyRows =
    getEmptyRange height (fun p -> p.y)

let rec toPairs list =
    match list with
    | [] -> []
    | head::tail ->
        List.map (fun x ->
            (head, x)) tail @ toPairs tail

let part1 =
    galaxies
    |> toPairs
    |> List.map (fun ((a, _), (b, _)) -> calculateDist emptyRows emptyColumns a b)
    |> List.sum

part1 |> printfn "Part one: %A"