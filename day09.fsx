# time

let split (withStr: string) (s: string) = s.Split(withStr)

let input = "inputs/day09.txt" |> System.IO.File.ReadAllLines

let histories =
    input
    |> List.ofArray
    |> List.map (split " ")
    |> List.map (List.ofArray >> List.map int)

let rec toDiffs (fullValues: int list list) values =
    let pairs = values |> List.pairwise
    let diffs = pairs |> List.map (fun (a, b) -> b - a)
    let allZeros = diffs |> List.where (fun x -> x <> 0) |> List.isEmpty
    let newFullValues = List.append [diffs] fullValues
    match allZeros with
    | true -> newFullValues
    | false ->
        toDiffs newFullValues diffs

let rec extrapolateLast (values: int list list) =
    match values with
    | head :: next :: tail ->
        let addValue = (head |> List.last) + (next |> List.last)
        let newNext = next @ [addValue]
        extrapolateLast ([newNext] @ tail)
    | _ -> values |> List.collect id

let part1 =
    histories
    |> List.map (fun h -> toDiffs [h] h)
    |> List.map (extrapolateLast >> List.last)
    |> List.sum

part1 |> printfn "Part one: %A"
