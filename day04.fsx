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

let commonNumbers (numbers: Set<int> array) =
    Set.intersect numbers.[0] numbers.[1]

let part1 =
    input
    |> Array.map toGame
    |> Array.map commonNumbers
    |> Array.map calculatePoints
    |> Array.sum

part1 |> printfn "Part one: %A"

let winningRange card numOfWins =
    [(card + 1)..(card + numOfWins)]

let emptyCardMap = input |> Array.mapi (fun i _ -> (i + 1, 1)) |> Map.ofArray

let cardsWon (round: Set<int> array) =
    round
    |> Array.mapi (fun idx winNumbers ->
        (idx + 1, winNumbers |> Set.count)
    )
    |> Array.fold (fun (cardMap: Map<int,int>) curWinningNumbers ->
        let (first, last) = curWinningNumbers
        let cardsWon = winningRange first last
        
        cardsWon
        |> List.fold (fun numberMap number ->
            let old = numberMap.[number]
            let multiplier = numberMap.[curWinningNumbers |> fst]
            numberMap |> Map.change number (fun _ -> Some (old + multiplier))
        ) cardMap
    ) emptyCardMap

let part2 =
    input
    |> Array.map toGame
    |> Array.map commonNumbers
    |> cardsWon
    |> Map.toArray
    |> Array.map snd
    |> Array.sum

part2 |> printfn "Part two: %A"