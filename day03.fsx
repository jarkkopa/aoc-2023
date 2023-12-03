# time

open System
open System.Text.RegularExpressions

let symbolRx = Regex(@"([^\d\.\n])")
let gearRx = Regex(@"(\*)")

let matchNumbers line =
    Regex(@"(\d+)").Matches(line)
    |> Array.ofSeq

let containsSymbol (line: string) =
    symbolRx.IsMatch(line)

let hasAdjacentSymbol (lines: string array) xStart xEnd =
    lines
    |> Array.map (fun line ->
        let section = line.Substring(xStart, xEnd - xStart)
        containsSymbol section
    )
    |> Array.reduce (||)

let numbersWithAdjacentSymbol (inputLines: string array) (numbersInLine: Match array array) =
    numbersInLine
    |> Array.mapi (fun lineIndex matchLines ->
        matchLines
        |> Array.map (fun matchLine -> 
            let line = inputLines.[lineIndex]
            let yStart = Math.Max(0, lineIndex - 1)
            let yEnd = Math.Min(inputLines.Length, lineIndex + 1)
            let lines = inputLines.[yStart..yEnd]

            let xStart = Math.Max(0, matchLine.Index - 1)
            let xEnd = Math.Min(line.Length, matchLine.Index + matchLine.Length + 1)

            if hasAdjacentSymbol lines xStart xEnd then Some (matchLine.Value |> int) else None
        )
    )

let input = "inputs/day03.txt" |> System.IO.File.ReadAllLines

let part1 =
    input
    |> Array.map matchNumbers
    |> numbersWithAdjacentSymbol input
    |> Array.collect id
    |> Array.choose id
    |> Array.sum

part1 |> printfn "Part one: %A"

let matchGears line =
    gearRx.Matches(line)

let gearMatches =
    input
    |> Array.mapi (fun idx line -> (idx, (matchGears line)))
    |> Array.filter (fun x -> 
        let matches = x |> snd
        matches.Count > 0
    )

let gearPositions =
    gearMatches
    |> Array.map(fun (lineIndex, matches) ->
        matches
        |> Seq.map (fun x -> (x.Index, lineIndex) )
    )
    |> Seq.collect id
    |> Array.ofSeq

let numbersWithAdjacentGears (gears: (int*int) array) (numbersInLine: (int * Match array) array) =
    gears
    |> Array.map(fun (xGear, yGear) ->
        let numberMatches = numbersInLine |> Array.where (fun (numberY, _) -> yGear = numberY || yGear = numberY - 1 || yGear = numberY + 1)

        numberMatches
        |> Array.map snd
        |> Array.map (Array.filter (fun m ->
            let xStart = m.Index - 1
            let xEnd = m.Index + m.Length
            xGear >= xStart && xGear <= xEnd
        ))
    )

let part2 =
    input
    |> Array.mapi (fun idx line -> (idx, matchNumbers line))
    |> numbersWithAdjacentGears gearPositions
    |> Array.map (Array.collect (Array.map (fun x -> x.Value |> int)))
    |> Array.filter(fun x -> x.Length >= 2)
    |> Array.map (Array.reduce (fun a b -> a * b))
    |> Array.sum

part2 |> printfn "Part two: %A"