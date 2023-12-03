# time

open System
open System.Text.RegularExpressions

let symbolRx = Regex(@"([^\d\.\n])")

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