# time

open System

let input = "inputs/day01.txt" |> System.IO.File.ReadAllLines

let isNumber value =
    Char.IsDigit value

let firstAndLast (a:char array) =
    let first = a |> Array.find isNumber
    let last = a |> Array.findBack isNumber
    let str = new String([|first; last|])
    str |> int

let part1 =
    input
    |> Array.map Seq.toArray
    |> Array.map firstAndLast
    |> Array.sum

let spellToDigit (value: string) =
    match value with
    | v when v.StartsWith "one" -> Some '1'
    | v when v.StartsWith "two" -> Some '2'
    | v when v.StartsWith "three" -> Some '3'
    | v when v.StartsWith "four" -> Some '4'
    | v when v.StartsWith "five" -> Some '5'
    | v when v.StartsWith "six" -> Some '6'
    | v when v.StartsWith "seven" -> Some '7'
    | v when v.StartsWith "eight" -> Some '8'
    | v when v.StartsWith "nine" -> Some '9'
    | _ -> None

let getDigit value =
    match value with
    | "" -> None
    | v when isNumber v.[0] -> Some v.[0]
    | v -> spellToDigit v

let rec calibrate (digits:char[]) (value:char list)  =
    let valueStr = String.concat "" <| List.map string value
    let newDigit = getDigit valueStr

    let newDigits = 
        match newDigit with
        | Some d -> Array.append digits [|d|]
        | None -> digits
    match value with
    |  _::tail -> calibrate newDigits tail
    | _ -> digits

let firstAndLastToInt a =
    let first = Array.head a
    let last = Array.last a
    let str = new String([|first; last|])
    str |> int

let part2 =
    input
    |> Array.map Seq.toList
    |> Array.map (calibrate [||])
    |> Array.map firstAndLastToInt
    |> Array.sum

part1 |> printfn "Part one: %A"
part2 |> printfn "Part two: %A"