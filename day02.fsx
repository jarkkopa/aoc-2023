# time

open System.Text.RegularExpressions

type Round = { Red: int; Green: int; Blue: int; }
type Game = { Id: int; Rounds: Round seq }

let trim (s: string) = s.Trim()
let split (withStr: string) (s: string) = s.Split(withStr)
let contains (what:string) (s:string) = s.Contains(what)

let gameRx = Regex(@"Game (\d*)")
let cubeRx color = Regex($"(\\d*) {color}")

let numOfCubes color (cubes: string) =
    let rx = cubeRx color
    if rx.IsMatch(cubes) then rx.Match(cubes).Groups.[1].Value |> int
    else 0

let toRound lines =
    let redLine = lines |> Seq.tryFind (contains "red")
    let greenLine = lines |> Seq.tryFind (contains "green")
    let blueLine = lines |> Seq.tryFind (contains "blue")
    let toVal a =
        match a with
        | Some a -> a
        | None -> ""
    {
        Red = redLine |> toVal |> numOfCubes "red";
        Green = greenLine |> toVal |> numOfCubes "green";
        Blue = blueLine |> toVal |> numOfCubes "blue";
    }

let lineToRound (line:string) =
    line
    |> trim
    |> split ","
    |> Seq.map trim
    |> toRound

let toGames (line:string) =
    let id = gameRx.Match(line).Groups.[1].Value |> int
    let roundLines = line.Split(":").[1].Split(";")
    let rounds =
        roundLines
        |> Seq.map lineToRound
    {Id = id; Rounds = rounds }

let gameIsLegal (game: Game) =
    game.Rounds
    |> Seq.fold (fun acc round ->
        acc && round.Red <= 12 && round.Green <= 13 && round.Blue <= 14
    ) true

let input = "inputs/day02.txt" |> System.IO.File.ReadAllLines

let part1 =
    input
    |> Seq.map toGames
    |> Seq.filter gameIsLegal
    |> Seq.map (fun r -> r.Id)
    |> Seq.sum

part1 |> printfn "Part one: %A"