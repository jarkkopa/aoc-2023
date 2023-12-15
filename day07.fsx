# time

type Hand = { bid: int; cards: char array }

let split (withStr: string) (s: string) = s.Split(withStr)

let input = "inputs/day07.txt" |> System.IO.File.ReadAllLines

let cardRanks = [|'A'; 'K'; 'Q'; 'J'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2'|]

let toHand line =
    line
    |> split " "
        { bid = x.[1] |> int; cards = x.[0] |> Seq.toArray }

let toHandRank (hand: Hand) =
    let grouped =
        hand.cards
        |> Array.groupBy id
        |> Array.sortByDescending (fun (_, cards) ->
            cards |> Array.length
        )

    let charCount (x: char * char array) =
        x |> snd |> Array.length
    let mostCommon = grouped |> Array.head |> charCount
    let secondCommon =
        grouped
        |> Array.tail
        |> (fun x ->
            match x with
            | [||] -> None
            | y -> y |> Array.head |> charCount |> Some
        )

    match (mostCommon, secondCommon) with
    | (most, _) when most = 5 -> 7 // five of a kind
    | (most, _) when most = 4  -> 6 // four of a kind
    | (most, Some second) when most = 3 && second = 2 -> 5 // full house
    | (most, _) when most = 3 -> 4 // three of a kind
    | (most, Some second) when most = 2 && second = 2 -> 3 //two pair
    | (most, _) when most = 2 -> 2 //one pair
    | _ -> 1

    let aRank = cardRanks |> Array.findIndex (fun x -> x = a)
    let bRank = cardRanks |> Array.findIndex (fun x -> x = b)
    bRank - aRank

let compareHandRanks a b =
    let aRank = toHandRank a
    let bRank = toHandRank b
    aRank - bRank

let compareHandCards a b =
    (a.cards, b.cards)
    ||> Array.zip
    |> Array.find (fun (aa, bb) -> aa <> bb)
    |> (fun (aa, bb) -> compareCards aa bb)

let compareHands a b =
    let primary = compareHandRanks a b
    
    match primary with
    | p when p = 0 -> compareHandCards a b
    | _ -> primary

let part1 =
    input
    |> Seq.map toHand
    |> Seq.sortWith compareHands
    |> Seq.mapi (fun i h ->
        (i + 1) * h.bid 
    )
    |> Seq.sum

part1 |> printfn "Part one: %A"