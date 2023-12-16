# time

type Hand = { bid: int; cards: char array }

let split (withStr: string) (s: string) = s.Split(withStr)

let input = "inputs/day07.txt" |> System.IO.File.ReadAllLines

let cardRanks = [|'A'; 'K'; 'Q'; 'J'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2'|]
let cardRanksWithJokers = [|'A'; 'K'; 'Q'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2'; 'J'|]

let toHand line =
    line
    |> split " "
    |> (fun x ->
        { bid = x.[1] |> int; cards = x.[0] |> Seq.toArray }
    )

let cardsToRank mostCommon secondCommon =
    match (mostCommon, secondCommon) with
    | (most, _) when most = 5 -> 7 // five of a kind
    | (most, _) when most = 4  -> 6 // four of a kind
    | (most, Some second) when most = 3 && second = 2 -> 5 // full house
    | (most, _) when most = 3 -> 4 // three of a kind
    | (most, Some second) when most = 2 && second = 2 -> 3 //two pair
    | (most, _) when most = 2 -> 2 //one pair
    | _ -> 1

let jokersToMostCommon grouped =
    let jokerIdx = grouped |> Array.tryFindIndex (fun (c, _) -> c = 'J' )
    match jokerIdx with
    | None -> grouped
    | Some idx ->
        let jokers = grouped.[idx] |> snd |> Array.length

        let withoutJokers = grouped |> Array.removeAt idx

        let jokersAdded =
            withoutJokers
            |> Array.head
            |> (fun (card, cards) ->
                let newJokers = Array.init jokers (fun _ -> 'J')
                (card, Array.append cards newJokers)
            )
        withoutJokers |> Array.updateAt 0 jokersAdded

let activateJokers (grouped: (char * char array) array) =
    let fiveJokers = grouped |> Array.head |> (fun (c, cards) -> c = 'J' && cards.Length = 5)
    let jokerReplacer =
        match fiveJokers with
        | true -> id
        | false -> jokersToMostCommon
    jokerReplacer grouped

let toHandRank jokers (hand: Hand) =
    let groupedWithoutJokers =
        hand.cards
        |> Array.groupBy id
        |> Array.sortByDescending (fun (_, cards) ->
            cards |> Array.length
        )

    let grouped =
        match jokers with
        | true -> activateJokers groupedWithoutJokers
        | _ -> groupedWithoutJokers

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

    cardsToRank mostCommon secondCommon

let compareCards jokers a b =
    let ranks = match jokers with | true -> cardRanksWithJokers | _ -> cardRanks
    let aRank = ranks |> Array.findIndex (fun x -> x = a)
    let bRank = ranks |> Array.findIndex (fun x -> x = b)
    bRank - aRank

let compareHandRanks jokers a b =
    let aRank = toHandRank jokers a
    let bRank = toHandRank jokers b
    aRank - bRank

let compareHandCards jokers a b =
    (a.cards, b.cards)
    ||> Array.zip
    |> Array.find (fun (aa, bb) -> aa <> bb)
    |> (fun (aa, bb) -> compareCards jokers aa bb)

let compareHands jokers a b =
    let primary = compareHandRanks jokers a b
    
    match primary with
    | p when p = 0 -> compareHandCards jokers a b
    | _ -> primary

let getWinnings input jokers =
    input
    |> Seq.map toHand
    |> Seq.sortWith (compareHands jokers)
    |> Seq.mapi (fun i h ->
        (i + 1) * h.bid 
    )
    |> Seq.sum

let part1 = getWinnings input false
part1 |> printfn "Part one: %A"

let part2 =  getWinnings input true
part2 |> printfn "Part one: %A"