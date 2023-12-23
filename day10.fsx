# time

type Pos = { x: int; y: int }
type Direction = N | W | S | E
type Symbol = NS | EW | NE | NW | SW | SE | Ground | Start
type Node = {symbol: Symbol; dist: int option}

let split (withStr: string) (s: string) = s.Split(withStr)

let input = "inputs/day10.txt" |> System.IO.File.ReadAllLines |> List.ofArray

let width = input |> List.sortByDescending (fun x -> x.Length) |> List.length
let height = input.Length

let toSymbol = function
    | '|' -> NS 
    | '-' -> EW
    | 'L' -> NE
    | 'J' -> NW
    | '7' -> SW
    | 'F' -> SE
    | '.' -> Ground
    | 'S' -> Start
    | _ -> failwith "Unknown symbol"
    
let nodeMap =
    input
    |> Seq.indexed
    |> Seq.map (fun (i, s) ->
        (i, s |> Seq.indexed |> List.ofSeq)
    )
    |> Seq.map (fun (y, line) ->
        line
        |> Seq.map (fun (x, c) -> ({x = x; y = y}, { symbol = (toSymbol c); dist = None}))
    )
    |> Seq.collect id
    |> Map.ofSeq

let startPos =
    nodeMap
    |> Map.findKey (fun _ v -> v.symbol = Start)

// let startNode =
//     nodeMap
//     |> Map.find startPos

let findConnectedPos (nodes: Map<Pos, Node>) pos direction =
    let neighbour =
        match direction with
        | N when pos.y > 0 -> Some { pos with y = pos.y - 1}
        | W when pos.x > 0 -> Some { pos with x = pos.x - 1}
        | S when pos.y < height -> Some { pos with y = pos.y + 1 }
        | E when pos.x < width -> Some { pos with x = pos.x + 1 }
        | _ -> None
    // printfn "Neighbour: %A, pos: %A direction: %A" neighbour pos direction
    match neighbour with
    | None -> None
    | Some n ->
        let node = nodes[n]

        match direction with
        | N when [NS; SW; SE] |> List.contains node.symbol -> neighbour
        | W when [EW; NE; SE] |> List.contains node.symbol -> neighbour
        | S when [NS; NE; NW] |> List.contains node.symbol -> neighbour
        | E when [EW; SW; NW] |> List.contains node.symbol -> neighbour
        | _ -> None

let firstNodes = 
    [N; W; S; E]
    |> List.map (fun dir -> findConnectedPos nodeMap startPos dir)
    |> List.choose id

let nextDirection (nodes: Map<Pos, Node>) fromPos fromDir =
    let symbol = nodes[fromPos].symbol
    // printfn "nextDirection fromPos: %A, fromDir: %A, symbol: %A" fromPos fromDir symbol
    match symbol with
    | NS when fromDir = S -> N
    | NS when fromDir = N -> S
    | EW when fromDir = E -> W
    | EW when fromDir = W -> E
    | NE when fromDir = N -> E
    | NE when fromDir = E -> N
    | NW when fromDir = N -> W
    | NW when fromDir = W -> N
    | SW when fromDir = S -> W
    | SW when fromDir = W -> S
    | SE when fromDir = S -> E
    | SE when fromDir = E -> S
    | Start when fromDir = N -> S
    | Start when fromDir = W -> E
    | Start when fromDir = S -> N
    | Start when fromDir = E -> W
    | _ -> failwith "Can't find nextDirection"

let prevDirection fromPos toPos =
    let diff = {x = toPos.x - fromPos.x; y = toPos.y - fromPos.y }
    // printfn "prevDirection: fromPos: %A, toPos: %A, diff: %A" fromPos toPos diff
    match diff with
    | {x = 0; y = y } when y > 0 -> N
    | {x = 0; y = y } when y < 0 -> S
    | {x = x; y = 0 } when x > 0 -> W
    | {x = x; y = 0 } when x < 0 -> E
    | _ -> failwith "Unknown prev direction"

let nextDirection2 curSymbol fromDir =
    match curSymbol with
    | NS when fromDir = S -> N
    | NS when fromDir = N -> S
    | EW when fromDir = E -> W
    | EW when fromDir = W -> E
    | NE when fromDir = N -> E
    | NE when fromDir = E -> N
    | NW when fromDir = N -> W
    | NW when fromDir = W -> N
    | SW when fromDir = S -> W
    | SW when fromDir = W -> S
    | SE when fromDir = S -> E
    | SE when fromDir = E -> S
    | Start when fromDir = N -> S
    | Start when fromDir = W -> E
    | Start when fromDir = S -> N
    | Start when fromDir = E -> W
    | _ -> failwith "Can't find nextDirection2"

let prevDirection2 fromDir =
    match fromDir with
    | N -> S
    | S -> N
    | W -> E
    | E -> W

let foo sym dir =
    let pD =prevDirection2 dir
    nextDirection2 sym pD

let updateSteps (nodes: Map<Pos,Node>) key (steps) =
    let old = nodes.[key]
    let oldDist = nodes[key] |> (fun x -> x.dist)
    let newDist =
        match (oldDist, steps) with
        | (None, s) -> s
        | (o, s) when s < o -> s
        | (o, _) -> o
    nodes |> Map.change key (fun _ -> Some {old with dist = newDist})

let rec travel steps (fromPos: Pos) (toDir: Direction) nodes =
    // printfn "TRAVEL"
    let nextNodePos = findConnectedPos nodes fromPos toDir
    // printfn "Travel from: %A, toDir: %A, nextNode: %A" fromPos toDir nextNodePos

    match nextNodePos with
    | Some next ->
        // let fromDir = prevDirection fromPos next
        let nextSymbol = nodes[next].symbol
        let prevDir = prevDirection2 toDir
        let nextDir = nextDirection2 nextSymbol prevDir
        // printfn "nextSymbol: %A, nextDir: %A, prevDir: %A"nextSymbol nextDir prevDir
        let newSteps = steps + 1
        // printfn "nextDir: %A, newSteps: %A" nextDir newSteps

        let updatedNodes = updateSteps nodes next (Some newSteps)
        travel newSteps next nextDir updatedNodes
    | _ -> nodes

let startDirections =
    firstNodes
    |> List.map ((prevDirection startPos) >> nextDirection nodeMap startPos)

let part1 =
    travel 0 startPos startDirections.[0] nodeMap
    |> travel 0 startPos startDirections.[1]
    |> Map.toList
    |> List.map snd
    |> List.sortByDescending (fun n -> n.dist)
    |> List.head
    |> (fun n -> n.dist)

part1 |> printfn "Part one: %A"