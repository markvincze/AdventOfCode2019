open System
open System.IO

type PortalType = Inner | Outer

type Tile =
| Empty
| Wall
| Portal of string * PortalType
| Start
| End

type Direction = Up | Right | Down | Left

let lines = File.ReadAllLines "FSharp/20-donut-input.txt"
            |> Array.map Array.ofSeq

let mapWidth = lines.[0] |> Array.length
let mapHeight = lines |> Array.length

let move dir (x, y) = match dir with
                      | Up -> x, y - 1
                      | Right -> x + 1, y
                      | Down -> x, y + 1
                      | Left -> x - 1, y

let opposite dir = match dir with
                   | Up -> Down
                   | Right -> Left
                   | Down -> Up
                   | Left -> Right

let directions = [ Up; Right; Down; Left ]

let getChar (lines : char[][]) (x, y) =
    if x >= 0 && y >= 0 && x < mapWidth && y < mapHeight
    then lines.[y].[x]
    else '#'

let isPortal (lines : char[][]) pos =
    let x, y = pos
    match lines.[y].[x] with
    | '.' ->
        let neighbors = directions |> List.map (fun d -> (d, getChar lines (move d pos)))
        match neighbors |> List.tryFind (snd >> Char.IsUpper) with
        | None -> None
        | Some (d, c1) -> let portalType = match d with
                                           | Up -> if y > mapHeight / 2 then Inner else Outer
                                           | Right -> if x > mapWidth / 2 then Outer else Inner
                                           | Down -> if y > mapHeight / 2 then Outer else Inner
                                           | Left -> if x > mapWidth / 2 then Inner else Outer
                          let c2 = getChar lines (pos |> move d |> move d)
                          match d with
                          | Left | Up -> Some ((string c2 + string c1), portalType)
                          | Right | Down -> Some ((string c1 + string c2), portalType)
    | _ -> None

let tiles =
    seq {
        for y in 0..(mapHeight - 1) do
            for x in 0..(mapWidth - 1) do
                let c = lines.[y].[x]
                let tile = match c with
                           | ' ' -> Empty
                           | '.' -> match isPortal lines (x, y) with
                                    | None -> Empty
                                    | Some (s, _) when s = "AA" -> Start
                                    | Some (s, _) when s = "ZZ" -> End
                                    | Some (s, t) -> Portal (s, t)
                           | '#' -> Wall
                           | _ -> Wall
                ((x, y), tile)
    }
    |> Map.ofSeq

let findPortalOtherSide (tiles : Map<int * int, Tile>) pos label =
    tiles
    |> Map.toSeq
    |> Seq.find (fun (p, t) -> match (p, t) with
                               | (p, Portal (l, _)) when l = label && pos <> p -> true
                               | _ -> false)
    |> fst

let findRoute tiles =
    let rec findRoute (tiles : Map<int * int, Tile>) queue visited =
        match queue with
        | [] -> failwith "No route found"
        | (pos, dist, level) :: rest ->
            if Set.contains (pos, level) visited
            then findRoute tiles rest visited
            else match tiles.[pos] with
                 | End when level = 0 -> dist
                 | t ->
                     let newQueueItems = 
                        directions
                        |> List.map (fun d -> (move d pos), (tiles.[move d pos]))
                        |> List.filter (fun (_, t) -> match t with
                                                      | Empty | End | Start | Portal _ -> true
                                                      | _ -> false)
                        |> List.map (fun (p, _) -> (p, dist + 1, level))
                    
                     let newQueueItems = match t with
                                         | Portal (s, t) when level > 0 || t = Inner ->
                                            let newLevel = match t with
                                                           | Inner -> level + 1
                                                           | Outer -> level - 1
                                            ((findPortalOtherSide tiles pos s), dist + 1, newLevel) :: newQueueItems
                                         | _ -> newQueueItems
                    
                     findRoute tiles (List.append newQueueItems rest |> List.sortBy (fun (_, dist, _) -> dist)) (Set.add (pos, level) visited)

    let startPos = tiles
                   |> Map.toSeq
                   |> Seq.find (fun (p, t) -> match (p, t) with
                                              | (_, Start) -> true
                                              | _ -> false)
                   |> fst
    
    findRoute tiles [ (startPos, 0, 0) ] Set.empty
    
let result1 = findRoute tiles


