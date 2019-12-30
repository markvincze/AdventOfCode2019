open System
open System.IO

type Tile =
| Empty
| Wall
| Key of char
| Door of char

let parse c = match c with
              | '.' | '@' -> Empty
              | '#' -> Wall
              | c when Char.IsLower c -> Key c
              | c when Char.IsUpper c -> Door (Char.ToLower c)
              | _ -> failwith "Invalid input"

let input = File.ReadAllLines "FSharp/18-manyworlds-input.txt"

let vault = Array2D.init (String.length input.[0]) (Array.length input)  (fun x y -> input.[y].[x] |> parse)

let startingPos =
    [ for x in 0..((String.length input.[0]) - 1) -> x ]
    |> List.collect (fun x -> [ for y in 0..((Array.length input) - 1) -> (x, y) ])
    |> List.find (fun (x, y) -> input.[y].[x] = '@')

let up (x, y) = (x, y - 1)
let right (x, y) = (x + 1, y)
let down (x, y) = (x, y + 1)
let left (x, y) = (x - 1, y)

type Direction = Up | Right | Down | Left

let move dir pos = match dir with
                   | Up -> up pos
                   | Right -> right pos
                   | Down -> down pos
                   | Left -> left pos

let opposite dir = match dir with
                   | Up -> Down
                   | Right -> Left
                   | Down -> Up
                   | Left -> Right

let hasCycle (vault : Tile[,]) startingPos startingFrom =
    let forwardDirections from =  [ Up; Right; Down; Left ] |> List.except [ (opposite from) ]
    let rec hasCycle (vault : Tile[,]) visited queue =
        // printfn "hasCycle, visited: %A, queue: %A" visited queue
        match queue with
        | [] -> false
        | ((x, y), from) :: rest ->
            let dirs = forwardDirections from
            // printfn "forwardDirections: %A" dirs
            if dirs |> List.exists (fun dir -> List.contains (move dir (x, y)) visited)
            then true
            else let newItems = dirs
                                |> List.map (fun dir -> let sx, sy = move dir (x, y)
                                                        match vault.[sx, sy] with
                                                        | Wall -> None
                                                        | _ -> Some ((sx, sy), dir))
                                |> List.choose id
                 hasCycle vault ((x, y) :: visited) (List.append newItems rest)
    hasCycle vault [] [(startingPos, startingFrom)]

let keyPositions =
    [ for x in 0..((String.length input.[0]) - 1) -> x ]
    |> List.collect (fun x -> [ for y in 0..((Array.length input) - 1) -> (x, y) ])
    |> List.filter (fun (x, y) -> match vault.[x, y] with
                                  | Key _ -> true
                                  | _ -> false)

let accessibleKeysForCache vault fromPos =
    let addNeighbors queue pos dist doors =
        ((up pos), dist, doors) ::
        ((right pos), dist, doors) ::
        ((down pos), dist, doors) ::
        ((left pos), dist, doors) :: queue
        |> List.sortBy (fun (_, d, _) -> d)

    let rec accessibleKeysForCache (vault : Tile[,]) queue visited start accessibles =
        match queue with
        | [] -> accessibles
        | ((x, y), dist, doors) :: rest ->
            if Set.contains (x, y) visited
            then accessibleKeysForCache vault rest visited start accessibles
            else match vault.[x, y] with
                 | Wall -> accessibleKeysForCache vault rest (Set.add (x, y) visited) start accessibles
                 | Empty -> accessibleKeysForCache vault (addNeighbors rest (x, y) (dist + 1) doors) (Set.add (x, y) visited) start accessibles
                 | Door c -> accessibleKeysForCache vault (addNeighbors rest (x, y) (dist + 1) (c :: doors)) (Set.add (x, y) visited) start accessibles
                 | Key c -> if (x, y) = start
                            then accessibleKeysForCache vault (addNeighbors rest (x, y) (dist + 1) doors) (Set.add (x, y) visited) start accessibles
                            else accessibleKeysForCache vault (addNeighbors rest (x, y) (dist + 1) doors) (Set.add (x, y) visited) start (((x, y), dist, c, doors) :: accessibles)

    accessibleKeysForCache vault [ (fromPos, 0, []) ] Set.empty<int * int> fromPos []

let accessibleCache =
    startingPos :: keyPositions
    |> List.map (fun pos -> (pos, (accessibleKeysForCache vault pos)))
    |> Map.ofList

let minDistBetweenTwoKeys =
    accessibleCache
    |> Seq.filter (fun kvp -> kvp.Key <> startingPos)
    |> Seq.map (fun kvp -> kvp.Value |> List.map (fun (_, dist, _, _ ) -> dist))
    |> Seq.collect id
    |> Seq.min

let accessibleKeys vault collectedKeys pos =
    match Map.tryFind pos accessibleCache with
    | None -> failwith "Missing cache item"
    | Some accessibles -> accessibles
                          |> List.filter (fun (_, _, key, _) -> collectedKeys |> Set.contains key |> not)
                          |> List.filter (fun (_, _, _, doorsPassed) ->  (doorsPassed |> List.forall (fun d -> Set.contains d collectedKeys)))
                          |> List.map (fun (keyPos, dist, key, _) -> keyPos, dist, key)

let allKeys =
    [ for x in 0..((String.length input.[0]) - 1) -> x ]
    |> List.collect (fun x -> [ for y in 0..((Array.length input) - 1) -> (x, y) ])
    |> List.filter (fun (x, y) -> match vault.[x, y] with
                                  | Key _ -> true
                                  | _ -> false)
    |> List.map (fun (x, y) -> match vault.[x, y] with
                               | Key c -> c
                               | _ -> failwith "")

let allKeyCount = allKeys |> List.length

let shortestSolution vault startingPos =
    let createHandledKey (collectedKeys : Set<char>) =
        collectedKeys
        |> Seq.sort
        |> Seq.map string
        |> String.concat ""

    let rec shortestSolution vault queue (handledCache: Map<string * (int * int), int>) bestSolution =
        let queue = queue
                    |> List.filter (fun (_, distSoFar, collectedKeys) -> (Option.isNone bestSolution) || (Option.get bestSolution) > (distSoFar + (minDistBetweenTwoKeys * (allKeyCount - (Set.count collectedKeys)))))
        match queue with
        | [] -> match bestSolution with
                | None -> failwith "No solution was found"
                | Some bestSolution -> bestSolution
        | pick :: rest ->
            let (pos, distSoFar, collectedKeys) = pick
            let handled = match Map.tryFind ((createHandledKey collectedKeys), pos) handledCache with
                          | None -> false
                          | (Some d) when d > distSoFar -> false
                          | _ -> true
            if handled
            then shortestSolution vault rest handledCache bestSolution
            else let newHandledCache = Map.add ((createHandledKey collectedKeys), pos) distSoFar handledCache
                 if Set.count collectedKeys = allKeyCount
                 then let newBestSolution = match bestSolution with 
                                            | None -> distSoFar
                                            | Some d -> min d distSoFar
                      shortestSolution vault rest newHandledCache (Some newBestSolution)
                 else let ak = accessibleKeys vault collectedKeys pos
                      let newQueue = rest
                                     |> List.append (ak |> List.map (fun (pos, dist, key) -> pos, distSoFar + dist, Set.add key collectedKeys))
                      shortestSolution vault newQueue newHandledCache bestSolution

    shortestSolution vault [ (startingPos, 0, Set.empty<char>) ] Map.empty<string * (int * int), int> None

let result1 = shortestSolution vault startingPos
