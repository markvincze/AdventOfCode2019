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
        match queue with
        | [] -> false
        | ((x, y), from) :: rest ->
            let dirs = forwardDirections from
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

printfn "Determining starting positions"
let startingPositions =
    [ for x in 0..((String.length input.[0]) - 1) -> x ]
    |> List.collect (fun x -> [ for y in 0..((Array.length input) - 1) -> (x, y) ])
    |> List.filter (fun (x, y) -> input.[y].[x] = '@')

// let startingPositions =
//     [ for x in 0..((String.length input.[0]) - 1) -> x ]
//     |> List.collect (fun x -> [ for y in 0..((Array.length input) - 1) -> (x, y) ])
//     |> List.filter (fun (x, y) -> vault.[x, y] = '@')

printfn "Building accessible cache"
let accessibleCache =
    startingPositions
    |> List.append keyPositions
    |> List.map (fun pos -> (pos, (accessibleKeysForCache vault pos)))
    |> Map.ofList

printfn "Calculating min distance"
let minDistBetweenTwoKeys =
    accessibleCache
    |> Seq.filter (fun kvp -> List.contains kvp.Key startingPositions |> not)
    |> Seq.map (fun kvp -> kvp.Value |> List.map (fun (_, dist, _, _ ) -> dist))
    |> Seq.collect id
    |> Seq.sort
    |> Seq.tryHead
printfn "Min distance: %A" minDistBetweenTwoKeys

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

let rec replaceNth n item list =
    match n with
    | 0 -> match list with
           | _ :: tail -> item :: tail
           | [] -> failwith "The list is empty."
    | n -> match list with
           | head :: tail -> head :: (replaceNth (n - 1) item tail)
           | [] -> failwith "The list is empty."

let shortestSolution vault startingPositions =
    let createHandledKey (collectedKeys : Set<char>) (positions : List<int * int>) =
        (collectedKeys
        |> Seq.sort
        |> Seq.map string
        |> String.concat "")
        +
        (positions
        |> Seq.map (fun (x, y) -> sprintf "%d,%d," x y)
        |> String.concat "")

    let rec shortestSolution vault queue bestSolution =
        // printfn "shortestSolution, queue: %A, bestSolution: %A" queue bestSolution
        let queue = queue
                    |> List.filter
                        (fun states -> let totalDist = states |> List.sumBy (fun (_, d, _) -> d)
                                       let totalCollected = states |> List.sumBy (fun (_, _, collectedKeys) -> Set.count collectedKeys)
                                    //    (Option.isNone bestSolution) || (Option.get bestSolution) > (totalDist + totalCollected))
                                       (Option.isNone bestSolution) || (Option.get bestSolution) > (totalDist + ((Option.defaultValue 0 minDistBetweenTwoKeys) * (allKeyCount - totalCollected))))
        match queue with
        | [] -> match bestSolution with
                | None -> failwith "No solution was found"
                | Some bestSolution -> bestSolution
        | states :: rest ->
            if List.length states <> List.length startingPositions
            then failwith ("Incorrect states length: " + (sprintf "%A" states))
            else ()

            if List.sumBy (fun (_, _, collectedKeys) -> Set.count collectedKeys) states = allKeyCount
            then let newBestSolution = match bestSolution with 
                                       | None -> List.sumBy (fun (_, distSoFar, _) -> distSoFar) states
                                       | Some d -> min d (List.sumBy (fun (_, distSoFar, _) -> distSoFar) states)
                 printfn "Solution was found, states: %A" states
                 shortestSolution vault rest (Some newBestSolution)
            else 
                 let allCollectedKeys = states
                                        |> List.map (fun (_, _, collectedKeys) -> collectedKeys)
                                        |> Set.unionMany
                 let newQueueItems =
                    states
                    |> List.mapi (fun i (pos, distSoFar, collectedKeys) -> 
                                    accessibleKeys vault allCollectedKeys pos
                                    |> List.map (fun (pos, dist, key) -> states |> replaceNth i (pos, distSoFar + dist, Set.add key collectedKeys)))
                    |> List.collect id
                 shortestSolution vault (List.append rest newQueueItems) bestSolution

    let initialQueueItem = startingPositions |> List.map (fun p -> p, 0, Set.empty<char>)
    shortestSolution vault [ initialQueueItem ] None

// let initialQueueItem = startingPositions |> List.map (fun p -> p, 0, Set.empty<char>)
let result1 = shortestSolution vault startingPositions
