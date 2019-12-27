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

let accessibleKeys vault collectedKeys pos =
    let addNeighbors queue pos dist =
        ((up pos), dist) ::
        ((right pos), dist) ::
        ((down pos), dist) ::
        ((left pos), dist) :: queue
        |> List.sortBy snd

    let rec accessibleKeys (vault : Tile[,]) collectedKeys queue visited accessibles =
        match queue with
        | [] -> accessibles
        | ((x, y), dist) :: rest ->
            if Set.contains (x, y) visited then accessibleKeys vault collectedKeys rest visited accessibles
            else match vault.[x, y] with
                 | Wall -> accessibleKeys vault collectedKeys rest visited accessibles
                 | Empty -> accessibleKeys vault collectedKeys (addNeighbors rest (x, y) (dist + 1)) (Set.add (x, y) visited) accessibles
                 | Door c -> if List.contains c collectedKeys
                             then accessibleKeys vault collectedKeys (addNeighbors rest (x, y) (dist + 1)) (Set.add (x, y) visited) accessibles
                             else accessibleKeys vault collectedKeys rest visited accessibles
                 | Key c -> if List.contains c collectedKeys
                            then accessibleKeys vault collectedKeys (addNeighbors rest (x, y) (dist + 1)) (Set.add (x, y) visited) accessibles
                            else accessibleKeys vault collectedKeys (addNeighbors rest (x, y) (dist + 1)) (Set.add (x, y) visited) (((x, y), dist, c) :: accessibles)

    accessibleKeys vault collectedKeys [ (pos, 0) ] Set.empty<int * int> []

// let ak = accessibleKeys vault [] startingPos

let allKeys =
    [ for x in 0..((String.length input.[0]) - 1) -> x ]
    |> List.collect (fun x -> [ for y in 0..((Array.length input) - 1) -> (x, y) ])
    |> List.filter (fun (x, y) -> match vault.[x, y] with
                                  | Key _ -> true
                                  | _ -> false)
    |> List.map (fun (x, y) -> match vault.[x, y] with
                               | Key c -> c
                               | _ -> failwith "")

// HandleCache item: Set<char>, 
// HandledCache: Map<Set<char> * (int * int), int>

// queue: pos, distSoFar, collectedKeys
let shortestSolution vault startingPos =
    let rec shortestSolution vault queue (handledCache: Map<Set<char> * (int * int), int>) bestSolution =
        match queue with
        | [] -> match bestSolution with
                | None -> failwith "No solution was found"
                | Some bestSolution -> bestSolution
        | pick :: rest ->
            let (pos, distSoFar, collectedKeys) = pick
            let handled = match Map.tryFind ((Set.ofList collectedKeys), pos) handledCache with
                          | None -> false
                          | (Some d) when d > distSoFar -> false
                          | _ -> //printfn "Already handled, skipping" 
                                 true
            if handled
            then shortestSolution vault rest handledCache bestSolution
            else let newHandledCache = Map.add ((Set.ofList collectedKeys), pos) distSoFar handledCache
                 if List.length collectedKeys = List.length allKeys
                 then let newBestSolution = match bestSolution with 
                                            | None -> distSoFar
                                            | Some d -> min d distSoFar
                      shortestSolution vault rest newHandledCache (Some newBestSolution)
                 else let ak = accessibleKeys vault collectedKeys pos
                      let newQueue = rest |> List.append (ak |> List.map (fun (pos, dist, key) -> pos, distSoFar + dist, key :: collectedKeys))
                      shortestSolution vault newQueue newHandledCache bestSolution

    shortestSolution vault [ (startingPos, 0, []) ] Map.empty<Set<char> * (int * int), int> None

// let ak = accessibleKeys vault [] startingPos
let result1 = shortestSolution vault startingPos


