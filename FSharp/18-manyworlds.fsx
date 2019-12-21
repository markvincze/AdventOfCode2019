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

// queue: pos, distSoFar, collectedKeys
let shortestSolution vault startingPos =
    let rec shortestSolution vault queue bestSolution =
        // match 
        let queue = queue |> List.sortBy (fun (_, d, _) -> d)
        // printfn "shortestSolution, queue length: %d, bestSolution: %A, queue head: %A" (List.length queue) bestSolution (List.tryHead queue)
        match bestSolution with
        | None ->
            match queue with
            | (pos, distSoFar, collectedKeys) :: rest ->
              let ak = accessibleKeys vault collectedKeys pos
            //   printfn "List.length collectedKeys: %d, List.length allKeys: %d, ak: %A" (List.length collectedKeys) (List.length allKeys) ak
              if List.length collectedKeys = (List.length allKeys) - 1
              then let (_, dist, _) :: _ = ak
                   shortestSolution vault rest (Some (distSoFar + dist))
              else let newQueue = rest
                                  |> List.append
                                      (ak |> List.map (fun (pos, dist, key) -> pos, distSoFar + dist, key :: collectedKeys))
                   shortestSolution vault newQueue bestSolution
            | [] -> failwith "No solution was found"
        | Some bestSolution -> 
            match queue with
            | (_, distSoFar, _) :: _ when distSoFar >= bestSolution -> bestSolution
            | (pos, distSoFar, collectedKeys) :: rest ->
               let ak = accessibleKeys vault collectedKeys pos
               if List.length collectedKeys = (List.length allKeys) - 1
               then let (_, dist, _) :: _ = ak
                    shortestSolution vault rest (Some (distSoFar + dist))
               else let newQueue = rest
                                   |> List.append
                                       (ak |> List.map (fun (pos, dist, key) -> pos, distSoFar + dist, key :: collectedKeys))
                    shortestSolution vault newQueue (Some bestSolution)
             | [] -> bestSolution


    shortestSolution vault [ (startingPos, 0, []) ] None

// let ak = accessibleKeys vault [] startingPos
let result1 = shortestSolution vault startingPos