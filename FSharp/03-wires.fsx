open System
open System.IO

type Section =
| Horizontal of y : int * x1 : int * x2 : int * dist : int * dir : char
| Vertical of x : int * y1 : int * y2 : int * dist : int * dir : char

let lines = File.ReadAllLines "FSharp/03-wires-input.txt"
// let lines = [| "R8,U5,L5,D3"; "U7,R6,D4,L4" |]
// let lines = [| "R75,D30,R83,U83,L12,D49,R71,U7,L72"; "U62,R66,U55,R34,D71,R55,D58,R83" |]
// let lines = [| "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"; "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" |]

let rec parseSections (posx, posy) d (steps : string list) =
    match steps with
    | [] -> []
    | h::t -> let dir = h.[0]
              let dist = h.Substring(1) |> Int32.Parse
              let newSection, newPosition =
                  match dir with
                  | 'U' -> Vertical (posx, posy, posy + dist, d, dir), (posx, posy + dist)
                  | 'R' -> Horizontal (posy, posx, posx + dist, d, dir), (posx + dist, posy)
                  | 'D' -> Vertical (posx, posy - dist, posy, d, dir), (posx, posy - dist)
                  | 'L' -> Horizontal (posy, posx - dist, posx, d, dir), (posx - dist, posy)
                  | _ -> failwith "Incorrect input"
              newSection :: (parseSections newPosition (d + dist) t)

let sections1 = lines.[0].Split(',') |> List.ofArray |> parseSections (0, 0) 0
let sections2 = lines.[1].Split(',') |> List.ofArray |> parseSections (0, 0) 0

let dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let tryGetIntersection s1 s2 =
    let calcIntersection (y, x1, x2, disth, dirh) (x, y1, y2, distv, dirv) =
        if y >= y1 && y <= y2 && x >= x1 && x <= x2
        then match (x, y) with
             | (0, 0) -> None
             | (x, y) -> let dist = disth + distv +
                                    (if dirh = 'R' then (x - x1) else (x2 - x)) +
                                    (if dirv = 'U' then (y - y1) else (y2 - y))
                         Some (x, y, dist)
        else None

    match s1, s2 with
    | Horizontal (y, x1, x2, disth, dirh), Vertical (x, y1, y2, distv, dirv) -> calcIntersection (y, x1, x2, disth, dirh) (x, y1, y2, distv, dirv)
    | Vertical (x, y1, y2, distv, dirv), Horizontal (y, x1, x2, disth, dirh) -> calcIntersection (y, x1, x2, disth, dirh) (x, y1, y2, distv, dirv)
    | _ -> None 

let intersections =
    sections1
    |> List.collect (fun s1 -> sections2 |> List.choose (fun s2 -> tryGetIntersection s1 s2))

let x1, y1, result1 = intersections
                      |> List.minBy (fun (x, y, _) -> dist (0, 0) (x, y))
                      |> (fun (x, y, _) -> x, y, dist (0, 0) (x, y))

let x2, y2, result2 = intersections
                    |> List.minBy (fun (_, _, d) -> d)
