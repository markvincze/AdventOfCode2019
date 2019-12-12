open System.IO

let asteroidsInput = File.ReadAllLines "FSharp/10-monitoring-input.txt"
                     |> Array.map (fun l -> l |> Seq.map (fun c -> c = '#') |> Array.ofSeq)

let asteroids = Array2D.init (Array.length asteroidsInput) (Array.length asteroidsInput.[0]) (fun x y -> asteroidsInput.[x].[y])

let height = Array2D.length1 asteroids

let width = Array2D.length2 asteroids

let inside (x, y) = x >= 0 && y >= 0 && x < width && y < height

let rec gcd x y =
    if x <= 0 then y
    elif y <= 0 then x
    elif x = y then x
    elif x > y then gcd (x-y) y
    else gcd x (y-x)

type Quadrant = TopRight | BottomRight | BottomLeft | TopLeft

let raysInQuadrant quadrant x y =
    let orderBy = match quadrant with
                  | TopRight | BottomLeft -> (fun (x, y) -> if y = 0 then infinity else (float x) / (float y))
                  | BottomRight | TopLeft -> (fun (x, y) -> if x = 0 then infinity else (float y) / (float x))
    let op1, op2 = match quadrant with
                   | TopRight -> (+), (-)
                   | BottomRight -> (+), (+)
                   | BottomLeft -> (-), (+)
                   | TopLeft -> (-), (-)
    match quadrant with
    | TopRight | BottomLeft -> [ 0..(width - 1) ]
                               |> Seq.collect (fun x -> [ for y in 1..(height - 1) -> (x, y)])
    | BottomRight | TopLeft -> [ 1..(width - 1) ]
                               |> Seq.collect (fun x -> [ for y in 0..(height - 1) -> (x, y)])
    |> Seq.filter (fun (xd, yd) -> gcd xd yd = 1)
    |> Seq.sortBy orderBy
    |> Seq.map (fun (xd, yd) -> Seq.initInfinite (fun i -> op1 x (i * xd), op2 y (i * yd))
                                |> Seq.skip 1
                                |> Seq.takeWhile inside)

let raysFrom x y =
    [ raysInQuadrant TopRight x y
      raysInQuadrant BottomRight x y
      raysInQuadrant BottomLeft x y
      raysInQuadrant TopLeft x y ]
    |> Seq.concat

let observableCount (asteroids : bool[,]) x y =
    raysFrom x y
    |> Seq.filter (fun ray -> ray |> Seq.exists (fun (x, y) -> asteroids.[y, x]))
    |> Seq.length

let result1 = [ 0..(width - 1) ]
              |> List.collect (fun x -> [ for y in 0..(height - 1) -> (x, y)])
              |> List.filter (fun (x, y) -> asteroids.[y, x])
              |> List.map (fun (x, y) -> (x, y, (observableCount asteroids x y)))
              |> List.maxBy (fun (_, _, o) -> o)

let baseX, baseY, _ = result1

let rays = raysFrom baseX baseY
           |> Seq.map List.ofSeq
           |> List.ofSeq
           |> List.filter (List.isEmpty >> not) 

let shootRound rays shootCounter =
    let rec shootRound rays shootCounter newRays =
        match rays with
        | [] -> shootCounter, newRays |> List.rev
        | ray :: rayRest -> let rest = ray |> List.skipWhile (fun (x, y) -> not asteroids.[y, x])
                            match rest with
                            | [] -> shootRound rayRest shootCounter newRays
                            | (x, y) :: t ->
                                printfn "Shooting %d. asteroid at %d,%d" (shootCounter + 1) x y
                                asteroids.[y, x] <- false
                                shootRound rayRest (shootCounter + 1) (t :: newRays)

    shootRound rays shootCounter []
