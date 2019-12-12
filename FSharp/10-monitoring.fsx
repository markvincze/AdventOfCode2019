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

let raysFrom x y =
    let rays x y xd yd =
        if xd = 0
        then [((+), (+)); ((+), (-))]
        elif yd = 0
        then [((+), (+)); ((-), (+))]
        else [((+), (+)); ((-), (+)); ((+), (-)); ((-), (-))]
        |> Seq.map (fun (op1, op2) -> Seq.initInfinite (fun i -> op1 x (i * xd), op2 y (i * yd)))

    [ 0..(width - 1) ]
    |> Seq.collect (fun x -> [ for y in 0..(height - 1) -> (x, y)])
    |> Seq.filter (fun (xd, yd) -> gcd xd yd = 1)
    |> Seq.collect (fun (xd, yd) -> rays x y xd yd)
    |> Seq.map (Seq.skip 1 >> Seq.takeWhile inside)

let observableCount (asteroids : bool[,]) x y =
    raysFrom x y
    |> Seq.filter (fun ray -> ray |> Seq.exists (fun (x, y) -> asteroids.[y, x]))
    |> Seq.length

let result1 = [ 0..(width - 1) ]
              |> List.collect (fun x -> [ for y in 0..(height - 1) -> (x, y)])
              |> List.filter (fun (x, y) -> asteroids.[y, x])
              |> List.map (fun (x, y) -> (x, y, (observableCount asteroids x y)))
              |> List.maxBy (fun (_, _, o) -> o)
