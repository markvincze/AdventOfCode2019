type Moon = {
    Position : int * int * int
    Velocity : int * int * int
}

let rec applyGravity otherMoons moon =
    match otherMoons with
    | [] -> moon
    | h :: t -> let x, y, z = moon.Position
                let ox, oy, oz = h.Position
                let dx = if x > ox then -1 elif x < ox then 1 else 0
                let dy = if y > oy then -1 elif y < oy then 1 else 0
                let dz = if z > oz then -1 elif z < oz then 1 else 0
                let vx, vy, vz = moon.Velocity
                applyGravity t { moon with Velocity = vx+dx, vy+dy, vz+dz }

let handleGravity moons =
    let rec handleGravity moons moonsLeft =
        match moonsLeft with
        | [] -> []
        | h :: t -> (applyGravity (moons |> List.except [ h ]) h) :: (handleGravity moons t)
    handleGravity moons moons

let move moon =
    let x, y, z = moon.Position
    let vx, vy, vz = moon.Velocity
    { moon with Position = x + vx, y + vy, z + vz }

let rec handleMove moons =
    match moons with
    | [] -> []
    | h :: t -> (move h) :: handleMove t

let step moons =
    moons
    |> handleGravity
    |> handleMove

let potentialEnergy moon = let x, y, z = moon.Position
                           abs x + abs y + abs z

let kineticEnergy moon = let vx, vy, vz = moon.Velocity
                         abs vx + abs vy + abs vz

let totalEnergy moon = potentialEnergy moon * kineticEnergy moon

let moons = [
    { Position = -9, 10, -1; Velocity = 0, 0, 0 }
    { Position = -14, -8, 14; Velocity = 0, 0, 0 }
    { Position = 1, 5, 6; Velocity = 0, 0, 0 }
    { Position = -19, 7, 8; Velocity = 0, 0, 0 }
]

let moonsAfter1000Steps = [ 1..1000 ] |> List.fold (fun moons _ -> step moons) moons

let result1 = moonsAfter1000Steps
              |> List.sumBy totalEnergy

let rec findMatch moons moonsInitial cmp cnt =
    let newMoons = step moons
    if cmp newMoons moonsInitial
    then cnt
    else findMatch newMoons moonsInitial cmp (cnt + 1L)

let cmpX moons1 moons2 = 
    List.zip moons1 moons2
    |> List.forall (fun ({ Position = x1, _, _; Velocity = vx1, _, _}, { Position = x2, _, _; Velocity = vx2, _, _}) -> x1 = x2 && vx1 = vx2)

let cmpY moons1 moons2 = 
    List.zip moons1 moons2
    |> List.forall (fun ({ Position = _, y1, _; Velocity = _, vy1, _}, { Position = _, y2, _; Velocity = _, vy2, _}) -> y1 = y2 && vy1 = vy2)

let cmpZ moons1 moons2 = 
    List.zip moons1 moons2
    |> List.forall (fun ({ Position = _, _, z1; Velocity = _, _, vz1}, { Position = _, _, z2; Velocity = _, _, vz2}) -> z1 = z2 && vz1 = vz2)

let repX = findMatch moons moons cmpX 1L
let repY = findMatch moons moons cmpY 1L
let repZ = findMatch moons moons cmpZ 1L

let rec gcd x y = if y = 0L then abs x else gcd y (x % y)
let lcm x y = x * y / (gcd x y)

let result2 = lcm (lcm repX repY) repZ