open System.IO

type Object = {
    Parent : string option
    Children : string list
}

let processOrbit objects (str : string) = 
    let pair = str.Split(')')
    let pair0 = match Map.tryFind pair.[0] objects with
                | None -> { Parent = None; Children = [ pair.[1] ] }
                | Some o -> { Parent = o.Parent; Children = pair.[1] :: o.Children }
    let pair1 = match Map.tryFind pair.[1] objects with
                | None -> { Parent = Some (pair.[0]); Children = [] }
                | Some o -> { Parent = Some (pair.[0]); Children = o.Children }
    objects
    |> Map.add pair.[0] pair0
    |> Map.add pair.[1] pair1

let objects = File.ReadAllLines "FSharp/06-orbit-input.txt"
              |> Array.fold processOrbit Map.empty<string, Object>

let sumOrbits objects object =
    let rec sumOrbits2 objects object acc =
        match object.Parent with
        | None -> acc
        | Some p -> sumOrbits2 objects (Map.find p objects) (acc + 1)

    sumOrbits2 objects object 0

let result1 = objects
              |> Map.toSeq
              |> Seq.sumBy (fun (_, o) -> sumOrbits objects o)

let accessible obj except =
    match obj.Parent with
    | None -> obj.Children 
    | Some p -> p :: obj.Children
    |> List.except [ except ]

let queue = [ ((Map.find "YOU" objects).Parent |> Option.get, 0, "YOU") ]

let rec findRoute objects queue =
    match queue with
    | [] -> failwith "No route found"
    | (objectName, distance, from) :: rest ->
        let object = Map.find objectName objects
        if object.Children |> List.contains "SAN"
        then distance
        else let queue = accessible object from
                         |> List.map (fun o -> (o, distance + 1, objectName))
                         |> List.append rest
             findRoute objects queue

let result2 = findRoute objects queue
