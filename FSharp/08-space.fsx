open System
open System.IO

let width = 25
let height = 6
let layerLength = width * height

let input = File.ReadAllText "FSharp/08-space-input.txt"
            |> Seq.toList

let layer = input
            |> List.chunkBySize layerLength
            |> List.minBy (fun l -> l |> List.filter (fun p -> p = '0') |> List.length)

let result1 = (layer |> List.filter (fun p -> p = '1') |> List.length) * (layer |> List.filter (fun p -> p = '2') |> List.length)

let layerCount = (input |> List.length) / layerLength

let solve2 () =
    for y in 0..height-1 do
        for x in 0..width-1 do
            let pixel = [ for l in 0..layerCount-1 -> input.[l * layerLength + y * width + x] ]
                        |> List.tryFind (fun p -> p <> '2')
            match pixel with
            | Some '0' -> printf " "
            | Some '1' -> printf "X"
            | _ -> printf " "
        printfn ""
