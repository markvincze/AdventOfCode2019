open System
open System.IO

// let input = File.ReadAllText "FSharp/16-fft-input.txt"
//             |> Seq.map (string >> Int32.Parse)
//             |> List.ofSeq
let rec repeat n list = match n with
                        | 0 -> Seq.empty
                        | n -> seq {
                                   yield! list
                                   yield! (repeat (n - 1) list)
                               }

let input = File.ReadAllText "FSharp/16-fft-input.txt"
            |> Seq.map (string >> Int32.Parse)
            |> repeat 10000
            |> List.ofSeq
            // |> List.rep

// let input = "80871224585914546619083218645595"
//             |> Seq.map (string >> Int32.Parse)
//             |> List.ofSeq

let multipliers outputNumber =
    Seq.initInfinite (fun _ ->
        seq {
            for _ in 0..outputNumber do
                yield 0
            for _ in 0..outputNumber do
                yield 1
            for _ in 0..outputNumber do
                yield 0
            for _ in 0..outputNumber do
                yield -1
        }
    ) |> Seq.collect id |> Seq.skip 1

let phase input =
    let inputLength = List.length input
    [ for i in 0..(inputLength - 1) -> i ]
    |> List.map
        ((fun i -> 
            (multipliers i)
            |> Seq.take inputLength
            |> List.ofSeq
            |> List.zip input
            |> List.sumBy (fun (a, b) -> a * b))
        >> (fun i -> abs (i % 10)))

let rec phaseAfterN input n =
    printfn "phaseAfterN, n: %d" n
    match n with
    | 0 -> input
    | n -> phaseAfterN (phase input) (n - 1)

let finalPhase = phaseAfterN input 100

// let result1 = finalPhase |> List.take 8 |> List.map string |> String.concat ""