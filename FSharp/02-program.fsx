open System
open System.IO

let arrayWith index value array =
    let result = Array.copy array
    Array.set result index value
    result

let numbers = File.ReadAllText("FSharp/02-program-input.txt").Split(',')
              |> Array.map Int32.Parse

let numbersPart1 = numbers |> arrayWith 1 12 |> arrayWith 2 2

let rec processCode (numbers : int array) index =
    match numbers.[index] with
    | 99 -> numbers
    | o -> let op1 = numbers.[numbers.[index + 1]]
           let op2 = numbers.[numbers.[index + 2]]
           let result = match o with
                        | 1 -> op1 + op2
                        | 2 -> op1 * op2
                        | _ -> failwith "Incorrect input"
           processCode (numbers |> arrayWith (numbers.[index + 3]) result) (index + 4)

let result1 = processCode numbersPart1 0

let searchValue = 19690720

let _, noun, verb =
    seq {
        for noun in 0..99 do
            for verb in 0..99 do
                yield (noun, verb)
    }
    |> Seq.map (fun (n, v) ->
                    let adjustedNumbers = numbers |> arrayWith 1 n |> arrayWith 2 v
                    let finalNumbers = processCode adjustedNumbers 0
                    finalNumbers.[0], n, v)
    |> Seq.find (fun (r, _, _) -> r = searchValue)

let result2 = 100 * noun + verb