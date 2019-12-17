open System
open System.IO

type Material = int64 * string

type Reaction = {
    Input : Material list
    Output : Material
}

let parseMaterial (str : string) =
    let parts = str.Trim().Split ' '
    (Int64.Parse (parts.[0]), parts.[1])

let parseLine (line : string) =
    let parts = line.Split([| " => " |], StringSplitOptions.RemoveEmptyEntries)

    {
        Input = parts.[0].Split ',' |> List.ofArray |> List.map parseMaterial
        Output = parts.[1] |> parseMaterial
    }

let reactions = File.ReadAllLines "FSharp/14-stoichiometry-input.txt"
                |> List.ofArray
                |> List.map parseLine

let cnt = reactions |> List.length
let outputCnt = reactions |> List.map (fun r -> snd r.Output) |> List.distinct |> List.length

let calcTotalNeeds reactions (surplus : Map<string,int64>) (amount, material)  =
    let rec calcTotalNeeds reactions (amount, material) (surplus : Map<string,int64>) =
        // printfn "calcTotalNeeds (%d, %s)" amount material
        let addMaterial reactions surplus ((inputAmount : int64), inputMaterial) ((outputAmount : int64), outputMaterial) neededAmount =
            printfn "Output Material is %s" outputMaterial
            // printfn "addMaterial (%d, %s) => (%d, %s), %d" inputAmount inputMaterial outputAmount outputMaterial neededAmount
            let surplusAvailable = (Map.tryFind outputMaterial surplus |> Option.defaultValue 0L)
            let neededWithoutSurplus = max (neededAmount - surplusAvailable) 0L
            let inputNeeded = (Math.Ceiling(((float neededWithoutSurplus) / (float outputAmount))) |> int64) * inputAmount
            let producedAmount = (inputNeeded / inputAmount) * outputAmount
            let surplusAmount = if neededWithoutSurplus > 0L
                                then producedAmount - neededWithoutSurplus 
                                else surplusAvailable - neededAmount
            let newSurplus = Map.add outputMaterial surplusAmount surplus
            // printfn "params. surplusAvailable: %d, neededWithoutSurplus: %d, inputNeeded: %d, producedAmount: %d, surplusAmount: %d" surplusAvailable neededWithoutSurplus inputNeeded producedAmount surplusAmount
            calcTotalNeeds reactions (inputNeeded, inputMaterial) newSurplus

        match material with
        | "ORE" -> amount, surplus
        | material -> let { Input = input; Output = (outputAmount, _) } = List.find (fun { Output = (_, m) } -> m = material ) reactions
                      input
                      |> List.fold (fun (total, surplus) (a, m) ->
                             let totalNeeded, newSurplus = addMaterial reactions surplus (a, m) (outputAmount, material) amount
                             (total + totalNeeded, newSurplus)) (0L, surplus)

    calcTotalNeeds reactions (amount, material) Map.empty<string, int64>
                        

// let addNeed needs (amount, material) =
//     Map.add material (match Map.tryFind material needs with
//                       | None -> amount
//                       | Some a -> amount + a) needs

// let calcTotalNeeds reactions (amount, material) =
//     let rec calcTotalNeeds reactions (amount, material) totalNeeds =
//         match material with
//         | "ORE" -> addNeed totalNeeds (amount, "ORE")
//         | material -> let { Input = input; Output = (outputAmount, _) } = List.find (fun { Output = (_, m) } -> m = material ) reactions
//                       let totalNeeds = List.fold addNeed totalNeeds
                      
//                     //   (input |> List.sumBy (calcOre reactions)) * (Math.Ceiling ((float amount) / (float outputAmount)) |> int64)
//     calcTotalNeeds reactions (amount, material) Map.empty<string, int>

let result1 = calcTotalNeeds reactions Map.empty<string, int64> (1L, "FUEL") 



