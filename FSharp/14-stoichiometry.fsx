open System
open System.IO

type Material = int * string

type Reaction = {
    Input : Material list
    Output : Material
}

let parseMaterial (str : string) =
    let parts = str.Trim().Split ' '
    (Int32.Parse (parts.[0]), parts.[1])

let parseLine (line : string) =
    let parts = line.Split([| " => " |], StringSplitOptions.RemoveEmptyEntries)
    {
        Input = parts.[0].Split ',' |> List.ofArray |> List.map parseMaterial
        Output = parts.[1] |> parseMaterial
    }

let reactions = File.ReadAllLines "FSharp/14-stoichiometry-input.txt"
                |> List.ofArray
                |> List.map parseLine

let rec calcOre reactions surplus (amount, material) =
    match material with
    | "ORE" -> match surplus |> Map.tryFind "ORE" with
               | Some oreSurplus -> max (amount - oreSurplus) 0, Map.add "ORE" (max (oreSurplus - amount) 0) surplus
               | None -> amount, surplus
    | material -> let { Input = input; Output = (outputAmount, _) } = List.find (fun { Output = (_, m) } -> m = material ) reactions
                  let surplusAmount = surplus |> Map.tryFind material |> Option.defaultValue 0
                  let amountWithoutSurplus = max (amount - surplusAmount) 0
                  let reactionCount = Math.Ceiling ((float amountWithoutSurplus) / (float outputAmount)) |> int
                  let producedAmount = reactionCount * outputAmount
                  let surplus = Map.add material (surplusAmount - (amount - amountWithoutSurplus) + (producedAmount - amountWithoutSurplus)) surplus
                  input
                  |> List.fold (fun (total, surplus) (a, m) ->
                      let totalNeeded, newSurplus = calcOre reactions surplus (a*reactionCount, m)
                      (total + totalNeeded, newSurplus)) (0, surplus)

let result1 = calcOre reactions Map.empty<string, int> (1, "FUEL")