open System
open System.IO

type Parameter =
| Position of int64
| Immediate of int64
| Relative of int64

type Instruction = 
| Addition of Parameter * Parameter * Parameter
| Multiplication of Parameter * Parameter * Parameter
| Input of Parameter
| Output of Parameter
| JumpIfTrue of Parameter * Parameter
| JumpIfFalse of Parameter * Parameter
| LessThan of Parameter * Parameter * Parameter
| Equals of Parameter * Parameter * Parameter
| AdjustRelativeBase of Parameter
| Halt

let numbers = File.ReadAllText("17-flare-input.txt").Split(',')
              |> Array.mapi (fun i n -> i |> int64, n |> Int64.Parse)
              |> Array.fold (fun m (i, n) -> Map.add i n m) Map.empty<int64, int64>

let modes number = ((number / 100L) % 10L), ((number / 1000L) % 10L), ((number / 10000L) % 10L)

let parameter mode (value : int64) =
    match mode with
    | 0L -> Position value
    | 1L -> Immediate value
    | 2L -> Relative value
    | _ -> failwith "Invalid input for parameter"

let instruction (numbers : Map<int64, int64>) index =
    let first = numbers.[index]
    let opcode = first % 100L
    let mode1, mode2, mode3 =  first |> modes
    match opcode with
    | 1L -> Addition ((parameter mode1 (numbers.[index+1L])), (parameter mode2 (numbers.[index+2L])), (parameter mode3 (numbers.[index+3L])))
    | 2L -> Multiplication ((parameter mode1 (numbers.[index+1L])), (parameter mode2 (numbers.[index+2L])), (parameter mode3 (numbers.[index+3L])))
    | 3L -> Input (parameter mode1 (numbers.[index+1L]))
    | 4L -> Output (parameter mode1 (numbers.[index+1L]))
    | 5L -> JumpIfTrue ((parameter mode1 (numbers.[index+1L])), (parameter mode2 (numbers.[index+2L])))
    | 6L -> JumpIfFalse ((parameter mode1 (numbers.[index+1L])), (parameter mode2 (numbers.[index+2L])))
    | 7L -> LessThan ((parameter mode1 (numbers.[index+1L])), (parameter mode2 (numbers.[index+2L])), (parameter mode3 (numbers.[index+3L])))
    | 8L -> Equals ((parameter mode1 (numbers.[index+1L])), (parameter mode2 (numbers.[index+2L])), (parameter mode3 (numbers.[index+3L])))
    | 9L -> AdjustRelativeBase (parameter mode1 (numbers.[index+1L]))
    | 99L -> Halt
    | _ -> failwith "Invalid input for the opcode"

let getValue numbers relativeBase parameter =
    match parameter with
    | Position p -> Map.tryFind p numbers |> Option.defaultValue 0L
    | Immediate v -> v |> int64
    | Relative p -> Map.tryFind (relativeBase + p) numbers |> Option.defaultValue 0L

let saveValue numbers relativeBase position value =
    match position with
    | Position p -> Map.add p value numbers
    | Immediate _ -> failwith "Invalid input, trying to save at immediate value"
    | Relative p -> Map.add (relativeBase + p) value numbers


let print view =
    if Map.count view = 0
    then ()
    else let xMin = view
                    |> Map.toList
                    |> List.map (fun ((x, _), _) -> x)
                    |> List.min

         let xMax = view
                    |> Map.toList
                    |> List.map (fun ((x, _), _) -> x)
                    |> List.max

         let yMin = view
                    |> Map.toList
                    |> List.map (fun ((_, y), _) -> y)
                    |> List.min

         let yMax = view
                    |> Map.toList
                    |> List.map (fun ((_, y), _) -> y)
                    |> List.max

         Console.Clear ()
         Console.SetCursorPosition(0, 0)
         for y in yMin..yMax do
             for x in xMin..xMax do
                 printf "%c" (view |> Map.tryFind (x, y) |> Option.defaultValue ' ') 
             printfn ""

let rec processCode (numbers : Map<int64, int64>) relativeBase input view (x, y) index =
    match instruction numbers index with
    | Addition (p1, p2, p3) -> let result = (getValue numbers relativeBase p1) + (getValue numbers relativeBase p2)
                               processCode (saveValue numbers relativeBase p3 result) relativeBase input view (x, y) (index + 4L)
    | Multiplication (p1, p2, p3) -> let result = (getValue numbers relativeBase p1) * (getValue numbers relativeBase p2)
                                     processCode (saveValue numbers relativeBase p3 result) relativeBase input view (x, y) (index + 4L)
    | Input p -> match input with
                 | [] -> failwith "Ran out of input"
                 | c :: cs -> processCode (saveValue numbers relativeBase p (int64 c)) relativeBase cs view (x, y) (index + 2L)
    | Output p -> let outputValue = getValue numbers relativeBase p
                  if outputValue < 256L
                  then let newView, newPos = match outputValue with
                                             | 10L -> //print view
                                                      view, (0, y + 1)
                                             | v -> (Map.add (x, y) (char v) view), (x + 1, y)
                       processCode numbers relativeBase input newView newPos (index + 2L)
                  else view, outputValue
    | JumpIfTrue (p1, p2) -> let newIndex = if (getValue numbers relativeBase p1) <> 0L
                                            then getValue numbers relativeBase p2
                                            else index + 3L
                             processCode numbers relativeBase input view (x, y) newIndex
    | JumpIfFalse (p1, p2) -> let newIndex = if (getValue numbers relativeBase p1) = 0L
                                             then getValue numbers relativeBase p2
                                             else index + 3L
                              processCode numbers relativeBase input view (x, y) newIndex
    | LessThan (p1, p2, p3) -> let result = if (getValue numbers relativeBase p1) < (getValue numbers relativeBase p2) then 1L else 0L
                               processCode (saveValue numbers relativeBase p3 result) relativeBase input view (x, y) (index + 4L)
    | Equals (p1, p2, p3) -> let result = if (getValue numbers relativeBase p1) = (getValue numbers relativeBase p2) then 1L else 0L
                             processCode (saveValue numbers relativeBase p3 result) relativeBase input view (x, y) (index + 4L)
    | AdjustRelativeBase p -> let relativeBase = relativeBase + (getValue numbers relativeBase p)
                              processCode numbers relativeBase input view (x, y) (index + 2L)
    | Halt -> view, -1L

let sumAlignment (view : Map<(int * int), char>) =
    let xMax = view
               |> Map.toList
               |> List.map (fun ((x, _), _) -> x)
               |> List.max
    let yMax = view
               |> Map.toList
               |> List.map (fun ((_, y), _) -> y)
               |> List.max

    [ for y in 1..(yMax - 1) -> y ]
    |> List.collect (fun y -> [ for x in 1..(xMax - 1) -> x ] |> List.map (fun x -> (x, y)))
    |> List.sumBy (fun (x, y) ->
        match Map.find (x, y) view, Map.find (x-1, y) view, Map.find (x, y-1) view, Map.find (x+1, y) view, Map.find (x, y+1) view with
        | '#', '#', '#', '#', '#' -> x * y
        | _ -> 0)

[<EntryPoint>]
let main argv =
    // Part 1
    let view, output = processCode numbers 0L ("" |> List.ofSeq) Map.empty<int*int, char> (0, 0) 0L
    print view
    printfn "The total alignment value: %d" (view |> sumAlignment)

// A: L,6,R,8,L,4,R,8,L,12
// B: L,12,R,10,L,4
// C: L,12,L,6,L,4,L,4

// A,B,B,C,B,C,B,C,A,A
    let fullInput = "A,B,B,C,B,C,B,C,A,A\nL,6,R,8,L,4,R,8,L,12\nL,12,R,10,L,4\nL,12,L,6,L,4,L,4\nn\n"
    let view2, output2 = processCode (Map.add 0L 2L numbers) 0L (fullInput |> List.ofSeq) Map.empty<int*int, char> (0, 0) 0L
    printfn "The collected dust amount: %d" output2
    0
