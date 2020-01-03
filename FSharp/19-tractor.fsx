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

let numbers = File.ReadAllText("FSharp/19-tractor-input.txt").Split(',')
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

let rec processCode (numbers : Map<int64, int64>) relativeBase (input : int list) (output : int64 list) index =
    match instruction numbers index with
    | Addition (p1, p2, p3) -> let result = (getValue numbers relativeBase p1) + (getValue numbers relativeBase p2)
                               processCode (saveValue numbers relativeBase p3 result) relativeBase input output (index + 4L)
    | Multiplication (p1, p2, p3) -> let result = (getValue numbers relativeBase p1) * (getValue numbers relativeBase p2)
                                     processCode (saveValue numbers relativeBase p3 result) relativeBase input output (index + 4L)
    | Input p -> match input with
                 | [] -> failwith "Ran out of input"
                 | i :: rest -> processCode (saveValue numbers relativeBase p (int64 i)) relativeBase rest output (index + 2L)
    | Output p -> let outputValue = getValue numbers relativeBase p
                  processCode numbers relativeBase input (outputValue :: output) (index + 2L)
    | JumpIfTrue (p1, p2) -> let newIndex = if (getValue numbers relativeBase p1) <> 0L
                                            then getValue numbers relativeBase p2
                                            else index + 3L
                             processCode numbers relativeBase input output newIndex
    | JumpIfFalse (p1, p2) -> let newIndex = if (getValue numbers relativeBase p1) = 0L
                                             then getValue numbers relativeBase p2
                                             else index + 3L
                              processCode numbers relativeBase input output newIndex
    | LessThan (p1, p2, p3) -> let result = if (getValue numbers relativeBase p1) < (getValue numbers relativeBase p2) then 1L else 0L
                               processCode (saveValue numbers relativeBase p3 result) relativeBase input output (index + 4L)
    | Equals (p1, p2, p3) -> let result = if (getValue numbers relativeBase p1) = (getValue numbers relativeBase p2) then 1L else 0L
                             processCode (saveValue numbers relativeBase p3 result) relativeBase input output (index + 4L)
    | AdjustRelativeBase p -> let relativeBase = relativeBase + (getValue numbers relativeBase p)
                              processCode numbers relativeBase input output (index + 2L)
    | Halt -> output |> List.head

let size = 50
let input = [ for y in 0..(size - 1) -> y ]
            |> List.collect (fun y -> [ for x in 0..(size - 1) -> x, y ])

let output = input
             |> List.map (fun (x, y) -> processCode numbers 0L [ x; y ] [] 0L)

let result1 = output |> List.sum

let determineHorizontalRange y minX =
    let firstX = (Seq.initInfinite (fun x -> x + minX)
                 |> Seq.find (fun x -> processCode numbers 0L [ x; y ] [] 0L = 1L))

    let lastX = (Seq.initInfinite (fun x -> x + firstX)
                |> Seq.find (fun x -> processCode numbers 0L [ x; y ] [] 0L = 0L))
                - 1
    firstX, lastX

let determineVerticalRange x minY =
    let firstY = (Seq.initInfinite (fun y -> y + minY)
                 |> Seq.find (fun y -> processCode numbers 0L [ x; y ] [] 0L = 1L))

    let lastY = (Seq.initInfinite (fun y -> y + firstY)
                |> Seq.find (fun y -> processCode numbers 0L [ x; y ] [] 0L = 0L))
                - 1
    firstY, lastY

let rec findCorner y minX lastMinY =
    let x1, x2 = determineHorizontalRange y minX
    if x2 - x1 <= 100
    then findCorner (y + 1) x1 lastMinY
    else let cornerX = x2 - 100 + 1
         let y1, y2 = determineVerticalRange cornerX lastMinY
         if y2 - y >= 99
         then cornerX, y
         else findCorner (y + 1) x1 y1

let corner = findCorner 5 0 0
let xr, yr = corner
let result2 = xr * 10000 + yr
