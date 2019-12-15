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

let numbers = File.ReadAllText("FSharp/11-space-police-input.txt").Split(',')
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

type Color = Black | White

type Direction = Up | Right | Down | Left

type Step = Paint | TurnAndMove

let move dir (x, y) = match dir with
                      | Up -> x, y - 1
                      | Right -> x + 1, y
                      | Down -> x, y + 1
                      | Left -> x - 1, y

let color (x, y) hull = match Map.tryFind (x, y) hull with
                        | None -> Black
                        | Some c -> c

let turnLeft dir = match dir with
                   | Up -> Left
                   | Right -> Up
                   | Down -> Right
                   | Left -> Down
 
let turnRight dir = match dir with
                    | Up -> Right
                    | Right -> Down
                    | Down -> Left
                    | Left -> Up

let rec processCode (numbers : Map<int64, int64>) relativeBase hull position direction painted step index =
    // printfn "processCode, index: %d, position: %A, direction: %A, inst: %A, painted: %A" index position direction (instruction numbers index) painted
    match instruction numbers index with
    | Addition (p1, p2, p3) -> let result = (getValue numbers relativeBase p1) + (getValue numbers relativeBase p2)
                               processCode (saveValue numbers relativeBase p3 result) relativeBase hull position direction painted step (index + 4L)
    | Multiplication (p1, p2, p3) -> let result = (getValue numbers relativeBase p1) * (getValue numbers relativeBase p2)
                                     processCode (saveValue numbers relativeBase p3 result) relativeBase hull position direction painted step (index + 4L)
    | Input p -> let input = match color position hull with
                             | Black -> 0L
                             | White -> 1L
                 processCode (saveValue numbers relativeBase p input) relativeBase hull position direction painted step (index + 2L)
    | Output p -> match step with
                  | Paint -> let colorToPaint = match getValue numbers relativeBase p with
                                                | 0L -> Black
                                                | 1L -> White
                                                | _ -> failwith "Invalid input for color to paint"
                             processCode numbers relativeBase (Map.add position colorToPaint hull) position direction (Set.add position painted) TurnAndMove (index + 2L)
                  | TurnAndMove -> let newDir = match getValue numbers relativeBase p with
                                                | 0L -> turnLeft direction
                                                | 1L -> turnRight direction
                                                | _ -> failwith "Invalid input for turn direction"
                                   processCode numbers relativeBase hull (move newDir position) newDir painted Paint (index + 2L)
    | JumpIfTrue (p1, p2) -> let newIndex = if (getValue numbers relativeBase p1) <> 0L
                                            then getValue numbers relativeBase p2
                                            else index + 3L
                             processCode numbers relativeBase hull position direction painted step newIndex
    | JumpIfFalse (p1, p2) -> let newIndex = if (getValue numbers relativeBase p1) = 0L
                                             then getValue numbers relativeBase p2
                                             else index + 3L
                              processCode numbers relativeBase hull position direction painted step newIndex
    | LessThan (p1, p2, p3) -> let result = if (getValue numbers relativeBase p1) < (getValue numbers relativeBase p2) then 1L else 0L
                               processCode (saveValue numbers relativeBase p3 result) relativeBase hull position direction painted step (index + 4L)
    | Equals (p1, p2, p3) -> let result = if (getValue numbers relativeBase p1) = (getValue numbers relativeBase p2) then 1L else 0L
                             processCode (saveValue numbers relativeBase p3 result) relativeBase hull position direction painted step (index + 4L)
    | AdjustRelativeBase p -> let relativeBase = relativeBase + (getValue numbers relativeBase p)
                              processCode numbers relativeBase hull position direction painted step (index + 2L)
    | Halt -> painted |> Set.count, hull

let result1, _ = processCode numbers 0L Map.empty<int*int, Color> (0, 0) Up Set.empty<int * int> Paint 0L

let result2, resultHull = processCode numbers 0L (Map.empty<int*int, Color> |> Map.add (0, 0) White) (0, 0) Up Set.empty<int * int> Paint 0L

let xMin = resultHull
           |> Map.toList
           |> List.map (fun ((x, _), _) -> x)
           |> List.min

let xMax = resultHull
           |> Map.toList
           |> List.map (fun ((x, _), _) -> x)
           |> List.max

let yMin = resultHull
           |> Map.toList
           |> List.map (fun ((_, y), _) -> y)
           |> List.min

let yMax = resultHull
           |> Map.toList
           |> List.map (fun ((_, y), _) -> y)
           |> List.max

for y in yMin..yMax do
    for x in xMin..xMax do
        printf "%c" (match resultHull |> Map.tryFind (x, y) with | Some White -> '#' | _ -> '.') 
    printfn ""