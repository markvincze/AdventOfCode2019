open System
open System.IO

type Parameter =
    | Position of int
    | Immediate of int
    | Relative of int

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

let numbers = File.ReadAllText("FSharp/09-sensor-input.txt").Split(',')
              |> Array.mapi (fun i n -> i, n |> Int64.Parse)
              |> Array.fold (fun m (i, n) -> Map.add i n m) Map.empty<int, int64>

let modes number = ((number / 100L) % 10L), ((number / 1000L) % 10L), ((number / 10000L) % 10L)

let parameter mode (value : int64) = match mode with
                           | 0L -> Position (int value)
                           | 1L -> Immediate (int value)
                           | 2L -> Relative (int value)
                           | _ -> failwith "Invalid input for parameter"

let instruction (numbers : Map<int, int64>) index =
    let first = numbers.[index]
    let opcode = first % 100L
    let mode1, mode2, mode3 =  first |> modes
    match opcode with
    | 1L -> Addition ((parameter mode1 (numbers.[index+1])), (parameter mode2 (numbers.[index+2])), (parameter mode3 (numbers.[index+3])))
    | 2L -> Multiplication ((parameter mode1 (numbers.[index+1])), (parameter mode2 (numbers.[index+2])), (parameter mode3 (numbers.[index+3])))
    | 3L -> Input (parameter mode1 (numbers.[index+1]))
    | 4L -> Output (parameter mode1 (numbers.[index+1]))
    | 5L -> JumpIfTrue ((parameter mode1 (numbers.[index+1])), (parameter mode2 (numbers.[index+2])))
    | 6L -> JumpIfFalse ((parameter mode1 (numbers.[index+1])), (parameter mode2 (numbers.[index+2])))
    | 7L -> LessThan ((parameter mode1 (numbers.[index+1])), (parameter mode2 (numbers.[index+2])), (parameter mode3 (numbers.[index+3])))
    | 8L -> Equals ((parameter mode1 (numbers.[index+1])), (parameter mode2 (numbers.[index+2])), (parameter mode3 (numbers.[index+3])))
    | 9L -> AdjustRelativeBase (parameter mode1 (numbers.[index+1]))
    | 99L -> Halt
    | _ -> failwith "Invalid input for the opcode"

let getValue (numbers : Map<int, int64>) relativeBase parameter =
    match parameter with
    | Position p -> Map.tryFind p numbers |> Option.defaultValue 0L
    | Immediate v -> v |> int64
    | Relative p -> Map.tryFind (relativeBase + p) numbers |> Option.defaultValue 0L

let saveValue (numbers : Map<int, int64>) relativeBase position value =
    match position with
    | Position p -> Map.add p value numbers
    | Immediate _ -> failwith "Invalid input, trying to save at immediate value"
    | Relative p -> Map.add (relativeBase + p) value numbers

type ReturnType = InputReadTwice | OutputReturned | Halted

let rec processCode input (numbers : Map<int, int64>) relativeBase index outputs =
    match instruction numbers index with
    | Addition (p1, p2, p3) -> let result = (getValue numbers relativeBase p1) + (getValue numbers relativeBase p2)
                               processCode input (saveValue numbers relativeBase p3 result) relativeBase (index + 4) outputs
    | Multiplication (p1, p2, p3) -> let result = (getValue numbers relativeBase p1) * (getValue numbers relativeBase p2)
                                     processCode input (saveValue numbers relativeBase p3 result) relativeBase (index + 4) outputs
    | Input p -> match input with
                 | Some input -> processCode None (saveValue numbers relativeBase p input) relativeBase (index + 2) outputs
                 | None -> failwith "Read input twice"
    | Output p -> processCode input numbers relativeBase (index + 2) ((getValue numbers relativeBase p) :: outputs)
    | JumpIfTrue (p1, p2) -> let newIndex = if (getValue numbers relativeBase p1) <> 0L
                                            then (getValue numbers relativeBase p2) |> int
                                            else (index + 3)
                             processCode input numbers relativeBase (int newIndex) outputs
    | JumpIfFalse (p1, p2) -> let newIndex = if (getValue numbers relativeBase p1) = 0L
                                             then (getValue numbers relativeBase p2) |> int
                                             else (index + 3)
                              processCode input numbers relativeBase newIndex outputs
    | LessThan (p1, p2, p3) -> let result = if (getValue numbers relativeBase p1) < (getValue numbers relativeBase p2) then 1L else 0L
                               processCode input (saveValue numbers relativeBase p3 result) relativeBase (index + 4) outputs
    | Equals (p1, p2, p3) -> let result = if (getValue numbers relativeBase p1) = (getValue numbers relativeBase p2) then 1L else 0L
                             processCode input (saveValue numbers relativeBase p3 result) relativeBase (index + 4) outputs
    | AdjustRelativeBase p -> let relativeBase = relativeBase + (getValue numbers relativeBase p |> int)
                              processCode input numbers relativeBase (index + 2) outputs
    | Halt -> (outputs |> List.rev), numbers, (index + 2), Halted

let outputs, _, _, _ = processCode (Some 1L) numbers 0 0 []

let outputs2, _, _, _ = processCode (Some 2L) numbers 0 0 []
