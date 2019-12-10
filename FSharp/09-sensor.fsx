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

let arrayWith index value array =
    let result = Array.copy array
    Array.set result index value
    result

let numbers = File.ReadAllText("FSharp/09-sensor-input.txt").Split(',')
              |> Array.map Int32.Parse

let modes number = ((number / 100) % 10), ((number / 1000) % 10), ((number / 10000) % 10)

let parameter mode value = match mode with
                           | 0 -> Position value
                           | 1 -> Immediate value
                           | 2 -> Relative value
                           | _ -> failwith "Invalid input for parameter"

let instruction (numbers : int array) index =
    let first = numbers.[index]
    let opcode = first % 100
    let mode1, mode2, mode3 =  first |> modes
    match opcode with
    | 1 -> Addition ((parameter mode1 (numbers.[index+1])), (parameter mode2 (numbers.[index+2])), (parameter mode3 (numbers.[index+3])))
    | 2 -> Multiplication ((parameter mode1 (numbers.[index+1])), (parameter mode2 (numbers.[index+2])), (parameter mode3 (numbers.[index+3])))
    | 3 -> Input (parameter mode1 (numbers.[index+1]))
    | 4 -> Output (parameter mode1 (numbers.[index+1]))
    | 5 -> JumpIfTrue ((parameter mode1 (numbers.[index+1])), (parameter mode2 (numbers.[index+2])))
    | 6 -> JumpIfFalse ((parameter mode1 (numbers.[index+1])), (parameter mode2 (numbers.[index+2])))
    | 7 -> LessThan ((parameter mode1 (numbers.[index+1])), (parameter mode2 (numbers.[index+2])), (parameter mode3 (numbers.[index+3])))
    | 8 -> Equals ((parameter mode1 (numbers.[index+1])), (parameter mode2 (numbers.[index+2])), (parameter mode3 (numbers.[index+3])))
    | 9 -> AdjustRelativeBase (parameter mode1 (numbers.[index+1]))
    | 99 -> Halt
    | _ -> failwith "Invalid input for the opcode"

let getValue (numbers : int array) relativeBase parameter =
    match parameter with
    | Position p -> numbers.[p]
    | Immediate v -> v
    | Relative p -> numbers.[relativeBase + p]

let saveValue (numbers : int array) relativeBase position value =
    match position with
    | Position p -> arrayWith p value numbers
    | Immediate _ -> failwith "Invalid input, trying to save at immediate value"
    | Relative p -> arrayWith (relativeBase + p) value numbers

type ReturnType = InputReadTwice | OutputReturned | Halted

let rec processCode input (numbers : int array) relativeBase index outputs =
    printfn "ProcessCode called, index: %d, number: %d, relative base: %d" index (numbers.[index]) relativeBase
    match instruction numbers index with
    | Addition (p1, p2, p3) -> let result = (getValue numbers relativeBase p1) + (getValue numbers relativeBase p2)
                               processCode input (saveValue numbers relativeBase p3 result) relativeBase (index + 4) outputs
    | Multiplication (p1, p2, p3) -> let result = (getValue numbers relativeBase p1) * (getValue numbers relativeBase p2)
                                     processCode input (saveValue numbers relativeBase p3 result) relativeBase (index + 4) outputs
    | Input p -> match input with
                 | Some input -> processCode None (saveValue numbers relativeBase p input) relativeBase (index + 2) outputs
                 | None -> failwith "Read input twice"
    | Output p -> //(getValue numbers relativeBase p), numbers, (index + 2), OutputReturned
                  processCode input numbers relativeBase (index + 2) ((getValue numbers relativeBase p) :: outputs)
    | JumpIfTrue (p1, p2) -> let newIndex = if (getValue numbers relativeBase p1) <> 0
                                            then (getValue numbers relativeBase p2)
                                            else (index + 3)
                             processCode input numbers relativeBase newIndex outputs
    | JumpIfFalse (p1, p2) -> let newIndex = if (getValue numbers relativeBase p1) = 0
                                             then (getValue numbers relativeBase p2)
                                             else (index + 3)
                              processCode input numbers relativeBase newIndex outputs
    | LessThan (p1, p2, p3) -> let result = if (getValue numbers relativeBase p1) < (getValue numbers relativeBase p2) then 1 else 0
                               processCode input (saveValue numbers relativeBase p3 result) relativeBase (index + 4) outputs
    | Equals (p1, p2, p3) -> let result = if (getValue numbers relativeBase p1) = (getValue numbers relativeBase p2) then 1 else 0
                             processCode input (saveValue numbers relativeBase p3 result) relativeBase (index + 4) outputs
    | AdjustRelativeBase p -> let relativeBase = relativeBase + (getValue numbers relativeBase p)
                              processCode input numbers relativeBase (index + 2) outputs
    | Halt -> (outputs |> List.rev), numbers, (index + 2), Halted

let outputs, _, _, _ = processCode (Some 1) numbers 0 0 []
