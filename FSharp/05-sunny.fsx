open System
open System.IO

type Parameter =
    | Position of int
    | Immediate of int

type Instruction = 
    | Addition of Parameter * Parameter * Parameter
    | Multiplication of Parameter * Parameter * Parameter
    | Input of Parameter
    | Output of Parameter
    | JumpIfTrue of Parameter * Parameter
    | JumpIfFalse of Parameter * Parameter
    | LessThan of Parameter * Parameter * Parameter
    | Equals of Parameter * Parameter * Parameter
    | Halt

let arrayWith index value array =
    let result = Array.copy array
    Array.set result index value
    result

let numbers = File.ReadAllText("FSharp/05-sunny-input.txt").Split(',')
              |> Array.map Int32.Parse

let modes number = ((number / 100) % 10), ((number / 1000) % 10), ((number / 10000) % 10)

let parameter mode value = match mode with
                           | 0 -> Position value
                           | 1 -> Immediate value
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
    | 99 -> Halt
    | _ -> failwith "Invalid input for the opcode"

let getValue (numbers : int array) parameter =
    match parameter with
    | Position p -> numbers.[p]
    | Immediate v -> v

let saveValue (numbers : int array) position value =
    match position with
    | Position p -> arrayWith p value numbers
    | Immediate _ -> failwith "Invalid input, trying to save at immediate value"

let rec processCode inputValue (numbers : int array) index lastOutput =
    match instruction numbers index with
    | Addition (p1, p2, p3) -> let result = (getValue numbers p1) + (getValue numbers p2)
                               processCode inputValue (saveValue numbers p3 result) (index + 4) lastOutput
    | Multiplication (p1, p2, p3) -> let result = (getValue numbers p1) * (getValue numbers p2)
                                     processCode inputValue (saveValue numbers p3 result) (index + 4) lastOutput
    | Input p -> processCode inputValue (saveValue numbers p inputValue) (index + 2) lastOutput
    | Output p -> processCode inputValue numbers (index + 2) (getValue numbers p)
    | JumpIfTrue (p1, p2) -> let newIndex = if (getValue numbers p1) <> 0
                                            then (getValue numbers p2)
                                            else (index + 3)
                             processCode inputValue numbers newIndex lastOutput
    | JumpIfFalse (p1, p2) -> let newIndex = if (getValue numbers p1) = 0
                                             then (getValue numbers p2)
                                             else (index + 3)
                              processCode inputValue numbers newIndex lastOutput
    | LessThan (p1, p2, p3) -> let result = if (getValue numbers p1) < (getValue numbers p2) then 1 else 0
                               processCode inputValue (saveValue numbers p3 result) (index + 4) lastOutput
    | Equals (p1, p2, p3) -> let result = if (getValue numbers p1) = (getValue numbers p2) then 1 else 0
                             processCode inputValue (saveValue numbers p3 result) (index + 4) lastOutput
    | Halt -> lastOutput

let result1 = processCode 1 numbers 0 0

let result2 = processCode 5 numbers 0 0