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

let numbers = File.ReadAllText("FSharp/07-amplifiers-input.txt").Split(',')
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

let rec processCode phase input (numbers : int array) index lastOutput =
    match instruction numbers index with
    | Addition (p1, p2, p3) -> let result = (getValue numbers p1) + (getValue numbers p2)
                               processCode phase input (saveValue numbers p3 result) (index + 4) lastOutput
    | Multiplication (p1, p2, p3) -> let result = (getValue numbers p1) * (getValue numbers p2)
                                     processCode phase input (saveValue numbers p3 result) (index + 4) lastOutput
    | Input p -> let input, newInput = match (phase, input) with
                                       | Some phase, _ -> phase, input
                                       | None, Some input -> input, None
                                       | None, None -> failwith "Input read twice"
                 processCode None newInput (saveValue numbers p input) (index + 2) lastOutput
    | Output p -> match input with
                  | Some _ -> processCode phase input numbers (index + 2) (getValue numbers p)
                  | None -> (getValue numbers p)
    | JumpIfTrue (p1, p2) -> let newIndex = if (getValue numbers p1) <> 0
                                            then (getValue numbers p2)
                                            else (index + 3)
                             processCode phase input numbers newIndex lastOutput
    | JumpIfFalse (p1, p2) -> let newIndex = if (getValue numbers p1) = 0
                                             then (getValue numbers p2)
                                             else (index + 3)
                              processCode phase input numbers newIndex lastOutput
    | LessThan (p1, p2, p3) -> let result = if (getValue numbers p1) < (getValue numbers p2) then 1 else 0
                               processCode phase input (saveValue numbers p3 result) (index + 4) lastOutput
    | Equals (p1, p2, p3) -> let result = if (getValue numbers p1) = (getValue numbers p2) then 1 else 0
                             processCode phase input (saveValue numbers p3 result) (index + 4) lastOutput
    | Halt -> lastOutput

let rec distribute e list = match list with
                            | [] -> [[e]]
                            | h::t -> (e::list) :: [for d in distribute e t -> h :: d]

let rec permute list = match list with
                       | [] -> [[]]
                       | h::t -> List.collect (distribute h) (permute t) 

let processWithPhases (phases : int list) =
    let amp1 = processCode (Some phases.[0]) (Some 0) numbers 0 0
    let amp2 = processCode (Some phases.[1]) (Some amp1) numbers 0 0
    let amp3 = processCode (Some phases.[2]) (Some amp2) numbers 0 0
    let amp4 = processCode (Some phases.[3]) (Some amp3) numbers 0 0
    let amp5 = processCode (Some phases.[4]) (Some amp4) numbers 0 0
    amp5

let result1 = [ 0..4 ]
              |> permute
              |> List.map processWithPhases
              |> List.max

// let rec processWithFeedbackLoop (numberLists : int list list) (indexes : int list) (inputs : int list) = 
