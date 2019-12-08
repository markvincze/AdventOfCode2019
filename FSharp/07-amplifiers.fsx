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

type ReturnType = InputReadTwice | OutputReturned | Halted

let rec processCodeF input (numbers : int array) index lastOutput =
    match instruction numbers index with
    | Addition (p1, p2, p3) -> let result = (getValue numbers p1) + (getValue numbers p2)
                               processCodeF input (saveValue numbers p3 result) (index + 4) lastOutput
    | Multiplication (p1, p2, p3) -> let result = (getValue numbers p1) * (getValue numbers p2)
                                     processCodeF input (saveValue numbers p3 result) (index + 4) lastOutput
    | Input p -> match input with
                 | Some input -> processCodeF None (saveValue numbers p input) (index + 2) lastOutput
                 | None -> printfn "Read input twice"
                           lastOutput, numbers, index, InputReadTwice
    | Output p -> match input with
                  | Some _ -> printfn "Warning: Output command reached without reading the input"
                              processCodeF input numbers (index + 2) (getValue numbers p)
                  | None -> (getValue numbers p), numbers, (index + 2), OutputReturned
    | JumpIfTrue (p1, p2) -> let newIndex = if (getValue numbers p1) <> 0
                                            then (getValue numbers p2)
                                            else (index + 3)
                             processCodeF input numbers newIndex lastOutput
    | JumpIfFalse (p1, p2) -> let newIndex = if (getValue numbers p1) = 0
                                             then (getValue numbers p2)
                                             else (index + 3)
                              processCodeF input numbers newIndex lastOutput
    | LessThan (p1, p2, p3) -> let result = if (getValue numbers p1) < (getValue numbers p2) then 1 else 0
                               processCodeF input (saveValue numbers p3 result) (index + 4) lastOutput
    | Equals (p1, p2, p3) -> let result = if (getValue numbers p1) = (getValue numbers p2) then 1 else 0
                             processCodeF input (saveValue numbers p3 result) (index + 4) lastOutput
    | Halt -> lastOutput, numbers, (index + 2), Halted

let rec distribute e list = match list with
                            | [] -> [[e]]
                            | h::t -> (e::list) :: [for d in distribute e t -> h :: d]

let rec permute list = match list with
                       | [] -> [[]]
                       | h::t -> List.collect (distribute h) (permute t) 

let processWithPhases (phases : int list) =
    let amp1, numbers1, index1, out1 = processCodeF (Some phases.[0]) numbers 0 0
    let amp2, numbers2, index2, out2 = processCodeF (Some phases.[1]) numbers 0 0
    let amp3, numbers3, index3, out3 = processCodeF (Some phases.[2]) numbers 0 0
    let amp4, numbers4, index4, out4 = processCodeF (Some phases.[3]) numbers 0 0
    let amp5, numbers5, index5, out5 = processCodeF (Some phases.[4]) numbers 0 0
    let out1, _, _, _ = processCodeF (Some 0) numbers1 index1 amp1
    let out2, _, _, _ = processCodeF (Some out1) numbers2 index2 amp2
    let out3, _, _, _ = processCodeF (Some out2) numbers3 index3 amp3
    let out4, _, _, _ = processCodeF (Some out3) numbers4 index4 amp4
    let out5, _, _, _ = processCodeF (Some out4) numbers5 index5 amp5
    out5

let result1 = [ 0..4 ]
              |> permute
              |> List.map processWithPhases
              |> List.max

let t1 (a, _, _) = a
let t2 (_, b, _) = b
let t3 (_, _, c) = c

let rec processWithFeedbackLoop (programStates : ((int array) * int * int) list) input = 
    let out1, numbers1, index1, _ = processCodeF (Some input) (t1 programStates.[0]) (t2 programStates.[0]) (t3 programStates.[0])
    let out2, numbers2, index2, _ = processCodeF (Some out1) (t1 programStates.[1]) (t2 programStates.[1]) (t3 programStates.[1])
    let out3, numbers3, index3, _ = processCodeF (Some out2) (t1 programStates.[2]) (t2 programStates.[2]) (t3 programStates.[2])
    let out4, numbers4, index4, _ = processCodeF (Some out3) (t1 programStates.[3]) (t2 programStates.[3]) (t3 programStates.[3])
    let out5, numbers5, index5, rt = processCodeF (Some out4) (t1 programStates.[4]) (t2 programStates.[4]) (t3 programStates.[4])
    match rt with
    | InputReadTwice -> failwith "Input read twice :("
    | Halted -> out5
    | OutputReturned -> let newProgramStates = [
                            (numbers1, index1, out1)
                            (numbers2, index2, out2)
                            (numbers3, index3, out3)
                            (numbers4, index4, out4)
                            (numbers5, index5, out5)
                        ]
                        processWithFeedbackLoop newProgramStates out5

let processWithPhasesWithFeedbackLoop (phases : int list) =
    let out1, numbers1, index1, _ = processCodeF (Some phases.[0]) numbers 0 0
    let out2, numbers2, index2, _ = processCodeF (Some phases.[1]) numbers 0 0
    let out3, numbers3, index3, _ = processCodeF (Some phases.[2]) numbers 0 0
    let out4, numbers4, index4, _ = processCodeF (Some phases.[3]) numbers 0 0
    let out5, numbers5, index5, _ = processCodeF (Some phases.[4]) numbers 0 0

    let programStates = [ (numbers1, index1, out1); (numbers2, index2, out2); (numbers3, index3, out3); (numbers4, index4, out4); (numbers5, index5, out5); ]
    processWithFeedbackLoop programStates 0

let result2 = [ 5..9 ]
              |> permute
              |> List.map processWithPhasesWithFeedbackLoop
              |> List.max
