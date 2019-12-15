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

let numbers = File.ReadAllText("FSharp/13-carepackage-input.txt").Split(',')
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

type Tile = Empty | Wall | Block | HorizontalPaddle | Ball
type Step = X | Y | Tile
type OutputState = {
    Step : Step
    X : int64
    Y : int64
    Tile : Tile
}

let newOutputState = { Step = X; X = 0L; Y = 0L; Tile = Empty }

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

let rec processCode (numbers : Map<int64, int64>) relativeBase board outputState score index =
    // printfn "processCode, index: %d, position: %A, direction: %A, inst: %A, painted: %A" index position (instruction numbers index) painted
    match instruction numbers index with
    | Addition (p1, p2, p3) -> let result = (getValue numbers relativeBase p1) + (getValue numbers relativeBase p2)
                               processCode (saveValue numbers relativeBase p3 result) relativeBase board outputState score (index + 4L)
    | Multiplication (p1, p2, p3) -> let result = (getValue numbers relativeBase p1) * (getValue numbers relativeBase p2)
                                     processCode (saveValue numbers relativeBase p3 result) relativeBase board outputState score (index + 4L)
    | Input p -> let ballX = Map.findKey (fun _ tile -> tile = Ball) board |> fst
                 let paddleX = Map.findKey (fun _ tile -> tile = HorizontalPaddle) board |> fst
                 let input = if ballX > paddleX then 1L
                             elif ballX < paddleX then -1L
                             else 0L
                 processCode (saveValue numbers relativeBase p input) relativeBase board outputState score (index + 2L)
    | Output p -> let outputValue = getValue numbers relativeBase p
                  match outputState.Step with
                  | X -> processCode numbers relativeBase board { outputState with Step = Y; X = outputValue } score (index + 2L)
                  | Y -> processCode numbers relativeBase board { outputState with Step = Tile; Y = outputValue } score (index + 2L)
                  | Tile -> match outputState.X, outputState.Y with
                            | -1L, 0L -> processCode numbers relativeBase board newOutputState outputValue (index + 2L)
                            | _ -> let tile = match outputValue with
                                              | 0L -> Empty
                                              | 1L -> Wall
                                              | 2L -> Block
                                              | 3L -> HorizontalPaddle
                                              | 4L -> Ball
                                              | _ -> failwith "Invalid input"
                                   let newBoard = Map.add (outputState.X, outputState.Y) tile board
                                   processCode numbers relativeBase newBoard newOutputState score (index + 2L)
    | JumpIfTrue (p1, p2) -> let newIndex = if (getValue numbers relativeBase p1) <> 0L
                                            then getValue numbers relativeBase p2
                                            else index + 3L
                             processCode numbers relativeBase board outputState score newIndex
    | JumpIfFalse (p1, p2) -> let newIndex = if (getValue numbers relativeBase p1) = 0L
                                             then getValue numbers relativeBase p2
                                             else index + 3L
                              processCode numbers relativeBase board outputState score newIndex
    | LessThan (p1, p2, p3) -> let result = if (getValue numbers relativeBase p1) < (getValue numbers relativeBase p2) then 1L else 0L
                               processCode (saveValue numbers relativeBase p3 result) relativeBase board outputState score (index + 4L)
    | Equals (p1, p2, p3) -> let result = if (getValue numbers relativeBase p1) = (getValue numbers relativeBase p2) then 1L else 0L
                             processCode (saveValue numbers relativeBase p3 result) relativeBase board outputState score (index + 4L)
    | AdjustRelativeBase p -> let relativeBase = relativeBase + (getValue numbers relativeBase p)
                              processCode numbers relativeBase board outputState score (index + 2L)
    | Halt -> board, score

let finalBoard, _ = processCode numbers 0L Map.empty<int64*int64, Tile> newOutputState 0L 0L

let result1 = finalBoard |> Seq.filter (fun kvp -> kvp.Value = Block) |> Seq.length

let _, result2 = processCode (numbers |> Map.add 0L 2L) 0L Map.empty<int64*int64, Tile> newOutputState 0L 0L