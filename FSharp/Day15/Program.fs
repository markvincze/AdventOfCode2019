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

let numbers = File.ReadAllText("15-oxygen-input.txt").Split(',')
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


type Tile = Empty | Wall | Oxygen

let print board droidPos =
    if Map.count board = 0
    then ()
    else let xMin = board
                    |> Map.toList
                    |> List.map (fun ((x, _), _) -> x)
                    |> List.min

         let xMax = board
                    |> Map.toList
                    |> List.map (fun ((x, _), _) -> x)
                    |> List.max

         let yMin = board
                    |> Map.toList
                    |> List.map (fun ((_, y), _) -> y)
                    |> List.min

         let yMax = board
                    |> Map.toList
                    |> List.map (fun ((_, y), _) -> y)
                    |> List.max

         Console.Clear ()
         Console.SetCursorPosition(0, 0)
         for y in yMin..yMax do
             for x in xMin..xMax do
                 let c = if droidPos = (x, y)
                         then 'D'
                         else match board |> Map.tryFind (x, y) with
                              | None -> ' '
                              | Some Empty -> '.'
                              | Some Wall -> '#'
                              | Some Oxygen -> 'X'
                 printf "%c" c 
             printfn ""

type MoveState =
| NoInput
| North
| East
| South
| West

let posFrom moveState (x, y) = match moveState with
                               | NoInput -> failwith "NoInput posFrom"
                               | North -> (x, y - 1)
                               | East -> (x + 1, y)
                               | South -> (x, y + 1)
                               | West -> (x - 1, y)

let rec processCode (numbers : Map<int64, int64>) relativeBase board droidPos moveState index =
    match instruction numbers index with
    | Addition (p1, p2, p3) -> let result = (getValue numbers relativeBase p1) + (getValue numbers relativeBase p2)
                               processCode (saveValue numbers relativeBase p3 result) relativeBase board droidPos moveState (index + 4L)
    | Multiplication (p1, p2, p3) -> let result = (getValue numbers relativeBase p1) * (getValue numbers relativeBase p2)
                                     processCode (saveValue numbers relativeBase p3 result) relativeBase board droidPos moveState (index + 4L)
    | Input p -> print board droidPos
                 let input, moveState = match (Console.ReadKey ()).Key with
                                        | ConsoleKey.UpArrow -> 1L, North
                                        | ConsoleKey.DownArrow -> 2L, South
                                        | ConsoleKey.LeftArrow -> 3L, West
                                        | ConsoleKey.RightArrow -> 4L, East
                                        | _ -> 1L, North
                                        // | _ -> failwith "Incorrect input"
                 processCode (saveValue numbers relativeBase p input) relativeBase board droidPos moveState (index + 2L)
    | Output p -> let outputValue = getValue numbers relativeBase p
                  match outputValue with
                  | 0L -> processCode numbers relativeBase (Map.add (posFrom moveState droidPos) Wall board) droidPos NoInput (index + 2L)
                  | 1L -> processCode numbers relativeBase (Map.add (posFrom moveState droidPos) Empty board) (posFrom moveState droidPos) NoInput (index + 2L)
                  | 2L -> processCode numbers relativeBase (Map.add (posFrom moveState droidPos) Oxygen board) (posFrom moveState droidPos) NoInput (index + 2L)
                  | _ -> failwith "Invalid output"
    | JumpIfTrue (p1, p2) -> let newIndex = if (getValue numbers relativeBase p1) <> 0L
                                            then getValue numbers relativeBase p2
                                            else index + 3L
                             processCode numbers relativeBase board droidPos moveState newIndex
    | JumpIfFalse (p1, p2) -> let newIndex = if (getValue numbers relativeBase p1) = 0L
                                             then getValue numbers relativeBase p2
                                             else index + 3L
                              processCode numbers relativeBase board droidPos moveState newIndex
    | LessThan (p1, p2, p3) -> let result = if (getValue numbers relativeBase p1) < (getValue numbers relativeBase p2) then 1L else 0L
                               processCode (saveValue numbers relativeBase p3 result) relativeBase board droidPos moveState (index + 4L)
    | Equals (p1, p2, p3) -> let result = if (getValue numbers relativeBase p1) = (getValue numbers relativeBase p2) then 1L else 0L
                             processCode (saveValue numbers relativeBase p3 result) relativeBase board droidPos moveState (index + 4L)
    | AdjustRelativeBase p -> let relativeBase = relativeBase + (getValue numbers relativeBase p)
                              processCode numbers relativeBase board droidPos moveState (index + 2L)
    | Halt -> board

[<EntryPoint>]
let main argv =
    let finalBoard = processCode (numbers) 0L Map.empty<int*int, Tile> (0, 0) NoInput 0L
    printfn "Hello World from F#!"
    0
