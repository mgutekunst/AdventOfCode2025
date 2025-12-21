type Sign =
    | R
    | L

type Instruction = { Sign: Sign; Rotations: int }
type Position = { Position: int; ZeroPositions: int }

let toInstruction (input: string) =
    let sign =
        match input.[0] with
        | 'R' -> R
        | 'L' -> L
        | _ -> failwith "Invalid sign"

    let number = input.[1..] |> int

    { Sign = sign; Rotations = number }


let calc (instruction: Instruction) =
    match instruction.Sign with
    | L -> -1 * instruction.Rotations
    | R -> instruction.Rotations

let adjustPosition (adjustment: int) (position: Position) : Position =
    let newPosition =
        match (position.Position + adjustment) % 100 with
        | p when p < 0 -> 100 - abs p
        | p -> p

    let newZeros = position.ZeroPositions + if newPosition = 0 then 1 else 0

    { Position = newPosition
      ZeroPositions = newZeros }



let inputFile = "input.txt"
// let inputFile = "demoInput.txt"
let inputLines = System.IO.File.ReadLines inputFile

let instructions = inputLines |> Seq.map toInstruction

let startingPosition = { Position = 50; ZeroPositions = 0 }

let endPosition =
    instructions
    |> Seq.map calc
    |> Seq.fold (fun pos instr -> adjustPosition instr pos) startingPosition

printfn "End Position: %A" endPosition
