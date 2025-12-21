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


let adjustPosition (adjustment: Instruction) (position: Position) : Position =
    let lOrR =
        match adjustment.Sign with
        | L -> -1
        | R -> 1

    let newPosition =
        [ 1 .. adjustment.Rotations ]
        |> Seq.fold
            (fun p _ ->
                let pos =
                    match (p.Position + lOrR) % 100 with
                    | x when x < 0 -> 100 - abs x
                    | x -> x

                let zeroCount =
                    match pos with
                    | 0 -> p.ZeroPositions + 1
                    | _ -> p.ZeroPositions

                { Position = pos
                  ZeroPositions = zeroCount })
            position

    newPosition



let inputFile = "input.txt"
// let inputFile = "demoInput.txt"
let inputLines = System.IO.File.ReadLines inputFile

let instructions = inputLines |> Seq.map toInstruction

let startingPosition = { Position = 50; ZeroPositions = 0 }

let endPosition =
    instructions
    |> Seq.fold (fun pos instr -> adjustPosition instr pos) startingPosition

printfn "End Position: %A" endPosition
