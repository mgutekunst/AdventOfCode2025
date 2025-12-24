open System
open System.IO

let add (a: int64) (b: int64) = a + b
let multiply a b = a * b

let getAggregationOperation =
    function
    | "+" -> add
    | "*" -> multiply
    | op -> failwithf "unknown operator %s" op

let aggregateLine =
    Array.rev
    >> fun v -> getAggregationOperation (v |> Array.head), Array.tail v
    >> fun (operation, list) -> list |> Array.map int64 |> Array.reduce operation

let aggregate (g: string[,]) =
    seq {
        for i in [ 0 .. Array2D.length1 g - 1 ] do
            yield g[i, *] |> aggregateLine
    }


let toNumberString (l: char[]) =
    if Array.forall ((=) ' ') l then None else Some(String l)

let toNumberStrings (g: char[,]) =
    [ for i in Array2D.length2 g - 1 .. -1 .. 0 -> g[*, i] |> toNumberString ]

type Calculation =
    { Values: int64 list
      Operation: int64 -> int64 -> int64 }

let toCalculationGroups (input: seq<option<string>>) =
    let folder (groups, current) item =
        match item with
        | Some v -> groups, v :: current
        | None ->
            if current = [] then
                groups, []
            else
                List.rev current :: groups, []

    let groups, current = Seq.fold folder ([], []) input

    if current = [] then groups else List.rev current :: groups
    |> List.rev

let toCalculation (numbers: string list) =
    let toNumberAndOperationSymbol (l: string) =
        (l[.. l.Length - 2], l[l.Length - 1])
        |> fun (numberString, op) -> numberString |> int64, op

    numbers
    |> List.map toNumberAndOperationSymbol
    |> List.fold
        (fun agg (number, operator) ->
            { Values = number :: agg.Values
              Operation =
                match operator with
                | ' ' -> agg.Operation
                | '+' -> add
                | '*' -> multiply
                | _ -> failwithf "Unknown operator %c" operator })
        { Values = List.empty; Operation = add }
    |> fun c -> { c with Values = List.rev c.Values }

let inputFile = "input.txt"
// let inputFile = "demoInput.txt"
let inputLines = File.ReadAllLines inputFile

let l =
    inputLines
    |> Array.map (fun s -> s.Split(' ', StringSplitOptions.RemoveEmptyEntries))

let grid = Array2D.init l[0].Length l.Length (fun i j -> l[j][i])

grid |> aggregate |> Seq.sum |> printfn "Sum of calculations: %A"

inputLines
|> Array.map (fun v -> v.ToCharArray())
|> fun l -> Array2D.init l.Length l[0].Length (fun i j -> l[i][j])
|> toNumberStrings
|> toCalculationGroups
|> List.map toCalculation
|> List.map (fun c -> c.Values |> List.reduce (fun u v -> c.Operation u v))
|> List.sum
|> printfn "Sum when reading like an cephalopod: %A"
