open System
open System.IO


let inputFile = "input.txt"
// let inputFile = "demoInput.txt"
let inputLines = File.ReadAllLines inputFile

let add (a: int64) (b: int64) = a + b
let multiply a b = a * b

let getAggregationOperation (op: string) =
    match op with
    | "+" -> add, 0
    | "*" -> multiply, 1
    | _ -> failwithf "unknown operator %s" op

let aggregateLine (l: string[]) =
    l
    |> Array.rev
    |> fun v -> getAggregationOperation (v |> Array.head), Array.tail v
    |> fun ((operation, neutralElement), list) -> list |> Array.map int64 |> Array.fold operation neutralElement

let aggregate (g: string[,]) =
    seq {
        for i in [ 0 .. Array2D.length1 g - 1 ] do
            yield g[i, *] |> aggregateLine
    }

let l =
    inputLines
    |> Array.map (fun s -> s.Split(' ', StringSplitOptions.RemoveEmptyEntries))

let grid = Array2D.init l[0].Length l.Length (fun i j -> l[j][i])

grid |> aggregate |> Seq.sum |> printfn "Sum of calculations: %A"
