open System
open System.IO

type Range = { From: int64; To: int64 }

let toRange (rangeString: string) =
    match rangeString.Split '-' with
    | [| fromStr; toStr |] ->
        { From = int64 fromStr
          To = int64 toStr }
    | _ -> failwithf "Invalid range string: %s" rangeString

let isRepeated (str: string) =
    let halfLength = str.Length / 2

    match str[.. halfLength - 1], str[halfLength..] with
    | x, y when x = y -> Some str
    | _ -> None



let getInvalidIds (range: Range) =
    [ range.From .. range.To ]
    |> Seq.map string
    |> Seq.filter (fun id -> id.Length % 2 = 0)
    |> Seq.map isRepeated
    |> Seq.choose id
    |> Seq.map decimal

let inputFile = "input.txt"
// let inputFile = "demoInput.txt"

let input =
    File.ReadAllText inputFile
    |> fun i -> i.Split([| ',' |], StringSplitOptions.RemoveEmptyEntries)

let ranges = input |> Seq.map toRange
printfn "%A" ranges
let invalidIds = ranges |> Seq.map getInvalidIds
printfn "%A" invalidIds
let sum = invalidIds |> Seq.map Seq.sum |> Seq.sum
printfn "%M" sum
