open System
open System.IO

type Range = { From: int64; To: int64 }

let toRange (rangeString: string) =
    match rangeString.Split '-' with
    | [| fromStr; toStr |] ->
        { From = int64 fromStr
          To = int64 toStr }
    | _ -> failwithf "Invalid range string: %s" rangeString

let isSegmentRepeated (str: string) (segmentLength: int) =
    let segmenString = str[.. segmentLength - 1]

    match str.Replace(segmenString, "") with
    | "" -> Some str
    | _ -> None


// introduce chars to consider that goes from 1 to length / 2 ?  we might then use a skip and take approach
let isRepeated (str: string) =
    let halfLength = str.Length / 2

    [ 1..halfLength ] |> Seq.tryPick (isSegmentRepeated str)


let getInvalidIds (range: Range) =
    [ range.From .. range.To ]
    |> Seq.map string
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
