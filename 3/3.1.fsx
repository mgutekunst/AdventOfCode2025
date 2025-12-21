open System
open System.IO

let dump any =
    printfn "%A" any
    any


let rec maxNDigits (numDigits: int) (str: string) =
    let rec loop n (s: string) acc =
        if n = 0 || s.Length = 0 then
            acc |> List.rev
        else
            let maxIndex =
                s[.. s.Length - n] |> Seq.mapi (fun i c -> i, c) |> Seq.maxBy snd |> fst

            let digit = s.[maxIndex]

            loop (n - 1) s[maxIndex + 1 ..] (digit :: acc)



    loop numDigits str []


let maxTwoDigits (str: string) = maxNDigits 2

let inputFile = "input.txt"
// let inputFile = "demoInput.txt"

let input = File.ReadLines inputFile

input
|> Seq.map (maxNDigits 12)
|> Seq.map String.Concat
|> Seq.map int64
|> Seq.sum
|> printfn "%d"
