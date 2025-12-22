open System.IO

type IngredientId = int64

type FreshIdRange =
    { From: IngredientId; To: IngredientId }

let toRange (line: string) =
    match line.Split '-' with
    | [| f; t |] -> { From = int64 f; To = int64 t }
    | _ -> failwithf "Invalid range format: %s" line

let merge (a: FreshIdRange) (b: FreshIdRange) =
    match a, b with
    | { FreshIdRange.From = aFrom
        FreshIdRange.To = aTo },
      { FreshIdRange.From = bFrom
        FreshIdRange.To = bTo } when aFrom <= bFrom && bFrom <= aTo -> Some { From = aFrom; To = max aTo bTo }
    | _ -> None

let mergeRangesInSeq (ranges: seq<FreshIdRange>) =
    let folder (current, acc) next =
        match merge current next with
        | Some merged -> merged, acc
        | None -> next, current :: acc

    let first = Seq.head ranges
    let rest = Seq.tail ranges

    let last, acc = Seq.fold folder (first, []) rest

    List.rev (last :: acc)

let isInRange (ingredient: IngredientId) (range: FreshIdRange) =
    range.From <= ingredient && ingredient <= range.To

let isFreshIngredient (ranges: seq<FreshIdRange>) (ingredientId: IngredientId) =
    ranges |> Seq.exists (isInRange ingredientId)

let inputFile = "input.txt"
// let inputFile = "demoInput.txt"
let inputLines = File.ReadAllLines inputFile

let ranges = inputLines |> Seq.takeWhile (fun line -> line <> "") |> Seq.map toRange

let ingredients =
    inputLines
    |> Seq.skipWhile (fun line -> line <> "")
    |> Seq.skip 1
    |> Seq.map int64

let freshIngredients =
    ingredients
    |> Seq.map (isFreshIngredient ranges)
    |> Seq.sumBy (fun v -> if v then 1 else 0)

printfn "Number of fresh ingredients %A" freshIngredients

ranges
|> Seq.sortBy (fun r -> r.From)
|> mergeRangesInSeq
|> List.map (fun v -> v.To - v.From + int64 1)
|> List.sum
|> printfn "total is: %i"
