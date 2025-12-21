open System.IO

type IngredientId = int64

type FreshIdRange =
    { From: IngredientId; To: IngredientId }

let toRange (line: string) =
    match line.Split '-' with
    | [| f; t |] -> { From = int64 f; To = int64 t }
    | _ -> failwithf "Invalid range format: %s" line

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

printf "%A" freshIngredients
