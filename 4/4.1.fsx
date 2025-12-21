open System.IO

let PaperRoll = '@'
let EmptyPlace = '.'
let CanBeMoved = 'X'

let inputFile = "input.txt"
// let inputFile = "demoInput.txt"
let inputLines = File.ReadAllLines inputFile

let grid =
    Array2D.init inputLines.Length inputLines.[0].Length (fun i j -> inputLines.[i].[j])

let betterGrid =
    Array2D.create (inputLines.Length + 2) (inputLines[0].Length + 2) '.'

let countInArray (lookAt: char[]) (grid: char[,]) =
    let mutable count = 0

    grid
    |> Array2D.iter (fun v ->
        if Array.contains v lookAt then
            count <- count + 1)

    count

let countPaperRolls = countInArray [| PaperRoll; CanBeMoved |]

let countMoveablePaperRolls = countInArray [| CanBeMoved |]

Array2D.blit grid 0 0 betterGrid 1 1 (Array2D.length1 grid) (Array2D.length2 grid)

for i in 1 .. (Array2D.length1 betterGrid - 2) do
    for j in 1 .. (Array2D.length2 betterGrid - 2) do
        if betterGrid[i, j] = PaperRoll then
            let subGrid = Array2D.create 3 3 '.'
            // copy the 3x3 grid around (i,j)
            Array2D.blit betterGrid (i - 1) (j - 1) subGrid 0 0 3 3

            if countPaperRolls subGrid - 1 < 4 then
                Array2D.set betterGrid i j CanBeMoved


printfn "Calculated Movings: \n%A" betterGrid
betterGrid |> countMoveablePaperRolls |> printfn "%i can be moved"
