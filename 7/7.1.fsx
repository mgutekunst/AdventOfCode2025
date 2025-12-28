open System.IO

type CellType =
    | Empty
    | Splitter
    | Beam
    | Start

    static member OfChar(c: char) =
        match c with
        | '.' -> Empty
        | '^' -> Splitter
        | '|' -> Beam
        | 'S' -> Start
        | _ -> failwithf "Unknown cell type: %c" c


type Cell =
    { Row: int
      Column: int
      Type: CellType }


let calculateBeamTransition (beam: Cell) (currentCell: Cell) =
    match currentCell.Type with
    | Empty -> [ { beam with Row = beam.Row + 1 } ]
    | Splitter ->
        [ { beam with
              Row = beam.Row + 1
              Column = beam.Column - 1 }
          { beam with
              Row = beam.Row + 1
              Column = beam.Column + 1 } ]
    | _ -> failwithf "Not implemented yet for cell type %A" currentCell.Type


let calculateBeam (beams: Cell list, splitCount: int) (currentRow: Cell[]) =
    if beams.Length = 0 then
        currentRow
        |> Array.find (fun v -> v.Type = Start)
        |> fun start ->
            [ { Row = start.Row + 1
                Column = start.Column
                Type = Beam } ],
            0
    else
        let newBeams =
            beams
            |> List.map (fun b -> calculateBeamTransition b (currentRow |> Array.find (fun l -> l.Column = b.Column)))
            |> List.reduce (fun a b -> a @ b)

        let newSplitCount = splitCount + (newBeams.Length - beams.Length)

        newBeams |> List.distinct, newSplitCount

// let inputFile = "input.txt"
let inputFile = "demoInput.txt"

inputFile
|> File.ReadAllLines
|> Array.fold
    (fun agg l ->
        calculateBeam
            agg
            (l.ToCharArray()
             |> Array.mapi (fun j c ->
                 { Row = 0
                   Column = j
                   Type = CellType.OfChar c })))
    ([], 0)
|> printfn "Result: %A"
