open System
open LocalSearch
open ChessBoard
open ChessBoard

let queenLocated  queens x y =
    Seq.length (queens |> Seq.filter(fun q->q.position.x = x && q.position.y = y) ) > 0

let getSymbol condition  = 
    match condition with true -> 'X'| _ -> '0'
 
let printBoard queens = 
    printfn "Solution has cost: %d" (Board.evaluate queens 1)
    let getChar x y = getSymbol(queenLocated queens x y)
    let row y =  seq {for x in 1..8 -> getChar x y }
    seq {for y in 1..(Seq.length queens) -> row y} 
        |> Seq.rev 
        |> Seq.toList
        |> List.map(fun r->printfn "%s" (System.String.Concat( (r|>Seq.toList))))
        |> ignore

[<EntryPoint>]
let main argv =
    printfn "Demo of 8 queen problem solved with a local search algorithm"  
    let startQueens = seq{for x in 1 .. 8 -> Board.createQueen x 1}
    printBoard startQueens 
    printfn "------"
    let move queens =  Run(GenerateMoves startQueens) queens Board.evaluate
    printBoard (move startQueens )
    printfn "------"

    let rec solveRec iterationIndex currentState =
        match iterationIndex with 
        | 20 -> currentState
        | _ -> solveRec (iterationIndex + 1) (move currentState)
    let solve = solveRec 0

    printBoard (solve (Seq.toList startQueens))

    0 // result of main