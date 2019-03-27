namespace LocalSearch

[<AutoOpen>]
module MoveRunner =
    open ChessBoard
    open LocalSearch

    let run moves queens evaluator =
        match SDRunner.runFree moves queens evaluator 10 with 
            | SDRunner.LocalMinimum(currentQueens,index)-> currentQueens
            | SDRunner.OutOfIterations(currentQueens) -> currentQueens        