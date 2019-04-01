namespace LocalSearch

[<AutoOpen>]
module MoveRunner =
    open ChessBoard
    open LocalSearch

    let run moveGenerator queens evaluator =
        let localSearch = match SDRunner.runFree moveGenerator queens evaluator 10 with 
            | SDRunner.LocalMinimum(currentQueens,index)-> currentQueens
            | SDRunner.OutOfIterations(currentQueens) -> currentQueens        
        
        localSearch