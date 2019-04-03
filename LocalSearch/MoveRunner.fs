namespace LocalSearch

[<AutoOpen>]
module MoveRunner =
    open ChessBoard
    open LocalSearch

    let run moveGenerator queens evaluator =
        let localSearch inputQueens = match SDRunner.runFree moveGenerator queens evaluator 10 with 
            | SDRunner.LocalMinimum(currentQueens,index)-> currentQueens
            | SDRunner.OutOfIterations(currentQueens) -> currentQueens        
        
        let SASearch inputQueens = SARunner.run moveGenerator inputQueens evaluator 100

        let firstTry = SASearch (localSearch queens)
        
        localSearch firstTry