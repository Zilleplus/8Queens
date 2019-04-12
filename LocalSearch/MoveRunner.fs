namespace LocalSearch

[<AutoOpen>]
module MoveRunner =
    open LocalSearch

    let run moveGenerator queens evaluator =
        let localSearch inputQueens = match SDRunner.runFree moveGenerator queens evaluator 20 with 
            | SDRunner.LocalMinimum(currentQueens,index)-> currentQueens
            | SDRunner.OutOfIterations(currentQueens) -> currentQueens        
        
        let SASearch inputQueens seed = SARunner.run moveGenerator inputQueens evaluator 20 seed

        let evaluateLocalMinima qs = evaluator (localSearch qs) 1

        // start running from local minima, 
        // try SA only use it if the solution is better then the local search solution.
        let maxIterations = 5 // try SA 5 times, maybe we get lucky in one of them ;-)
        let rec runSearch qs iteration = match SASearch qs iteration with
            | SARunner.MovesAccepted newQs 
                when (evaluator qs 1) < (evaluateLocalMinima newQs) 
                && iteration<maxIterations 
                    -> runSearch (localSearch newQs) (iteration+1)
            | SARunner.NoMovesAccepted when iteration<maxIterations -> runSearch qs (iteration+1)
            | _ when iteration>=maxIterations -> qs
            | _ -> runSearch qs (iteration+1)

        runSearch (localSearch queens) 1