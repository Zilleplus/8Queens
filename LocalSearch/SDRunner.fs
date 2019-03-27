namespace LocalSearch
open ChessBoard
module SDRunner = 
    let CreateEmptyMove()= {from = {x=1;y=1};too = {x=1;y=1}}

    type SDConvergenceResult = 
        | LocalMinimum of seq<Queen>*int
        | OutOfIterations of seq<Queen>

    type SDRunnerResult =
        | SUCCESS of Move // if success return move 
        | FAILURE 

    let run moves queens evaluator =
        let accumulateBestMove  (acc_move,acc_cost) (move,cost) =
            match acc_cost>cost with
            | true -> (move,cost)
            | _    -> (acc_move,acc_cost)
        let (bestMove,cost) = 
            moves 
            |> Seq.map(fun move-> (move, evaluator (MoveApplier.applyMove queens move) 1))
            |> Seq.fold accumulateBestMove (CreateEmptyMove(),1000)

        match cost with 
            | 1000 -> FAILURE 
            | _    -> SUCCESS (bestMove)

    let runFree moves queens evaluator maxInters =
        let trySD state = run moves state evaluator

        let rec recRun currentQueens index = 
            let newIndex = index+1
            match (trySD currentQueens) with
                | SUCCESS (move) -> match index with 
                    | n when n<>maxInters-1 -> (recRun (MoveApplier.applyMove currentQueens move) newIndex) 
                    | _ -> OutOfIterations(currentQueens)
                | FAILURE -> match index with
                    | n when n=maxInters-1 -> OutOfIterations(currentQueens)
                    | _ -> LocalMinimum (currentQueens,index)

        recRun queens 0
