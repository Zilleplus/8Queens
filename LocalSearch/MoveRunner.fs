namespace LocalSearch

[<AutoOpen>]
module MoveRunner =
    open ChessBoard
    open MoveGenerator
    open Board

    let ApplyMove queens move=
       queens 
            |> Seq.map(fun q-> 
                match (q.position=move.from) with 
                    true ->  createQueenNewPos(move.too) |_ ->  q )

    // try all moves return only the best move
    let Run moves queens evaluator =
        let accumulateBestMove  (acc_move,acc_cost) (move,cost) =
            match acc_cost>cost with
            | true ->  (move,cost)
            | _ -> (acc_move,acc_cost)
        let (bestMove,cost) = 
            moves 
            |> Seq.map(fun move-> (move, evaluator (ApplyMove queens move) 1))
            |> Seq.fold accumulateBestMove (CreateEmptyMove(),1000)
        
        ApplyMove queens bestMove |> Seq.toList

    