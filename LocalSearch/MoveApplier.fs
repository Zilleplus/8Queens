namespace LocalSearch

module MoveApplier =
    open ChessBoard

    let applyMove queens move=
       queens 
            |> Seq.map(fun q-> 
                match (q.position=move.from) with 
                    true ->  Board.createQueenNewPos(move.too) |_ ->  q )

