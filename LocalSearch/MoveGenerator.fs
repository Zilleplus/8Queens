namespace LocalSearch

[<AutoOpen>]
module MoveGenerator =
    open ChessBoard

    type Move = {from:Position;too:Position}

    let CreateEmptyMove()= {from = {x=1;y=1};too = {x=1;y=1}}
    let GenerateMoves queens =
        queens 
            |> Seq.map(fun q-> 
                {1.. (Seq.length queens)} 
                |> Seq.filter(fun i -> q.position.y <> i) 
                |> Seq.map(fun i->  {from=q.position;too={x=q.position.x;y=i}}))
                |> Seq.collect(fun x->x)