namespace ChessBoard

type Position = {x:int;y:int}
type Queen = {position:Position}

module Board =
    let createQueen x y= { position = { x=x;y=y} }
    let createQueenNewPos newPos= { position = newPos }

    let hasDiagonalConflicts queen queens =
        let numberOfQueens = queens |> Seq.length
        let conflictingQueens = seq{ 
            for i in -numberOfQueens  .. numberOfQueens  do 
                if i<>0 then  yield {x=i+queen.position.x;y=i+queen.position.y}}
        Seq.exists (fun q-> Seq.contains q.position conflictingQueens) queens
        
    let evaluateDiagonal queens =
        queens 
            |> Seq.filter(fun q->hasDiagonalConflicts q queens) 
            |> Seq.length

    let countConflictGroups conflictGroups =
        conflictGroups
            |> Seq.map(fun (i,qs) ->Seq.length qs)
            |> Seq.filter(fun l -> l>1)
            |> Seq.fold (fun acc l->acc+l) 0

    let evaluateHorizontally queens =
        queens 
            |> Seq.groupBy(fun q -> q.position.x) 
            |> countConflictGroups

    let evaluateVertically queens =
        queens 
            |> Seq.groupBy(fun q -> q.position.y) 
            |> countConflictGroups

    let isDiagonalStep (position1,position2) =
        let xDiff = position1.x-position2.x
        let yDiff = position1.y-position2.y

        abs(yDiff) = 1 &&  abs(yDiff) = 1 && sign(xDiff)=sign(yDiff)

    let evaluate queens penalty=
        (evaluateHorizontally queens+ evaluateVertically queens + evaluateDiagonal queens)*penalty
