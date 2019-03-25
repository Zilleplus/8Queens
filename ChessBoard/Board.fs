namespace ChessBoard

type Position = {x:int;y:int}
type Queen = {position:Position}

module Board =
    let createQueen x y= { position = { x=x;y=y} }
    let createQueenNewPos newPos= { position = newPos }

    let hasDiagonalConflicts queen queens =
        let conflictingQueens = seq{ 
            for i in -8 .. 8  do 
                if i<>0 then  yield {x=i+queen.position.x;y=i+queen.position.y}}
        Seq.exists (fun q-> Seq.contains q.position conflictingQueens) queens
        
    let evaluateDiagonal queens =
        queens 
            |> Seq.filter(fun q->hasDiagonalConflicts q queens) 
            |> Seq.length

    let evaluateHorizontally queens =
        Seq.length queens - Seq.length (queens 
            |> Seq.groupBy(fun q -> q.position.x))

    let evaluateVertically queens =
        Seq.length queens - Seq.length (queens 
            |> Seq.groupBy(fun q -> q.position.y))

    let isDiagonalStep (position1,position2) =
        let xDiff = position1.x-position2.x
        let yDiff = position1.y-position2.y

        abs(yDiff) = 1 &&  abs(yDiff) = 1 && sign(xDiff)=sign(yDiff)

    let evaluate queens penalty=
        (evaluateHorizontally queens+ evaluateVertically queens + evaluateDiagonal queens)*penalty
