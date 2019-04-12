namespace LocalSearch

module SARunner = 
    open System

    type SARunnerResult =
        | MovesAccepted of seq<ChessBoard.Queen> // positions with there cost
        | NoMovesAccepted 

    /// The random number should be in [1-0]
    /// -> returns true if move should be accepted
    let acceptance currentCost newCost temperatur randomNumber=
        Math.Exp(-(newCost-currentCost)/temperatur) > randomNumber

    let acceptedMove queens evaluator temperature (move,randomNumber)=
        let cost = evaluator queens 1 |> double
        let newCost = evaluator (MoveApplier.applyMove queens move) 1 |> double

        let probabilityOfAcceptance = Math.Exp(-(newCost-cost)/temperature) 
        probabilityOfAcceptance > randomNumber

    let tryMoves queens evaluator moves temperature seed=
        let randomSteps = PseudoRandomNumberGenerator.generate (Seq.length moves) seed 
        Seq.zip moves randomSteps
            |> Scrambler.scramble // is this really nessesary ??
            |> Seq.tryFind (acceptedMove queens evaluator temperature) 

    let run moveGenerator initQueens evaluator maxIters seed =
        let coolingFactor = 0.5
        let rec SALoop queens temperature iteration =
            match (tryMoves queens evaluator (moveGenerator queens) temperature seed) with // only appy move if one was found
            | Some (move,_) when iteration < maxIters 
                -> SALoop (MoveApplier.applyMove queens move) (coolingFactor*temperature) (iteration+1)
            | _ -> (queens,temperature)

        let initTemperature = 0.1
        match (SALoop initQueens initTemperature 0) with
            | (_,temp) when temp=initTemperature -> NoMovesAccepted
            | (qs,_) -> MovesAccepted qs