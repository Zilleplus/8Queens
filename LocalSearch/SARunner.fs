namespace LocalSearch

module SARunner = 
    open System

    /// The random number should be in [1-0]
    /// -> returns true if move should be accepted
    let acceptance currentCost newCost temperatur randomNumber=
        Math.Exp(-(newCost-currentCost)/temperatur) > randomNumber

    let acceptedMove queens evaluator move=
        false

    let tryStep queens evaluator moves=
        let move = 
            moves 
                |> Scrambler.scramble //TODO implement scramble
                |> Seq.tryFind (acceptedMove queens evaluator)
        match move with // only appy move if one was found
            | Some m -> MoveApplier.applyMove queens m
            | None -> queens

    let run moveGenerator queens evaluator numberOfIterations =
        let randomSteps = PseudoRandomNumberGenerator.generate 10 1 // take 10 random steps with seed 1
        
        //let runRec = 
        //    match tryStep queens evaluator (moveGenerator queens) with

        0


