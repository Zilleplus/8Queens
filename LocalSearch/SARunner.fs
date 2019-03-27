namespace LocalSearch

module SARunner = 
    open System

    let run moves queens evaluator =
        let rnd = new Random()

        let rec move (random: Random) = 
            match (random.NextDouble()) with
            | n when n < 0.5  -> Seq.item 0 moves
            |_ -> move random

        move rnd


