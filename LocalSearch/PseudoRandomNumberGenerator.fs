namespace LocalSearch

module PseudoRandomNumberGenerator =
    open System
    let generate count seed = 
        let rnd = new Random(seed)
        seq{1..count} |> Seq.map(fun x->rnd.NextDouble())
