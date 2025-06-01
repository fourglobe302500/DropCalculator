namespace DropCalculator

open System

module Result =
    let sequenceMany l =
        let f v acc =
            match v, acc with
            | Result.Error v, Result.Error l -> Result.Error <| v :: l
            | Result.Error v, _ -> Result.Error <| v :: []
            | _, Result.Error l -> Result.Error l
            | Result.Ok v, Result.Ok l -> Result.Ok <| v :: l

        List.foldBack <| f <| l <| Result.Ok []

module Extra =
    let roll (random: Random) count dice =
        List.sum [ for _ in 1..count -> random.Next(dice) + 1 ]

    let (|Range|_|) l u v = l <= v && v <= u
