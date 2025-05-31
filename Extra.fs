namespace DropCalculator

module Result =
    let sequenceMany l =
        let f v acc =
            match v, acc with
            | Result.Error v, Result.Error l -> Result.Error <| v :: l
            | Result.Error v, _ -> Result.Error <| v :: []
            | _, Result.Error l -> Result.Error l
            | Result.Ok v, Result.Ok l -> Result.Ok <| v :: l

        List.foldBack <| f <| l <| Result.Ok []
