module DropCalculator.Program


[<EntryPoint>]
let main _ =
    // TODO: Make main loop for the drop calculator
    let mut lootTable = LootTable.load ()
    
    LootTable.load () |> printfn "%A"

    0
