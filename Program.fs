module DropCalculator.Program

open System

[<EntryPoint>]
let main _ =
    printfn "Loading"

    match LootTable.load () with
    | Result.Error(mob, err) -> printfn $"Error parsing from %s{mob}: {err}"
    | Result.Ok lootTable ->
        let mutable running = true

        while running do
            printf "> "

            Console.ReadLine().ToLower()
            |> Command.parse lootTable
            |> function
                | Result.Error l ->
                    l|>List.iter
                        (fun e ->
                            match e with
                            | Command.UnknownCommand e -> printfn $"Unknown command {e}"
                            | Command.LootTableError e -> printfn $"Error {e}"
                            | Command.MissingArgument e -> printfn $"Error {e}")
                        
                | Result.Ok command ->
                    match command with
                    | List -> lootTable |> List.map LootTable.name |> List.iter (printfn "%s")
                    | Exit -> running <- false
                    | Hoard m -> printfn $"Get Hoard for {m.Name} with CR {m.CR}"
                    | Kill kills ->
                        for mob, amount in kills do
                            printfn $"Kill {amount} {mob.Name} with CR {mob.CR}"


    0
