module DropCalculator.Program

open System

[<EntryPoint>]
let main _ =
    printfn "Loading"

    match LootTable.load () with
    | Result.Error(mob, err) -> printfn $"Error parsing from %s{mob}: {err}"
    | Result.Ok lootTable ->
        let mutable running = true

        let random = Random()

        while running do
            printf "> "

            Console.ReadLine().ToLower()
            |> Command.parse lootTable
            |> function
                | Result.Error l ->
                    l
                    |> List.iter (fun e ->
                        match e with
                        | Command.UnknownCommand e -> printfn $"Unknown command {e}"
                        | Command.LootTableError e -> printfn $"Error {e}"
                        | Command.MissingArgument e -> printfn $"Error {e}")

                | Result.Ok command ->
                    match command with
                    | NOP -> ()
                    | List -> lootTable |> List.map LootTable.name |> List.iter (printfn "%s")
                    | Exit -> running <- false
                    | Hoard m ->
                        let money, valuables, magicItems = LootTable.rollHoard random m
                        money |> List.iter (fun (coin, value) -> printfn $"{coin}: {value}")

                        valuables
                        |> Option.iter (fun (amount, value) -> printfn $"Found {amount} valuables worth {value}gp each")

                        magicItems
                        |> List.iter
                            (fun
                                { Name = name
                                  Rarity = rarity
                                  Value = (coin, amount) } -> printfn $"{name}: {rarity} worth {amount} {coin}")

                    | Kill kills ->
                        for mob, amount in kills do
                            printfn $"Kill {amount} {mob.Name} with CR {mob.CR}"

                        let money, meat, items = LootTable.rollKill random kills

                        for pair in money do
                            printfn $"{pair.Key}: {pair.Value}"

                        printfn $"Butchered {meat} pieces of meat"

                        for pair in items do
                            let name, size = pair.Key
                            let conditions = pair.Value
                            let total = Map.fold (fun acc _ count -> acc + count) (Fixed 0) conditions
                            printfn $"Found {total} {size} {name}s, of which:"

                            for condition in conditions do
                                printfn $"- {condition.Value} are {condition.Key}"



    0
