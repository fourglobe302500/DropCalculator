module DropCalculator.Program

open System
open FParsec
open FSharpx


type Command =
    | Hoard of LootTable
    | Kill of (LootTable * uint) list
    | Exit
    | List

[<RequireQualifiedAccess>]
module Command =
    type ParserError =
        | UnknownCommand of string
        | LootTableError of LootTable.ParseError

        override this.ToString() =
            match this with
            | UnknownCommand s -> $"Unknown command '{s}'"
            | LootTableError e -> $"LootTable->{e}"

    let parser lootTable =
        attempt (
            choice
                [ pstring "list" >>% Result.Ok List
                  pstring "exit" >>% Result.Ok Exit
                  pstring "hoard" >>. spaces1 >>. LootTable.nameParser lootTable
                  |>> Result.map Hoard
                  |>> Result.mapError LootTableError
                  pstring "kill"
                  >>. spaces1
                  >>. sepBy1 (LootTable.nameParser lootTable .>> spaces .>>. puint32) spaces1
                  |>> List.map (fun (t, a) -> Result.map (flip tuple2 a) t)
                  |>> Result.sequence
                  |>> Result.map Kill
                  |>> Result.mapError LootTableError ]
            .>> eof
        )
        <|> (many1Chars anyChar |>> fun s -> Result.Error <| UnknownCommand s)

    let parse lootTable =
        run <| parser lootTable
        >> function
            | Failure(e, _, _) -> failwith $"Unknown Error {e}"
            | Success(r, _, _) -> r

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
                | Result.Error e ->
                    match e with
                    | Command.UnknownCommand e -> printfn $"Unknown command {e}"
                    | Command.LootTableError e -> printfn $"Error {e}"
                | Result.Ok command ->
                    match command with
                    | List -> lootTable |> List.map LootTable.name |> List.iter (printfn "%s")
                    | Exit -> running <- false
                    | Hoard m -> printfn $"Get Hoard for {m.Name} with CR {m.CR}"
                    | Kill kills ->
                        for mob, amount in kills do
                            printfn $"Kill {amount} {mob.Name} with CR {mob.CR}"


    0
