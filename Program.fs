module DropCalculator.Program

open System
open FSharpx
open FParsec
open Helpers

type Commands =
    | Reload
    | Kill of (int * string) list
    | End

[<RequireQualifiedAccess>]
module Commands =
    let parser =
        choice
            [ pstring "reload" |=> Reload
              pstring "end" |=> End
              pstring "kill" .>> ws
              >>. sepBy1 (num .>> ws .>>. between (pchar '\'') (pchar '\'') (manySatisfy ((<>) '\''))) spaces1
              |>> Kill ]

[<EntryPoint>]
let main _ =
    // TODO: Make main loop for the drop calculator
    Result.result {
        let mutable lootTable = Map []
        let mutable run = true

        let load () =
            match LootTable.load () with
            | Result.Error msg -> printfn $"%s{msg}"
            | Result.Ok v ->
                lootTable <- v
                lootTable |> Map.keys |> Seq.iter (printfn "Loaded: %s")

        load ()

        while run do
            let line = Console.ReadLine()

            match! parse Commands.parser line with
            | End -> run <- false
            | Reload -> load ()
            | Kill values -> printfn $"Kill %A{values}"

    }
    |> ignore

    0
