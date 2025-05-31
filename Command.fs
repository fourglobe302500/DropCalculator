namespace DropCalculator

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
        | MissingArgument of string

        override this.ToString() =
            match this with
            | UnknownCommand s -> $"UnknownCommand '{s}'"
            | LootTableError e -> $"LootTable->{e}"
            | MissingArgument s -> $"MissingArgument {s}"

    let parseList = pstring "list" >>% Result.Ok List

    let parseExit = pstring "exit" >>% Result.Ok Exit

    let catchFail err (parser: Parser<_, _>) : Parser<_, _> =
        fun stream ->
            let reply = parser stream

            if reply.Status = ReplyStatus.Ok then
                Reply(Result.Ok())
            else
                Reply(Result.Error(err))

    let onOk p r =
        match r with
        | Result.Error e -> fun _ -> Reply <| Result.Error [ e ]
        | Result.Ok() -> p

    let parseHoard lootTable =
        pstring "hoard"
        >>. catchFail (MissingArgument "hoard must have one mob") spaces1
        >>= onOk (
            LootTable.nameParser lootTable
            |>> Result.mapError LootTableError
            |>> Result.mapError List.singleton
        )
        |>> Result.map Hoard

    let parseKill lootTable : Parser<Result<Command, ParserError list>, _> =

        pstring "kill"
        >>. catchFail (MissingArgument "kill must have at least one mob") spaces1
        >>= onOk (
            many (
                LootTable.nameParser lootTable .>> spaces .>>. opt puint32 .>> spaces
                |>> (fun (name, value) -> Result.map <| (flip tuple2 << Option.getOrElse 1u <| value) <| name)
                |>> Result.mapError LootTableError
            )
            |>> Result.sequenceMany
        )
        |>> Result.map Kill

    let parser lootTable : Parser<Result<Command, ParserError list>, _> =
        (choice [ parseList; parseExit; parseHoard lootTable; parseKill lootTable ]
         .>> eof)
        <|> (many1Chars anyChar |>> fun s -> Result.Error <| [ UnknownCommand s ])

    let parse lootTable : string -> Result<Command, ParserError list> =
        String.replace '_' ' '
        >> run (parser lootTable)
        >> function
            | Failure(e, _, _) -> failwith $"Unknown Error {e}"
            | Success(r, _, _) -> r
