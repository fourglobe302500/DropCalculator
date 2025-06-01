namespace DropCalculator

open FParsec

type CoinType =
    | Copper
    | Silver
    | Electrum
    | Gold
    | Platinum

[<RequireQualifiedAccess>]
module CoinType =
    type ParseError =
        | InvalidCoinType of string
        | InvalidNumber of string

        override this.ToString() =
            match this with
            | InvalidCoinType s -> $"CoinType: %s{s} is a invalid coin type"
            | InvalidNumber s -> $"CoinType: %s{s} is a invalid whole number"

    let coinParser =
        choice
            [ pstring "cp" >>% Result.Ok Copper
              pstring "sp" >>% Result.Ok Silver
              pstring "ep" >>% Result.Ok Electrum
              pstring "gp" >>% Result.Ok Gold
              pstring "pp" >>% Result.Ok Platinum
              many1Chars anyChar |>> fun s -> Result.Error <| InvalidCoinType s ]

    let valueParser =
        choice
            [ puint32 .>>. coinParser
              |>> fun (i, coin) -> Result.map (fun coin -> coin, i) coin
              many1Chars anyChar |>> fun s -> Result.Error <| InvalidNumber s ]

    let parseValue s =
        match run valueParser s with
        | Success(v, _, _) -> v
        | Failure _ -> failwith "Fatal mistake"
