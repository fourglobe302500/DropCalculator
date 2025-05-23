namespace DropCalculator

open System
open FSharpx

type Drop =
    { Name: string
      Type: DropType
      DC: uint
      Amount: uint
      Value: CoinType * uint }

[<RequireQualifiedAccess>]
module Drop =
    type ParseError =
        | DropTypeParseError of DropType.ParseError
        | CoinTypeParseError of CoinType.ParseError

        override this.ToString() =
            match this with
            | CoinTypeParseError err -> $"Drop->{err}"
            | DropTypeParseError err -> $"Drop->{err}"

    let parse (root: LootTableSchema.Record) =
        Result.result {
            let name = root.Nome
            let! dropType = DropType.parse root.ItemType |> Result.mapError ParseError.DropTypeParseError
            let dc = Decimal.ToUInt32 root.Dc
            let amount = Decimal.ToUInt32 root.Amount
            let! value = CoinType.parseValue root.Value |> Result.mapError ParseError.CoinTypeParseError

            return
                { Name = name
                  Type = dropType
                  DC = dc
                  Amount = amount
                  Value = value }
        }

    let parseList l = l |> List.map parse |> Result.sequence
