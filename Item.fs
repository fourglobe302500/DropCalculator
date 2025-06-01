namespace DropCalculator

open FParsec
open FSharpx
open Microsoft.FSharp.Core

type Rarity =
    | Common
    | Uncommon
    | Rare
    | VeryRare
    | Legendary

[<RequireQualifiedAccess>]
module Rarity =
    type ParseError =
        | InvalidRarity of string

        override this.ToString() =
            match this with
            | InvalidRarity s -> $"InvalidRarity: {s}"

    let parse =
        function
        | "Common" -> Result.Ok Common
        | "Uncommon" -> Result.Ok Uncommon
        | "Rare" -> Result.Ok Rare
        | "VeryRare" -> Result.Ok VeryRare
        | "Legendary" -> Result.Ok Legendary
        | s -> Result.Error <| InvalidRarity s

type Condition =
    | Base
    | Superb
    | AlmostNew
    | Low
    | Terrible
    | Broken

type Amount =
    | Fixed of int
    | Rolled of int * int
    | Compound of Dies: Map<int, int> * Static: int

    override this.ToString() =
        match this with
        | Fixed n -> n.ToString()
        | Rolled(count, die) -> if count <> 1 then $"{count}d{die}" else $"d{die}"
        | Compound(map, n) ->
            let dies =
                Map.fold (fun acc die count -> Rolled(count, die).ToString() :: acc) [] map
                |> List.reduce (fun l r -> $"{l} + {r}")

            if n = 0 then $"{dies}" else $"{dies} + {n}"

    static member (+)(this, other) =
        match this, other with
        | Fixed 0, other
        | other, Fixed 0 -> other
        | Fixed n, Fixed m -> Fixed(n + m)

        | Compound(map, add), Fixed n
        | Fixed n, Compound(map, add) -> Compound(map, add + n)

        | Rolled(count, die), Fixed n
        | Fixed n, Rolled(count, die) -> Compound(Map [ (die, count) ], n)

        | Compound(map, add), Rolled(count, die)
        | Rolled(count, die), Compound(map, add) ->
            (Map.change die (Option.map ((+) count) >> Option.orElse (Some count)) map, add)
            |> Compound

        | Rolled(count1, die1), Rolled(count2, die2) ->
            (Map.change die2 (Option.map ((+) count2) >> Option.orElse (Some count2)) (Map [ (die1, count1) ]), 0)
            |> Compound

        | Compound(map1, add1), Compound(map2, add2) ->
            let fold map die count =
                Map.change die (Option.map ((+) count) >> Option.orElse (Some count)) map

            (Map.fold fold map1 map2, add1 + add2) |> Compound


[<RequireQualifiedAccess>]
module Amount =
    type ParseError =
        | InvalidAmount of string

        override this.ToString() =
            match this with
            | InvalidAmount s -> $"InvalidAmount: {s}"

    let parse value =
        run
        <| choice
            [ pchar 'd' >>. pint32 |>> (tuple2 1 >> Amount.Rolled) |>> Result.Ok
              pint32 .>>. opt (pchar 'd' >>. pint32)
              |>> (function
              | l, None -> Fixed l
              | l, Some(r) -> Rolled(l, r))
              |>> Result.Ok
              many1Chars anyChar |>> ParseError.InvalidAmount |>> Result.Error ]
        <| value
        |> function
            | Success(r, _, _) -> r
            | ParserResult.Failure(e, _, _) -> failwith $"Unknown error {e}"

type Item =
    { Amount: Amount
      Name: string
      Value: CoinType * uint
      Rarity: Rarity
      Condition: Condition
      Size: Size }

[<RequireQualifiedAccess>]
module Item =
    type ParseError =
        | AmountParseError of Amount.ParseError
        | BaseValueParseError of CoinType.ParseError
        | RarityParseError of Rarity.ParseError

        override this.ToString() =
            match this with
            | AmountParseError e -> $"Amount->{e}"
            | BaseValueParseError e -> $"CoinType->{e}"
            | RarityParseError e -> $"Rarity->{e}"

    let parse size (root: LootTableSchema.Record) =
        Result.result {
            let name = root.Nome
            let! amount = Amount.parse root.Amount |> Result.mapError AmountParseError
            let! value = CoinType.parseValue root.BaseValue |> Result.mapError BaseValueParseError

            let! rarity =
                root.Rarity
                |> Option.map (Rarity.parse >> Result.mapError RarityParseError)
                |> Option.getOrElse (Result.Ok Rarity.Common)

            return
                { Name = name
                  Amount = amount
                  Condition = Base
                  Rarity = rarity
                  Size = size
                  Value = value }
        }

    let parseList size l =
        l |> List.map (parse size) |> Result.sequence
