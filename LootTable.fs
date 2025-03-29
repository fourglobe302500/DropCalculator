module DropCalculator.LootTable

open System.IO
open FSharp.Data
open FSharpx.Result
open FParsec

type LootTableSchema = JsonProvider<Schema="format.json">

let (|=>) a p = a |>> fun _ -> p

let ws = spaces

let num: Parser<int, 'a> = pint32

type Amount =
    | Dice of count: int * sides: int
    | Amount of int

[<RequireQualifiedAccess>]
module Amount =
    let dice =
        opt num .>> pchar 'd' .>>. num
        |>> fun (count, sides) -> Dice(Option.defaultValue 1 count, sides)

    let numAmount = num |>> Amount
    let parser = attempt dice <|> numAmount

type Operation =
    | Sub
    | Add
    | Scale

[<RequireQualifiedAccess>]
module Operation =
    let parser =
        choice
            [ pchar '+' |>> fun _ -> Add
              pchar '-' |>> fun _ -> Sub
              pchar '*' |>> fun _ -> Scale ]

type Range =
    | Single of int
    | Range of low: int * high: int

[<RequireQualifiedAccess>]
module Range =
    let parser =
        num .>>. opt (pchar '-' >>. num)
        |>> fun (leftRange, rightRange) ->
            match rightRange with
            | None -> Single leftRange
            | Some rightRange -> Range(leftRange, rightRange)

type Coin =
    | Copper
    | Silver
    | Electrum
    | Gold
    | Platinum

[<RequireQualifiedAccess>]
module Coin =
    let parser =
        choice
            [ pstring "cp" |=> Copper
              pstring "sp" |=> Silver
              pstring "ep" |=> Electrum
              pstring "gp" |=> Gold
              pstring "pp" |=> Platinum ]

type RollValue = Amount list * Operation list

[<RequireQualifiedAccess>]
module RollValue =
    let parser =
        Amount.parser .>> ws
        .>>. many (Operation.parser .>> ws .>>. Amount.parser .>> ws)
        |>> fun (min, rest) -> RollValue <| (min :: List.map snd rest, List.map fst rest)

type Table<'T> = { Roll: Amount; Rows: 'T list }

type Harvest =
    { DC: int
      Amount: RollValue
      Name: string }

[<RequireQualifiedAccess>]
module Harvest =
    let parser =
        pstring "DC" >>. ws >>. num .>> ws
        .>>. between (pchar '(') (pchar ')') RollValue.parser
        .>> ws
        .>>. between (pchar '\'') (pchar '\'') (manySatisfy (fun c -> c <> '\''))
        |>> fun ((dc, value), name) -> { DC = dc; Amount = value; Name = name }

type Loot = Range * RollValue * Coin

[<RequireQualifiedAccess>]
module Loot =
    let parser =
        Range.parser .>> ws .>>. between (pchar '(') (pchar ')') RollValue.parser .>> ws
        .>>. Coin.parser
        |>> fun ((range, amount), coin) -> range, amount, coin

[<RequireQualifiedAccess>]
module Hoard =
    type Money = RollValue * Coin

    type Valuable =
        | Art of RollValue * Coin * int
        | Gem of RollValue * Coin * int

    [<RequireQualifiedAccess>]
    module Valuable =
        let valType = choice [ pstring "art" |=> Art; pstring "gems" |=> Gem ]

        let parser =
            choice
                [ pchar '-' |=> None
                  between (pchar '(') (pchar ')') RollValue.parser .>> pchar '~'
                  .>>. num
                  .>>. Coin.parser
                  .>> pchar '~'
                  .>>. valType
                  |>> fun (((amount, value), coin), typeBuilder) -> Some(typeBuilder (amount, coin, value)) ]

    type MagicTable =
        | A of RollValue
        | B of RollValue
        | C of RollValue
        | D of RollValue
        | E of RollValue
        | F of RollValue
        | G of RollValue
        | H of RollValue
        | I of RollValue

    [<RequireQualifiedAccess>]
    module MagicTable =
        let parser =
            choice
                [ pchar '-' |=> None
                  between (pchar '(') (pchar ')') RollValue.parser .>> pstring "MIT"
                  .>>. anyOf [ 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I' ]
                  |>> fun (value, c) ->
                      match c with
                      | 'A' -> Some <| A(value)
                      | 'B' -> Some <| B(value)
                      | 'C' -> Some <| C(value)
                      | 'D' -> Some <| D(value)
                      | 'E' -> Some <| E(value)
                      | 'F' -> Some <| F(value)
                      | 'G' -> Some <| G(value)
                      | 'H' -> Some <| H(value)
                      | 'I' -> Some <| I(value)
                      | _ -> None ]

    type Row = Range * Valuable option * MagicTable option

    [<RequireQualifiedAccess>]
    module Row =
        let parser =
            Range.parser .>> ws .>> pchar '|' .>> ws .>>. Valuable.parser
            .>> ws
            .>> pchar '|'
            .>> ws
            .>>. MagicTable.parser
            |>> fun ((range, valuable), table) -> Row(range, valuable, table)

    type Model =
        { Money: Money list
          Valuables: Table<Row> }

    let parser = Row.parser
    let moneyParser = RollValue.parser .>> ws .>>. Coin.parser |>> Money

type Equipment = { Amount: RollValue; Name: string }

[<RequireQualifiedAccess>]
module Equipment =
    let parser =
        RollValue.parser .>> ws
        .>>. between (pchar '\'') (pchar '\'') (manySatisfy (fun c -> c <> '\''))
        |>> fun (value, name) -> { Name = name; Amount = value }

type Meat =
    | Inedible
    | Tiny
    | Small
    | Medium
    | Large
    | Huge
    | Gargantuan

[<RequireQualifiedAccess>]
module Meat =
    let parser =
        choice
            [ pstring "inedible" |=> Inedible
              pstring "tiny" |=> Tiny
              pstring "small" |=> Small
              pstring "medium" |=> Medium
              pstring "large" |=> Large
              pstring "huge" |=> Huge
              pstring "gargantuan" |=> Gargantuan ]

type LootTable =
    { Harvest: Harvest list
      Loot: Table<Loot>
      Hoard: Hoard.Model
      Equipment: Equipment list
      Trinkets: Table<string>
      Meat: Meat }

let test = run Hoard.parser

let parse parser =
    run parser
    >> function
        | Failure(msg, _err, _) -> Result.Error msg
        | Success(result, _, _) -> Result.Ok result

let parseMany parser = List.map (parse parser) >> sequence

let fromString s =

    let result = ResultBuilder()

    result {
        let JsonTable = LootTableSchema.Parse s
        let! harvest = JsonTable.Harvest |> Array.toList |> parseMany Harvest.parser

        let! lootRoll = parse Amount.dice JsonTable.Loot.Roll
        let! lootRows = JsonTable.Loot.Table |> Array.toList |> parseMany Loot.parser


        let! equipment = JsonTable.Equipment |> Array.toList |> parseMany Equipment.parser

        let! meat = JsonTable.Meat.JsonValue.AsString() |> parse Meat.parser

        let! hoardMoney = JsonTable.Hoard.Money |> Array.toList |> parseMany Hoard.moneyParser
        let! hoardRoll = parse Amount.dice JsonTable.Hoard.Roll
        let! hoardRows = JsonTable.Hoard.Table |> Array.toList |> parseMany Hoard.parser

        let trinketsRows = JsonTable.Trinkets |> Array.toList
        let trinketsRoll = Dice(1, List.length trinketsRows)

        return
            { Harvest = harvest
              Loot = { Rows = lootRows; Roll = lootRoll }
              Hoard =
                { Money = hoardMoney
                  Valuables = { Roll = hoardRoll; Rows = hoardRows } }
              Equipment = equipment
              Trinkets =
                { Roll = trinketsRoll
                  Rows = trinketsRows }
              Meat = meat }
    }

let load () =
    let folder = "C:\\projects\\DropCalculator\\data"

    Directory.EnumerateFiles folder
    |> List.ofSeq
    |> List.map (fun file -> file, File.ReadAllText file |> fromString)
    |> Map.ofList
