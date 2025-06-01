namespace DropCalculator

open System
open System.IO
open FParsec
open FSharpx
open Extra
open Microsoft.FSharp.Core

type LootTable =
    { Name: string
      CR: CR
      Size: Size
      CreatureType: CreatureType
      Loot: Drop list
      Carries: Item list }

module LootTable =
    let name lootTable = lootTable.Name

    type ParseError =
        | SizeParseError of Size.ParseError
        | CreatureTypeParseError of CreatureType.ParseError
        | DropParseError of Drop.ParseError
        | InvalidName of string
        | CRParseError of CR.ParseError
        | ItemParseError of Item.ParseError

        override this.ToString() =
            match this with
            | DropParseError err -> $"LootTable->{err}"
            | SizeParseError err -> $"LootTable->{err}"
            | CreatureTypeParseError err -> $"LootTable->{err}"
            | CRParseError err -> $"LootTable->{err}"
            | ItemParseError err -> $"LootTable->{err}"
            | InvalidName s ->
                $"LootTable: Invalid name '{s}' it's not on the list of creatures. Use 'list' to see valid names."

    let endOfName (stream: CharStream) =
        let error = ErrorMessageList <| ErrorMessage.Other "Expected end of String"

        if
            stream.IsEndOfStream
            || Char.IsWhiteSpace <| stream.Peek()
            || Char.IsDigit <| stream.Peek()
        then
            Reply(true)
        else
            Reply(ReplyStatus.Error, error)

    let nameParser lootTable =
        [ for mob in lootTable -> pstring <| mob.Name.ToLower() .>>? endOfName >>% Result.Ok mob
          yield many1Satisfy (not << Char.IsWhiteSpace) |>> InvalidName |>> Result.Error ]
        |> choice

    let rollHoard (random: Random) lootTable =
        let roll = roll random

        match lootTable.CR with
        | CR.Between 0u 4u ->
            let Copper = CoinType.Copper, roll 6 6 |> (*) 100

            let Silver = CoinType.Silver, roll 3 6 |> (*) 100

            let Gold = CoinType.Gold, roll 2 6 |> (*) 10

            let art, magic =
                match random.Next(100) + 1 with
                | Range 1 6 -> None, []
                | Range 7 16 -> Some(roll 2 6, 10), []
                | Range 17 26 -> Some(roll 2 4, 25), []
                | Range 27 36 -> Some(roll 2 6, 50), []
                | Range 37 44 -> Some(roll 2 6, 10), RandomMagicTable.A 6 random
                | Range 45 52 -> Some(roll 2 4, 25), RandomMagicTable.A 6 random
                | Range 53 60 -> Some(roll 2 6, 50), RandomMagicTable.A 6 random
                | Range 61 65 -> Some(roll 2 6, 10), RandomMagicTable.B 6 random
                | Range 66 70 -> Some(roll 2 4, 25), RandomMagicTable.B 6 random
                | Range 71 75 -> Some(roll 2 6, 50), RandomMagicTable.B 6 random
                | Range 76 78 -> Some(roll 2 6, 10), RandomMagicTable.C 6 random
                | Range 79 80 -> Some(roll 2 4, 25), RandomMagicTable.C 6 random
                | Range 81 85 -> Some(roll 2 6, 50), RandomMagicTable.C 6 random
                | Range 86 92 -> Some(roll 2 4, 25), RandomMagicTable.F 6 random
                | Range 93 97 -> Some(roll 2 4, 50), RandomMagicTable.F 6 random
                | Range 98 99 -> Some(roll 2 4, 25), RandomMagicTable.G 6 random
                | _ -> Some(roll 2 6, 50), RandomMagicTable.G 6 random

            [ Copper; Silver; Gold ], art, magic
        | CR.Between 5u 10u ->
            let Copper =
                CoinType.Copper, List.sum [ for _ in 1..2 -> random.Next(6) + 1 ] |> (*) 100

            let Silver =
                CoinType.Silver, List.sum [ for _ in 1..2 -> random.Next(6) + 1 ] |> (*) 1000

            let Gold =
                CoinType.Gold, List.sum [ for _ in 1..6 -> random.Next(6) + 1 ] |> (*) 100

            let Platinum =
                CoinType.Platinum, List.sum [ for _ in 1..3 -> random.Next(6) + 1 ] |> (*) 10

            let art, magic =
                match random.Next(100) + 1 with
                | Range 1 4 -> None, []
                | Range 5 10 -> Some(roll 2 4, 25), []
                | Range 11 16 -> Some(roll 3 6, 50), []
                | Range 17 22 -> Some(roll 3 6, 100), []
                | Range 23 28 -> Some(roll 2 4, 25), []
                | Range 29 32 -> Some(roll 2 4, 25), RandomMagicTable.A 6 random
                | Range 33 36 -> Some(roll 3 6, 50), RandomMagicTable.A 6 random
                | Range 37 39 -> Some(roll 3 6, 100), RandomMagicTable.A 6 random
                | Range 41 44 -> Some(roll 2 4, 250), RandomMagicTable.A 6 random
                | Range 45 49 -> Some(roll 2 4, 25), RandomMagicTable.B 4 random
                | Range 50 54 -> Some(roll 3 6, 50), RandomMagicTable.B 4 random
                | Range 55 59 -> Some(roll 3 6, 100), RandomMagicTable.B 4 random
                | Range 60 63 -> Some(roll 2 4, 250), RandomMagicTable.B 4 random
                | Range 64 66 -> Some(roll 2 4, 25), RandomMagicTable.C 4 random
                | Range 67 69 -> Some(roll 3 6, 50), RandomMagicTable.C 4 random
                | Range 70 72 -> Some(roll 3 6, 100), RandomMagicTable.C 4 random
                | Range 73 74 -> Some(roll 2 4, 250), RandomMagicTable.C 4 random
                | Range 75 76 -> Some(roll 2 4, 25), RandomMagicTable.D 1 random
                | Range 77 78 -> Some(roll 3 6, 50), RandomMagicTable.D 1 random
                | Range 79 79 -> Some(roll 3 6, 100), RandomMagicTable.D 1 random
                | Range 80 80 -> Some(roll 2 4, 250), RandomMagicTable.D 1 random
                | Range 81 84 -> Some(roll 2 4, 25), RandomMagicTable.F 4 random
                | Range 85 88 -> Some(roll 3 6, 50), RandomMagicTable.F 4 random
                | Range 89 91 -> Some(roll 3 6, 100), RandomMagicTable.F 4 random
                | Range 92 94 -> Some(roll 2 4, 250), RandomMagicTable.F 4 random
                | Range 95 96 -> Some(roll 3 6, 100), RandomMagicTable.G 6 random
                | Range 97 98 -> Some(roll 2 4, 250), RandomMagicTable.G 6 random
                | Range 99 99 -> Some(roll 3 6, 100), RandomMagicTable.H 1 random
                | _ -> Some(roll 2 4, 250), RandomMagicTable.H 1 random

            [ Copper; Silver; Gold; Platinum ], art, magic
        | CR.Between 11u 16u ->
            let Gold =
                CoinType.Gold, List.sum [ for _ in 1..4 -> random.Next(6) + 1 ] |> (*) 1000

            let Platinum =
                CoinType.Platinum, List.sum [ for _ in 1..5 -> random.Next(6) + 1 ] |> (*) 100

            let art, magic =
                match random.Next(100) + 1 with
                | Range 1 3 -> None, []
                | Range 4 6 -> Some(roll 2 4, 250), []
                | Range 7 10 -> Some(roll 2 4, 750), []
                | Range 11 12 -> Some(roll 3 6, 500), []
                | Range 13 15 -> Some(roll 3 6, 1000), []
                | Range 16 19 ->
                    Some(roll 2 4, 250), RandomMagicTable.many [ RandomMagicTable.A 4; RandomMagicTable.B 6 ] random
                | Range 20 23 ->
                    Some(roll 2 4, 750), RandomMagicTable.many [ RandomMagicTable.A 4; RandomMagicTable.B 6 ] random
                | Range 24 26 ->
                    Some(roll 3 6, 500), RandomMagicTable.many [ RandomMagicTable.A 4; RandomMagicTable.B 6 ] random
                | Range 27 29 ->
                    Some(roll 3 6, 1000), RandomMagicTable.many [ RandomMagicTable.A 4; RandomMagicTable.B 6 ] random
                | Range 30 35 -> Some(roll 2 4, 250), RandomMagicTable.C 4 random
                | Range 36 40 -> Some(roll 2 4, 750), RandomMagicTable.C 6 random
                | Range 41 45 -> Some(roll 3 6, 500), RandomMagicTable.C 6 random
                | Range 46 50 -> Some(roll 3 6, 1000), RandomMagicTable.C 4 random
                | Range 51 54 -> Some(roll 2 4, 250), RandomMagicTable.D 4 random
                | Range 55 58 -> Some(roll 2 4, 750), RandomMagicTable.D 4 random
                | Range 59 62 -> Some(roll 3 6, 500), RandomMagicTable.D 4 random
                | Range 63 66 -> Some(roll 3 6, 1000), RandomMagicTable.D 4 random
                | Range 67 68 -> Some(roll 2 4, 250), RandomMagicTable.E 1 random
                | Range 69 70 -> Some(roll 2 4, 750), RandomMagicTable.E 1 random
                | Range 71 72 -> Some(roll 3 6, 500), RandomMagicTable.E 1 random
                | Range 73 74 -> Some(roll 3 6, 1000), RandomMagicTable.E 1 random
                | Range 75 76 ->
                    Some(roll 2 4, 250), RandomMagicTable.many [ RandomMagicTable.F 1; RandomMagicTable.G 4 ] random
                | Range 77 78 ->
                    Some(roll 2 4, 750), RandomMagicTable.many [ RandomMagicTable.F 1; RandomMagicTable.G 4 ] random
                | Range 79 80 ->
                    Some(roll 3 6, 500), RandomMagicTable.many [ RandomMagicTable.F 1; RandomMagicTable.G 4 ] random
                | Range 81 82 ->
                    Some(roll 3 6, 1000), RandomMagicTable.many [ RandomMagicTable.F 1; RandomMagicTable.G 4 ] random
                | Range 83 85 -> Some(roll 2 4, 250), RandomMagicTable.H 4 random
                | Range 86 88 -> Some(roll 2 4, 750), RandomMagicTable.H 4 random
                | Range 89 90 -> Some(roll 3 6, 500), RandomMagicTable.H 4 random
                | Range 91 92 -> Some(roll 3 6, 1000), RandomMagicTable.H 4 random
                | Range 93 94 -> Some(roll 2 4, 250), RandomMagicTable.I 1 random
                | Range 95 96 -> Some(roll 2 4, 750), RandomMagicTable.I 1 random
                | Range 97 98 -> Some(roll 3 6, 1000), RandomMagicTable.I 1 random
                | _ -> Some(roll 3 6, 1000), RandomMagicTable.I 1 random

            [ Gold; Platinum ], art, magic
        | _ ->
            let Gold =
                CoinType.Gold, List.sum [ for _ in 1..12 -> random.Next(6) + 1 ] |> (*) 1000

            let Platinum =
                CoinType.Platinum, List.sum [ for _ in 1..8 -> random.Next(6) + 1 ] |> (*) 1000

            let art, magic =
                match random.Next(100) + 1 with
                | Range 1 2 -> None, []
                | Range 3 5 -> Some(roll 3 6, 1000), RandomMagicTable.C 8 random
                | Range 6 8 -> Some(roll 1 10, 2500), RandomMagicTable.C 8 random
                | Range 9 11 -> Some(roll 1 4, 7500), RandomMagicTable.C 8 random
                | Range 12 14 -> Some(roll 1 8, 5000), RandomMagicTable.C 8 random
                | Range 15 22 -> Some(roll 3 6, 1000), RandomMagicTable.D 6 random
                | Range 23 30 -> Some(roll 1 10, 2500), RandomMagicTable.D 6 random
                | Range 31 38 -> Some(roll 1 4, 7500), RandomMagicTable.D 6 random
                | Range 39 46 -> Some(roll 1 8, 5000), RandomMagicTable.D 6 random
                | Range 47 52 -> Some(roll 3 6, 1000), RandomMagicTable.E 6 random
                | Range 53 58 -> Some(roll 1 10, 2500), RandomMagicTable.E 6 random
                | Range 59 63 -> Some(roll 1 4, 7500), RandomMagicTable.E 6 random
                | Range 64 68 -> Some(roll 1 8, 5000), RandomMagicTable.E 6 random
                | Range 69 69 -> Some(roll 3 6, 1000), RandomMagicTable.G 4 random
                | Range 70 70 -> Some(roll 1 10, 2500), RandomMagicTable.G 4 random
                | Range 71 71 -> Some(roll 1 4, 7500), RandomMagicTable.G 4 random
                | Range 72 72 -> Some(roll 1 8, 5000), RandomMagicTable.G 4 random
                | Range 73 74 -> Some(roll 3 6, 1000), RandomMagicTable.H 4 random
                | Range 75 76 -> Some(roll 1 10, 2500), RandomMagicTable.H 4 random
                | Range 77 78 -> Some(roll 1 4, 7500), RandomMagicTable.H 4 random
                | Range 79 80 -> Some(roll 1 8, 5000), RandomMagicTable.H 4 random
                | Range 81 85 -> Some(roll 3 6, 1000), RandomMagicTable.I 4 random
                | Range 86 90 -> Some(roll 1 10, 2500), RandomMagicTable.I 4 random
                | Range 91 95 -> Some(roll 1 4, 7500), RandomMagicTable.I 4 random
                | _ -> Some(roll 1 8, 5000), RandomMagicTable.I 4 random

            [ Gold; Platinum ], art, magic

    let rollKill (random: Random) kills =
        let roll = roll random

        let mapReduce map (key, value) =
            Map.change
                key
                (function
                | Some a -> Some <| a + value
                | None -> Some value)
                map

        let money =
            [ for mob, amount in kills do
                  for _ in 1u .. amount do
                      match mob.CR with
                      | CR.Between 0u 4u ->
                          match roll 1 100 with
                          | Range 1 30 -> yield (Copper, roll 5 6)
                          | Range 31 60 -> yield (Silver, roll 4 6)
                          | Range 61 70 -> yield (Electrum, roll 3 6)
                          | Range 71 95 -> yield (Gold, roll 3 6)
                          | _ -> yield (Platinum, roll 1 6)
                      | CR.Between 5u 10u ->
                          match roll 1 100 with
                          | Range 1 30 -> yield! [ (Copper, roll 5 6 * 100); (Electrum, roll 1 6 * 10) ]
                          | Range 31 60 -> yield! [ (Silver, roll 4 6 * 10); (Gold, roll 2 6 * 10) ]
                          | Range 61 70 -> yield! [ (Electrum, roll 3 6 * 10); (Gold, roll 2 6 * 10) ]
                          | Range 71 95 -> yield (Gold, roll 4 6 * 10)
                          | _ -> yield! [ (Gold, roll 2 6 * 10); (Platinum, roll 3 6) ]
                      | CR.Between 11u 16u ->
                          match roll 1 100 with
                          | Range 1 20 -> yield! [ (Silver, roll 4 6 * 100); (Gold, roll 1 6 * 100) ]
                          | Range 21 35 -> yield! [ (Electrum, roll 1 6 * 100); (Gold, roll 1 6 * 100) ]
                          | Range 36 75 -> yield! [ (Gold, roll 4 6 * 100); (Platinum, roll 1 6 * 10) ]
                          | _ -> yield! [ (Gold, roll 4 6 * 10); (Platinum, roll 2 6 * 10) ]
                      | _ ->
                          match roll 1 100 with
                          | Range 1 15 -> yield! [ (Electrum, roll 4 6 * 1000); (Gold, roll 8 6 * 100) ]
                          | Range 16 55 -> yield! [ (Gold, roll 1 6 * 1000); (Platinum, roll 1 6 * 100) ]
                          | _ -> yield! [ (Gold, roll 1 6 * 1000); (Platinum, roll 2 6 * 10) ] ]
            |> List.fold mapReduce (Map [])

        let meat =
            List.sum
                [ for mob, amount in kills ->
                      let amount = Int32.CreateChecked amount

                      match mob.Size with
                      | Tiny -> amount
                      | Small -> roll amount 4
                      | Medium -> roll (amount * 2) 6
                      | Large -> roll (amount * 6) 6
                      | Huge -> roll (amount * 8) 12
                      | Gargantuan -> roll (amount * 8) 20 ]

        let itemsReduce map (item: Item) =
            Map.change
                (item.Name, item.Size)
                (Option.map (fun conditions ->
                    Map.change
                        item.Condition
                        (Option.map ((+) item.Amount) >> Option.orElse (Some item.Amount))
                        conditions)
                 >> Option.orElse (Some(Map [ (item.Condition, item.Amount) ])))
                map

        let items =
            [ for mob, amount in kills do
                  for item in mob.Carries do
                      for _ in 1u .. amount do
                          let quality =
                              match roll 1 100 with
                              | Range 1 2 -> Superb
                              | Range 3 5 -> AlmostNew
                              | Range 6 55 -> Low
                              | Range 56 65 -> Terrible
                              | _ -> Broken

                          let count =
                              match item.Amount with
                              | Fixed n -> n
                              | Rolled(c, n) -> roll c n
                              | Compound(Dies = map; Static = n) ->
                                  Map.fold (fun acc die count -> acc + roll count die) n map

                          yield
                              { item with
                                  Amount = Fixed count
                                  Condition = quality
                                  Size = mob.Size } ]
            |> List.filter (fun item -> item.Condition <> Broken)
            |> List.fold itemsReduce (Map [])

        money, meat, items

    let parseRoot (root: LootTableSchema.Root) =
        Result.result {
            let name = root.Nome
            let! cr = CR.parse root.Cr |> Result.mapError (CRParseError >> tuple2 name)
            let! size = Size.parse root.Size |> Result.mapError (SizeParseError >> tuple2 name)

            let! creatureType =
                CreatureType.parse root.Type
                |> Result.mapError (CreatureTypeParseError >> tuple2 name)

            let! loot =
                Drop.parseList <| Seq.toList root.Loot
                |> Result.mapError (DropParseError >> tuple2 name)

            let! items =
                Item.parseList size <| Seq.toList root.Carrying
                |> Result.mapError (ItemParseError >> tuple2 name)

            return
                { Name = name
                  CR = cr
                  Size = size
                  CreatureType = creatureType
                  Loot = loot
                  Carries = items }
        }

    let load () =
        File.ReadAllText "C:\\projects\\DropCalculator\\data\\Drops.json"
        |> LootTableSchema.Parse
        |> Seq.toList
        |> List.map parseRoot
        |> Result.sequence
