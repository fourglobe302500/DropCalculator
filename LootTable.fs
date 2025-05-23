namespace DropCalculator

open System.IO
open FParsec
open FSharpx

type LootTable =
    { Name: string
      CR: CR
      Size: Size
      CreatureType: CreatureType
      Loot: Drop list }

module LootTable =
    let name lootTable = lootTable.Name

    type ParseError =
        | SizeParseError of Size.ParseError
        | CreatureTypeParseError of CreatureType.ParseError
        | DropParseError of Drop.ParseError
        | InvalidName of string
        | CRParseError of CR.ParseError

        override this.ToString() =
            match this with
            | DropParseError err -> $"LootTable->{err}"
            | SizeParseError err -> $"LootTable->{err}"
            | CreatureTypeParseError err -> $"LootTable->{err}"
            | CRParseError err -> $"LootTable->{err}"
            | InvalidName s ->
                $"LootTable: Invalid name '{s}' it's not on the list of creatures. Use 'list' to see valid names."

    let nameParser lootTable =
        [ yield! lootTable |> List.map (fun m -> pstring (m.Name.ToLower()) >>% Result.Ok m)
          yield many1Chars anyChar |>> InvalidName |>> Result.Error ]
        |> choice

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

            return
                { Name = name
                  CR = cr
                  Size = size
                  CreatureType = creatureType
                  Loot = loot }
        }

    let load () =
        File.ReadAllText "C:\\projects\\DropCalculator\\data\\Drops.json"
        |> LootTableSchema.Parse
        |> Seq.toList
        |> List.map parseRoot
        |> Result.sequence
