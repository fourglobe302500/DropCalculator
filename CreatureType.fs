namespace DropCalculator

type CreatureType =
    | Humanoids
    | Monstrosities
    | Dragons
    | Giants
    | Undead
    | Aberrations
    | Fiends
    | Celestials
    | Fey
    | Elementals
    | Constructs
    | Oozes
    | Plants
    | Beasts

[<RequireQualifiedAccess>]
module CreatureType =
    type ParseError =
        | InvalidCreatureType of string

        override this.ToString() =
            let (InvalidCreatureType creatureType) = this
            $"CreatureType: %s{creatureType} is a invalid creature type"

    let parse =
        function
        | "humanoids" -> Ok Humanoids
        | "monstrosities" -> Ok Monstrosities
        | "dragons" -> Ok Dragons
        | "giants" -> Ok Giants
        | "undead" -> Ok Undead
        | "aberrations" -> Ok Aberrations
        | "fiends" -> Ok Fiends
        | "celestials" -> Ok Celestials
        | "fey" -> Ok Fey
        | "elementals" -> Ok Elementals
        | "constructs" -> Ok Constructs
        | "oozes" -> Ok Oozes
        | "plants" -> Ok Plants
        | "beasts" -> Ok Beasts
        | s -> Error(InvalidCreatureType s)
