namespace DropCalculator

type DropType =
    | BodyPart
    | Bones
    | Feather
    | Ears
    | Hair
    | Head
    | HidePelt
    | Liquid
    | Poisons
    | MarksTattoos
    | Wings

[<RequireQualifiedAccess>]
module DropType =
    type ParseError =
        | InvalidDropType of string

        override this.ToString() =
            let (InvalidDropType dropType) = this
            $"DropType: %s{dropType} is a invalid drop type"

    let parse =
        function
        | "Body Part" -> Ok BodyPart
        | "Bones" -> Ok Bones
        | "Feather" -> Ok Feather
        | "Ears" -> Ok Ears
        | "Hair" -> Ok Hair
        | "Head" -> Ok Head
        | "Hide/Pelt" -> Ok HidePelt
        | "Liquid" -> Ok Liquid
        | "Poisons" -> Ok Poisons
        | "Marks/Tattoos" -> Ok MarksTattoos
        | "Wings" -> Ok Wings
        | s -> Error <| InvalidDropType s
