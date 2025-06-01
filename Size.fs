namespace DropCalculator

type Size =
    | Tiny
    | Small
    | Medium
    | Large
    | Huge
    | Gargantuan

[<RequireQualifiedAccess>]
module Size =
    type ParseError =
        | InvalidSize of string

        override this.ToString() =
            let (InvalidSize size) = this
            $"Size: %s{size} is a invalid size"

    let parse =
        function
        | "tiny" -> Ok(Tiny)
        | "small" -> Ok(Small)
        | "medium" -> Ok(Medium)
        | "large" -> Ok(Large)
        | "huge" -> Ok(Huge)
        | "gargantuan" -> Ok(Gargantuan)
        | s -> Error <| InvalidSize s
