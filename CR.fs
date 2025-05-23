namespace DropCalculator

open System

type CR =
    | Half
    | Quarter
    | Whole of uint

    override this.ToString() =
        match this with
        | Half -> "1/2"
        | Quarter -> "1/4"
        | Whole s -> s.ToString()

[<RequireQualifiedAccess>]
module CR =
    type ParseError =
        | InvalidCR of string

        override this.ToString() =
            let (InvalidCR cr) = this
            $"CR: '{cr}' is a invalid cr"

    let parse =
        function
        | "1/4" -> Ok Quarter
        | "1/2" -> Ok Half
        | n ->
            try
                UInt32.Parse n |> Whole |> Ok
            with _ ->
                n |> InvalidCR |> Error
