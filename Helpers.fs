module DropCalculator.Helpers

open FParsec
open FSharpx

let (|=>) a p = a |>> fun _ -> p

let ws = spaces

let num: Parser<int, 'a> = pint32

let parse parser =
    run parser
    >> function
        | Failure(msg, _err, _) -> Result.Error msg
        | Success(result, _, _) -> Result.Ok result

let parseMany parser =
    List.map (parse parser) >> Result.sequence
