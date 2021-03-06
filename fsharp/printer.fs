namespace Mal

open System
open Types

type Printer() =
    static member pr_str exp =
        let rec f = function
            | Number x -> string x
            | List xs -> "(" + (List.map f xs |> String.concat " ") + ")"
            | Symbol x -> x
        f exp
