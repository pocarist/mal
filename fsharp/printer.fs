namespace Mal

open System
open Types

type Printer() =
    static member pr_str ast =
        let rec f acc = function
            | Number x -> string x
            | List xs -> acc + "(" + (List.map (f "") xs |> String.concat " ") + ")"
            | Symbol x -> x
        f "" ast
