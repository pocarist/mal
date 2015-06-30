namespace Mal

open System
open Types

type Printer() =
    static member pr_str exp =
        let rec f = function
            | Number x -> string x
            | List xs -> "(" + (List.map f xs |> String.concat " ") + ")"
            | Symbol x -> x
            | Lambda x -> sprintf "#<fun:%x>" <| x.GetHashCode() 
            | String x -> "\"" + x + "\""
            | Keyword x -> ":" + x
            | Vector xs -> "[" + (Array.map f xs |> String.concat " ") + "]"
            | Hash xs -> "{" + (Map.fold (fun s k v -> (f k+" "+f v) :: s) [] xs |> String.concat " ") + "}"
            | Atom x -> "(atom " + f x + ")"
            | Nil -> "nil"
            | Bool x -> if x then "true" else "false"
        f exp
