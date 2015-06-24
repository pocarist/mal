namespace Mal

open System
open Types

type Printer() =
    static member pr_str exp =
        let rec f = function
            | Number x -> string x
            | List xs -> "(" + (List.map f xs |> String.concat " ") + ")"
            | Symbol x -> x
            | Lambda x -> sprintf "#<fun:%d>" <| x.GetHashCode() 
            | Keyword x -> ":" + x
            | Vector xs -> "[" + (Array.map f xs |> String.concat " ") + "]"
            | Hash xs -> "{" + (Map.fold (fun s k v -> s + "(" + f k + " " + f v + ")") "" xs) + "}"
            | Atom x -> "(atom " + f x + ")"
        f exp
