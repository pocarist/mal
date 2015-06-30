namespace Mal

open System
open Types

type Printer() =
    static member pr_str (exp, ?sep, ?print_readably) =
        let sep = defaultArg sep " "
        let print_readably = defaultArg print_readably false
        let escape (x : string) = x.Replace("\\", "\\\\").Replace("\"", "\\\"").Replace("\n", "\\n")
        let rec f = function
            | Number x -> string x
            | List xs -> "(" + (List.map f xs |> String.concat sep) + ")"
            | Symbol x -> x
            | Lambda x -> sprintf "#<fun:%x>" <| x.GetHashCode() 
            | String x ->
                if print_readably then "\"" + escape x + "\""
                else "\"" + x + "\""
            | Keyword x -> ":" + x
            | Vector xs -> "[" + (Array.map f xs |> String.concat sep) + "]"
            | Hash xs -> "{" + (Map.fold (fun s k v -> (f k+" "+f v) :: s) [] xs |> String.concat sep) + "}"
            | Atom x -> "(atom " + f x + ")"
            | Nil -> "nil"
            | Bool x -> if x then "true" else "false"
        f exp
