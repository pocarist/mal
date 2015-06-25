(* Step 0: The REPL *)
open System

open Mal

let num_func f xs =
    xs 
    |> List.map (function Types.Number x -> x | _ -> failwith "num_func args") 
    |> List.reduce f
    |> Types.Number

let make_lambda f =
    Types.Lambda ({f=f})

let repl_env =
    [
        "+", make_lambda (num_func ( + ))
        "-", make_lambda (num_func ( - ))
        "*", make_lambda (num_func ( * ))
        "/", make_lambda (num_func ( / ))
    ] 
    |> Map.ofList

let lookup env key = Map.find key env

(* stub *)
let read str = Reader.read_str str
let print exp = Printer.pr_str exp

let rec eval ast env =
    match ast with
    | Types.List _ ->
        match eval_ast ast env with
        | Types.List (f::args) -> apply f args
        | _ -> failwith "eval1"
    | _ -> eval_ast ast env
and eval_ast ast env =
    match ast with
    | Types.Symbol x -> 
        try lookup env x with
        | _ -> failwith <| "'" + x + "' not found"
    | Types.List xs -> Types.List(List.map (fun x -> eval x env) xs)
    | Types.Vector xs -> Types.Vector(Array.map (fun x -> eval x env) xs)
    | Types.Hash xs -> Types.Hash(Map.map (fun k v -> Types.List([k; eval v env])) xs)
    | _ -> ast
and apply f args =
    match f, args with
    | Types.Lambda {f=f'}, _ -> f' args
    | _ -> failwith "apply"

let read_line () =
    let line = Console.ReadLine ()
    if line = null then
        raise (new IO.EndOfStreamException())
    else line

let rep x =
    x
    |> read
    |> fun x -> eval x repl_env
    |> print

[<EntryPoint>]
let main argv =
    let rec loop prompt =
        printf "%s" prompt
        printfn "%s" (rep (read_line ()))
        loop prompt
    try
        loop "user> "
    with 
        | :? IO.EndOfStreamException -> () //normal exit
        | _ as e -> printfn "Error: %s" <| e.ToString()
    0
