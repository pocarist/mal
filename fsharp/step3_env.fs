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
        Types.Symbol "+", make_lambda (num_func ( + ))
        Types.Symbol "-", make_lambda (num_func ( - ))
        Types.Symbol "*", make_lambda (num_func ( * ))
        Types.Symbol "/", make_lambda (num_func ( / ))
    ] 
    |> List.fold (fun (env:Env) (k,v) -> env.set k v; env) (new Env())

(* stub *)
let read str = Reader.read_str str
let print exp = Printer.pr_str exp

let rec eval ast (env : Env) =
    match ast with
    | Types.List (Types.Symbol "def!" :: ast1 :: ast2 :: []) ->
        let value = eval ast2 env
        env.set ast1 value
        value
    | Types.List (Types.Symbol "let*" :: Types.Vector(bindings) :: body :: []) ->
        let let_env = new Env(env)
        bind let_env (List.ofArray bindings)
        eval body let_env
    | Types.List (Types.Symbol "let*" :: Types.List(bindings) :: body :: []) ->
        let let_env = new Env(env)
        bind let_env bindings 
        eval body let_env
    | Types.List _ ->
        match eval_ast ast env with
        | Types.List (f::args) -> apply f args
        | _ -> failwith "eval1"
    | _ -> eval_ast ast env
and eval_ast ast env =
    match ast with
    | Types.Symbol x -> 
        try env.get ast with
        | _ -> failwith <| "'" + x + "' not found"
    | Types.List xs -> Types.List(List.map (fun x -> eval x env) xs)
    | Types.Vector xs -> Types.Vector(Array.map (fun x -> eval x env) xs)
    | Types.Hash xs -> Types.Hash(Map.map (fun _ v -> eval v env) xs)
    | _ -> ast
and apply f args =
    match f, args with
    | Types.Lambda {f=f'}, _ -> f' args
    | _ -> failwith "apply"
and bind env = function
    | sym :: expr :: rest ->
        env.set sym (eval expr env)
        bind env rest
    | _ -> ()

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
    let loop = ref true
    while !loop do
        try
            printf "user> "
            printfn "%s" (rep (read_line ()))
        with 
            | :? IO.EndOfStreamException ->
                loop := false //normal exit
            | e ->
                printfn "Error: %s" <| e.ToString()
    done
    0