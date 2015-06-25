(* Step 0: The REPL *)
open System

open Mal

<<<<<<< HEAD
let repl_env =
    [
        "+", (+)
        "-", (-)
        "*", ( * )
        "/", (/)
=======
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
>>>>>>> 3cefb9980f7a101e806e4c6cc05eeb8a5927fdbf
    ] 
    |> Map.ofList

let lookup env key = Map.find key env

(* stub *)
let read str = Reader.read_str str
let print exp = Printer.pr_str exp

<<<<<<< HEAD
(*
eval_ast(ast,env):
  switch type(ast):
    symbol:      return lookup(env, ast) OR raise "'" + ast + "' not found"
    list,vector: return ast.map((x) -> EVAL(x,env))
    hash:        return ast.map((k,v) -> list(k, EVAL(v,env)))
    _default_:   return ast

EVAL(ast,env):
    if not list?(ast): return eval_ast(ast, env)
    f, args = eval_ast(ast, env)
    return apply(f, args)
*)

let rec eval ast env = ast
and eval_ast ast env =
    match ast with
    | Types.Symbol _ -> 
        try lookup env ast with
        | _ -> failwith <| "'" + print ast + "' not found"
    | Types.List xs -> Types.List(List.map (fun x -> eval x env) xs)
    | Types.Vector xs -> Types.Vector(Array.map (fun x -> eval x env) xs)
    | Types.Hash xs -> Types.Hash(Map.map (fun k v -> Types.List([k; eval v env])) xs)
    | _ -> ast
=======
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
    | Types.Hash xs -> Types.Hash(Map.map (fun _ v -> eval v env) xs)
    | _ -> ast
and apply f args =
    match f, args with
    | Types.Lambda {f=f'}, _ -> f' args
    | _ -> failwith "apply"
>>>>>>> 3cefb9980f7a101e806e4c6cc05eeb8a5927fdbf

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
<<<<<<< HEAD
    let rec loop prompt =
        printf "%s" prompt
        printfn "%s" (rep (read_line ()))
        loop prompt
    try
        loop "user> "
    with 
        | :? IO.EndOfStreamException -> () //normal exit
        | _ as e -> printfn "Error: %s" <| e.ToString()
=======
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
>>>>>>> 3cefb9980f7a101e806e4c6cc05eeb8a5927fdbf
    0
