(* Step 0: The REPL *)
open System

open Mal

let choose_num = function
    | Types.Number x -> Some x
    | _ -> None

let num_func f xs =
    xs 
    |> Array.choose choose_num 
    |> Array.reduce f
    |> fun x -> Types.Number x

let repl_env =
    [
        "+", num_func (+)
        "-", num_func (-)
        "*", num_func ( * )
        "/", num_func (/)
    ] 
    |> Map.ofList

let lookup env key = Map.find key env

(* stub *)
let read str = Reader.read_str str
let print exp = Printer.pr_str exp

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
    | Types.Symbol x -> 
        try lookup env x with
        | _ -> failwith <| "'" + x + "' not found"
    | Types.List xs -> Types.List(List.map (fun x -> eval x env) xs)
    | Types.Vector xs -> Types.Vector(Array.map (fun x -> eval x env) xs)
    | Types.Hash xs -> Types.Hash(Map.map (fun k v -> Types.List([k; eval v env])) xs)
    | _ -> ast

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
