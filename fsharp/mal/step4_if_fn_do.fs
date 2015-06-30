(* Step 0: The REPL *)
open System

open Mal

let repl_env =
    new Env(None, ref Core.ns)

(* stub *)
let read str = Reader.read_str str
let print exp = Printer.pr_str exp " " true

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
    | Types.List (Types.Symbol "do" :: rest) ->
        let rec loop ret = function
            | ast :: rest -> loop (eval ast env) rest
            | [] -> ret
        loop Types.Nil rest
    | Types.List (Types.Symbol "if" :: cond :: t_expr :: f_expr :: []) ->
        match eval cond env with
        | Types.Nil
        | Types.Bool(false) -> eval f_expr env
        | _ -> eval t_expr env
    | Types.List (Types.Symbol "if" :: cond :: t_expr :: []) ->
        match eval cond env with
        | Types.Nil
        | Types.Bool(false) -> Types.Nil
        | _ -> eval t_expr env
    | Types.List (Types.Symbol "fn*" :: Types.Vector(binds) :: body :: []) ->
        let fn args =
            eval body (new Env(List.ofArray binds, args, env))
        Types.Lambda ({f=fn})
    | Types.List (Types.Symbol "fn*" :: Types.List(binds) :: body :: []) ->
        let fn args =
            eval body (new Env(binds, args, env))
        Types.Lambda ({f=fn})
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
    match f with
    | Types.Lambda ({f=fn}) -> fn args
    | _ -> failwith "apply"
and bind env = function
    | sym :: expr :: rest ->
        env.set sym (eval expr env)
        bind env rest
    | _ -> ()

let rep str = print (eval (read str) repl_env)

let read_line () =
    let line = Console.ReadLine ()
    if line = null then
        raise (new IO.EndOfStreamException())
    else line

[<EntryPoint>]
let main argv =
    rep("(def! not (fn* (a) (if a false true)))") |> ignore
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
