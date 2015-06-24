(* Step 0: The REPL *)
open System

open Mal

(* stub *)
let read x = Reader.read_str x
let eval x = x
let print x = Printer.pr_str x

let read_line () =
    let line = Console.ReadLine ()
    if line = null then
        raise (new IO.EndOfStreamException())
    else line

let rep x =
    x
    |> read
    |> eval
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
        | _ as e -> eprintfn "%s" <| e.ToString()
    0
