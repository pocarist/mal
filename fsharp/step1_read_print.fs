(* Step 0: The REPL *)
open System

open Mal

(* stub *)
let read str = Reader.read_str str
let eval ast env = ast
let print exp = Printer.pr_str exp

let read_line () =
    let line = Console.ReadLine ()
    if line = null then
        raise (new IO.EndOfStreamException())
    else line

let rep x =
    x
    |> read
    |> fun x -> eval x ""
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
