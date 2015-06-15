(* Step 0: The REPL *)
open System

(* stub *)
let read x = x
let eval x = x
let print x = x

let rep x =
    x
    |> read
    |> eval
    |> print

[<EntryPoint>]
let main argv =
    let read_line () =
        let line = Console.ReadLine ()
        if line = null then
            raise (new IO.EndOfStreamException())
        else line
    let rec loop prompt =
        printf "%s" prompt
        printfn "%s" (rep (read_line ()))
        loop prompt
    try
        loop "user> "
    with :? IO.EndOfStreamException -> ()
    0
