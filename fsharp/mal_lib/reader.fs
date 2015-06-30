namespace Mal

open System
open System.Text.RegularExpressions

type Reader(token_array : string array) =
    let tokens = 
        List.ofArray token_array
        |> List.filter ((<>) "")
        |> fun x -> ref x
    let peek () =
        match !tokens with
        | [] -> None
        | x :: _ -> Some x
    let next () = 
        match !tokens with
        | [] -> ()
        | _ :: xs -> tokens := xs

    let rec read_form () =
        match peek () with
        | None -> failwith "read_form : None" 
        | Some "(" -> next(); read_list ()
        | Some "[" -> next(); read_vector ()
        | Some "{" -> next(); read_hash ()
        | Some x -> read_atom ()
    and read_list () =
        let rec loop acc =
            match peek () with
            | None -> failwith "expected ')', got EOF"
            | Some ")" -> next (); Types.List(List.rev acc)
            | Some x -> loop (read_form () :: acc)
        loop []
    and read_vector () =
        let rec loop acc =
            match peek () with
            | None -> failwith "expected ']', got EOF"
            | Some "]" -> next (); Types.Vector(List.rev acc|>Array.ofList)
            | Some x -> loop (read_form () :: acc)
        loop []
    and read_hash () =
        let rec loop acc =
            match peek () with
            | None -> failwith "expected '}', got EOF"
            | Some "}" ->
                next ()
                let rec loop2 acc2 = function
                    | v :: k :: rest -> loop2 ((k,v)::acc2) rest
                    | _ -> Types.Hash(Map.ofList acc2)
                loop2 [] acc
            | Some x -> loop (read_form () :: acc)
        loop []
    and read_atom () =
        match peek () with
        | None -> failwith "read_atom : None"
        | Some x when '0' <= x.[0] && x.[0] <= '9' ->
            next (); Types.Number (int x)
        | Some x when '"' = x.[0] ->
            if x.Length = 1 then failwith "expected '\"', got EOF"
            next (); Types.String (x.Trim([|'"'|]))
        | Some x when ':' = x.[0] ->
            next (); Types.Keyword (x.Trim([|':'|]))
        | Some x when "'" = x ->
            next (); Types.List ([Types.Symbol "quote"; read_form ()]) 
        | Some x when "`" = x ->
            next (); Types.List ([Types.Symbol "quasiquote"; read_form ()]) 
        | Some x when "~" = x ->
            next (); Types.List ([Types.Symbol "unquote"; read_form ()]) 
        | Some x when "~@" = x ->
            next (); Types.List ([Types.Symbol "splice-unquote"; read_form ()]) 
        | Some x when "@" = x ->
            next (); Types.List ([Types.Symbol "deref"; read_form ()]) 
        | Some x when '^' = x.[0] ->
            next ()
            let meta = read_form ()
            let form = read_form ()
            Types.List ([Types.Symbol "with-meta"; form; meta])
        | Some "nil" ->
            next (); Types.Nil
        | Some "true" ->
            next (); Types.Bool true
        | Some "false" ->
            next (); Types.Bool false
        | Some x ->
            next (); Types.Symbol x

    static let pattern = """[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"|;.*|[^\s\[\]{}('"`,;)]*)"""
    static let tokenize input =
        let tokens = Regex.Split(input, pattern)
        new Reader(tokens)

    member this.SEexp with get() = read_form ()

    static member read_str str =
        let r = tokenize str
        r.SEexp