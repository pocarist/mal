namespace Mal

type Env(?outer : Env) =
    let outer = outer
    let data = ref Map.empty

    member this.set sym value =
        match sym with
        | Types.Symbol x ->
            data := Map.add x value !data
        | _ -> failwith "set requires a Symbol for its key"

    member this.find sym =
        match sym with
        | Types.Symbol x ->
            if Map.exists (fun k _ -> k=x) !data then
                Some this
            else
                match outer with
                | Some env -> env.find sym
                | None -> None                
        | _ -> failwith "find requires a Symbol for its key"

    member this.get sym : Types.t =
        match sym with
        | Types.Symbol x ->
            match Map.tryFind x !data with
            | Some v -> v
            | None ->
                match outer with
                | Some env -> env.get sym
                | None -> failwith ("'" + x + "' not found")                
        | _ -> failwith "get requires a Symbol for its key"
