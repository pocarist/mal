namespace Mal

type Env(outer : Env option, data) =
    let outer = outer
    let data = data

    new(?outer : Env) =
        new Env(outer, ref Map.empty)

    new(binds, exprs, ?outer : Env) =
        let rec loop m = function
            | Types.Symbol bx :: bs, e :: es -> loop (Map.add bx e m) (bs, es)
            | Types.Symbol "&" :: Types.Symbol bx :: [], es -> Map.add bx (Types.List es) m
            | _ -> m
        let data = loop Map.empty (binds, exprs)
        new Env(outer, ref data)

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
