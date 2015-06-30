namespace Mal

module Core =
    let fun_num f xs =
        xs 
        |> List.map (function Types.Number x -> x | _ -> failwith "num_func args") 
        |> List.reduce f
        |> Types.Number

    let make_lambda fn =
        Types.Lambda ({f=fn})

    let ns =
        [
            "+", make_lambda (fun_num ( + ))
            "-", make_lambda (fun_num ( - ))
            "*", make_lambda (fun_num ( * ))
            "/", make_lambda (fun_num ( / ))
            "list", make_lambda (Types.List)
            "list?", make_lambda (Types.Bool << function Types.List _ :: _ -> true | _ -> false)
            "empty?", make_lambda (Types.Bool << function Types.List ([]) :: _ -> true | _ -> false)
            "count", make_lambda (Types.Number << function Types.List xs :: _ -> xs.Length | _ -> 0)
            "=", make_lambda (Types.Bool << function a :: b :: [] -> a=b | _ -> false)
            "<", make_lambda (Types.Bool << function Types.Number a :: Types.Number b :: [] -> a<b | _ -> false)
            "<=", make_lambda (Types.Bool << function Types.Number a :: Types.Number b :: [] -> a<=b | _ -> false)
            ">", make_lambda (Types.Bool << function Types.Number a :: Types.Number b :: [] -> a>b | _ -> false)
            ">=", make_lambda (Types.Bool << function Types.Number a :: Types.Number b :: [] -> a>=b | _ -> false)
            "pr-str", make_lambda (Types.String << fun xs -> xs |> List.map (fun x -> Printer.pr_str(x, true)) |> String.concat " " )
            "str", make_lambda (Types.String << fun xs -> xs |> List.map (fun x -> Printer.pr_str(x, false)) |> String.concat "" )
            "prn", make_lambda (fun xs -> xs |> List.map (fun x -> Printer.pr_str(x, true)) |> String.concat " " |> Printf.printfn "%s"; Types.Nil)
            "println", make_lambda (fun xs -> xs |> List.map (fun x -> Printer.pr_str(x, false)) |> String.concat " " |> Printf.printfn "%s"; Types.Nil)
        ]
        |> Map.ofList