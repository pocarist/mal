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
            "pr-str", make_lambda (Types.String << fun xs -> Printer.pr_str (Types.List xs, " ", true))
            "str", make_lambda (Types.String << fun xs -> Printer.pr_str (Types.List xs, "", false))
            "prn", make_lambda (fun xs -> Printer.pr_str (Types.List xs, " ", true) |> Printf.printfn "%s"; Types.Nil)
            "println", make_lambda (fun xs -> Printer.pr_str (Types.List xs, " ", false) |> Printf.printfn "%s"; Types.Nil)
        ]
        |> Map.ofList