open Import

let to_actions get_solver_var loc package (commands : OpamTypes.command list) =
  let open Result.O in
  List.map commands ~f:(fun (args, filter) ->
    let filter = Option.map filter ~f:(Filter.simplify get_solver_var) in
    match Filter.partial_eval filter with
    | `Skip -> Ok None
    | `Filter filter ->
      let* terms =
        List.filter_map args ~f:(fun ((simple_arg : OpamTypes.simple_arg), filter) ->
          let filter = Option.map filter ~f:(Filter.simplify get_solver_var) in
          match Filter.partial_eval filter with
          | `Skip -> None
          | `Filter filter ->
            let slang =
              let+ slang =
                match simple_arg with
                | CString s -> Filter.opam_string_to_slang ~package ~loc s
                | CIdent ident -> Filter.opam_raw_fident_to_slang ~loc ident
              in
              Slang.simplify slang
            in
            Some
              (let+ slang =
                 match filter with
                 | None -> slang
                 | Some filter ->
                   let+ filter_blang =
                     Filter.to_blang ~package ~loc filter >>| Slang.simplify_blang
                   and+ slang = slang in
                   let filter_blang_handling_undefined =
                     (* Wrap the blang filter so that if any undefined
                        variables are expanded while evaluating the filter,
                        the filter will return false. *)
                     let slang =
                       Slang.catch_undefined_var
                         (Slang.blang filter_blang)
                         ~fallback:(Slang.bool false)
                     in
                     Blang.Expr slang
                   in
                   Slang.when_ filter_blang_handling_undefined slang
               in
               Slang.simplify slang))
        |> Result.List.all
      in
      if List.is_empty terms
      then Ok None
      else
        let+ action =
          let action = Action.Run terms in
          match filter with
          | None -> Ok action
          | Some filter ->
            let+ condition =
              Filter.to_blang ~package ~loc filter >>| Slang.simplify_blang
            in
            Action.When (condition, action)
        in
        Some action)
  |> Result.List.all
  |> Result.map ~f:List.filter_opt
;;
