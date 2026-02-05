open Import

let make_action = function
  | [] -> None
  | [ action ] -> Some action
  | actions -> Some (Dune_lang.Action.Progn actions)
;;

let build_env opam_file =
  OpamFile.OPAM.build_env opam_file
  |> List.map ~f:(fun (var, op, value, _) ->
    { Dune_lang.Action.Env_update.op
    ; var
    ; value = String_with_vars.make_text Loc.none value
    })
;;

let wrap_build_env opam_file action =
  match build_env opam_file with
  | [] -> action
  | env -> Dune_lang.Action.Withenv (env, action)
;;
