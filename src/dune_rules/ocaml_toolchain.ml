open Import
open Memo.O

type t =
  { bin_dir : Path.t
  ; ocaml : Action.Prog.t
  ; ocamlc : Path.t
  ; ocamlopt : Action.Prog.t
  ; ocamldep : Action.Prog.t
  ; ocamlmklib : Action.Prog.t
  ; ocamlobjinfo : Action.Prog.t
  ; ocaml_config : Ocaml_config.t
  ; ocaml_config_vars : Ocaml_config.Vars.t
  ; version : Ocaml.Version.t
  }

let of_env_with_findlib name env findlib_config ~which =
  let not_found ?hint prog =
    Action.Prog.Not_found.create ?hint ~context:name ~program:prog ~loc:None ()
  in
  let get_tool_using_findlib_config prog =
    Memo.Option.bind findlib_config ~f:(Findlib.Config.tool ~prog)
  in
  let* ocamlc =
    let ocamlc = "ocamlc" in
    get_tool_using_findlib_config ocamlc >>= function
    | Some x -> Memo.return x
    | None -> (
      which ocamlc >>| function
      | Some x -> x
      | None -> not_found ocamlc |> Action.Prog.Not_found.raise)
  in
  let ocaml_bin = Path.parent_exn ocamlc in
  let get_ocaml_tool prog =
    get_tool_using_findlib_config prog >>= function
    | Some x -> Memo.return (Ok x)
    | None -> (
      Which.best_path ~dir:ocaml_bin prog >>| function
      | Some p -> Ok p
      | None ->
        let hint =
          sprintf "ocamlc found in %s, but %s/%s doesn't exist (context: %s)"
            (Path.to_string ocaml_bin) (Path.to_string ocaml_bin) prog
            (Context_name.to_string name)
        in
        Error (not_found ~hint prog))
  in
  let* ocaml_config_vars, ocfg =
    let+ vars =
      Process.run_capture_lines ~display:Quiet ~env Strict ocamlc [ "-config" ]
      |> Memo.of_reproducible_fiber >>| Ocaml_config.Vars.of_lines
    in
    match
      match vars with
      | Error msg -> Error (Ocaml_config.Origin.Ocamlc_config, msg)
      | Ok vars ->
        let open Result.O in
        let+ ocfg = Ocaml_config.make vars in
        (vars, ocfg)
    with
    | Ok x -> x
    | Error (Ocaml_config.Origin.Makefile_config file, msg) ->
      User_error.raise ~loc:(Loc.in_file file) [ Pp.text msg ]
    | Error (Ocamlc_config, msg) ->
      User_error.raise
        [ Pp.textf "Failed to parse the output of '%s -config':"
            (Path.to_string ocamlc)
        ; Pp.text msg
        ]
  and* ocamlopt = get_ocaml_tool "ocamlopt"
  and* ocaml = get_ocaml_tool "ocaml"
  and* ocamldep = get_ocaml_tool "ocamldep"
  and* ocamlmklib = get_ocaml_tool "ocamlmklib"
  and* ocamlobjinfo = get_ocaml_tool "ocamlobjinfo" in
  let version = Ocaml.Version.of_ocaml_config ocfg in
  Memo.return
    { bin_dir = ocaml_bin
    ; ocaml
    ; ocamlc
    ; ocamlopt
    ; ocamldep
    ; ocamlmklib
    ; ocamlobjinfo
    ; ocaml_config = ocfg
    ; ocaml_config_vars
    ; version
    }

let compiler t (mode : Ocaml.Mode.t) =
  match mode with
  | Byte -> Ok t.ocamlc
  | Native -> t.ocamlopt

let best_mode t : Mode.t =
  match t.ocamlopt with
  | Ok _ -> Native
  | Error _ -> Byte
