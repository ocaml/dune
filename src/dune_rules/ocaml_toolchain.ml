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
  ; builtins : Meta.Simplified.t Package.Name.Map.t Memo.t
  ; lib_config : Lib_config.t
  }

let make_builtins ~ocaml_config ~version =
  Memo.Lazy.create (fun () ->
    let stdlib_dir = Path.of_string (Ocaml_config.standard_library ocaml_config) in
    Meta.builtins ~stdlib_dir ~version)
;;

let make_ocaml_vars_and_config ~env ~ocamlc =
  let+ vars =
    Process.run_capture_lines ~display:Quiet ~env Strict ocamlc [ "-config" ]
    |> Memo.of_reproducible_fiber
    >>| Ocaml_config.Vars.of_lines
  in
  match
    match vars with
    | Error msg -> Error (Ocaml_config.Origin.Ocamlc_config, msg)
    | Ok vars ->
      let open Result.O in
      let+ ocfg = Ocaml_config.make vars in
      vars, ocfg
  with
  | Ok x -> x
  | Error (Ocaml_config.Origin.Makefile_config file, msg) ->
    User_error.raise ~loc:(Loc.in_file file) [ Pp.text msg ]
  | Error (Ocamlc_config, msg) ->
    User_error.raise
      [ Pp.textf "Failed to parse the output of '%s -config':" (Path.to_string ocamlc)
      ; Pp.text msg
      ]
;;

let compiler t (mode : Ocaml.Mode.t) =
  match mode with
  | Byte -> Ok t.ocamlc
  | Native -> t.ocamlopt
;;

let best_mode t : Mode.t =
  match t.ocamlopt with
  | Ok _ -> Native
  | Error _ -> Byte
;;

let make name ~which ~env ~get_ocaml_tool =
  let not_found ?hint program =
    Action.Prog.Not_found.create ?hint ~context:name ~loc:None ~program ()
  in
  let* ocamlc =
    let program = "ocamlc" in
    which program
    >>| function
    | Some x -> x
    | None -> not_found program |> Action.Prog.Not_found.raise
  in
  let ocaml_bin = Path.parent_exn ocamlc in
  let get_ocaml_tool prog =
    get_ocaml_tool ~dir:ocaml_bin prog
    >>| function
    | Some prog -> Ok prog
    | None ->
      let hint =
        sprintf
          "ocamlc found in %s, but %s/%s doesn't exist (context: %s)"
          (Path.to_string ocaml_bin)
          (Path.to_string ocaml_bin)
          prog
          (Context_name.to_string name)
      in
      Error (not_found ~hint prog)
  in
  let* ocaml_config_vars, ocaml_config = make_ocaml_vars_and_config ~env ~ocamlc in
  let* ocamlopt = get_ocaml_tool "ocamlopt"
  and* ocaml = get_ocaml_tool "ocaml"
  and* ocamldep = get_ocaml_tool "ocamldep"
  and* ocamlmklib = get_ocaml_tool "ocamlmklib"
  and* ocamlobjinfo = get_ocaml_tool "ocamlobjinfo" in
  let version = Ocaml.Version.of_ocaml_config ocaml_config in
  let builtins = make_builtins ~version ~ocaml_config in
  Memo.return
    { bin_dir = ocaml_bin
    ; ocaml
    ; ocamlc
    ; ocamlopt
    ; ocamldep
    ; ocamlmklib
    ; ocamlobjinfo
    ; ocaml_config
    ; ocaml_config_vars
    ; version
    ; builtins = Memo.Lazy.force builtins
    ; lib_config = Lib_config.create ocaml_config ~ocamlopt
    }
;;

let of_env_with_findlib name env findlib_config ~which =
  let get_tool_using_findlib_config prog =
    Memo.Option.bind findlib_config ~f:(Findlib_config.tool ~prog)
  in
  let which program =
    get_tool_using_findlib_config program
    >>= function
    | Some x -> Memo.return (Some x)
    | None -> which program
  in
  let get_ocaml_tool ~dir prog =
    get_tool_using_findlib_config prog
    >>= function
    | Some x -> Memo.return (Some x)
    | None -> Which.best_in_dir ~dir prog
  in
  make name ~env ~get_ocaml_tool ~which
;;

let of_binaries ~path name env binaries =
  let which =
    let map =
      Path.Set.to_list binaries
      |> Filename.Map.of_list_map_exn ~f:(fun binary -> Path.basename binary, binary)
    in
    fun basename ->
      match Which.candidates basename |> List.find_map ~f:(Filename.Map.find map) with
      | Some s -> Memo.return (Some s)
      | None -> Which.which ~path basename
  in
  let get_ocaml_tool ~dir:_ prog = which prog in
  make name ~env ~get_ocaml_tool ~which
;;

(* Seems wrong to support this at the level of the engine. This is easily
   implemented at the level of the rules and is noly needed for windows *)
let register_response_file_support t =
  if Ocaml.Version.supports_response_file t.version
  then (
    let set prog = Response_file.set ~prog (Zero_terminated_strings "-args0") in
    Result.iter t.ocaml ~f:set;
    set t.ocamlc;
    Result.iter t.ocamlopt ~f:set;
    Result.iter t.ocamldep ~f:set;
    if Ocaml.Version.ocamlmklib_supports_response_file t.version
    then Result.iter ~f:set t.ocamlmklib)
;;

let check_fdo_support { version; lib_config = { has_native; _ }; ocaml_config; _ } name =
  let version_string = Ocaml_config.version_string ocaml_config in
  let err () =
    User_error.raise
      [ Pp.textf
          "fdo requires ocamlopt version >= 4.10, current version is %s (context: %s)"
          (Context_name.to_string name)
          version_string
      ]
  in
  if not has_native then err ();
  if Ocaml_config.is_dev_version ocaml_config
  then
    ( (* Allows fdo to be invoked with any dev version of the compiler. This is
         experimental and will be removed when ocamlfdo is fully integrated into
         the toolchain. When using a dev version of ocamlopt that does not
         support the required options, fdo builds will fail because the compiler
         won't recognize the options. Normals builds won't be affected. *) )
  else if not (Ocaml.Version.supports_split_at_emit version)
  then
    if not (Ocaml.Version.supports_function_sections version)
    then err ()
    else
      User_warning.emit
        [ Pp.textf
            "fdo requires ocamlopt version >= 4.10, current version %s has partial \
             support. Some optimizations are disabled! (context: %s)"
            (Context_name.to_string name)
            version_string
        ]
;;

let make_ocaml_config ~env ~ocamlc =
  let+ (_ : Ocaml_config.Vars.t), ocaml_config =
    make_ocaml_vars_and_config ~env ~ocamlc
  in
  ocaml_config
;;
