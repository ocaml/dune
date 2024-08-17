open Import
module DAP = Dune_action_plugin.Private.Protocol
module Dependency = Dune_action_plugin.Private.Protocol.Dependency

let to_dune_dep_set =
  let of_DAP_dep ~loc ~working_dir : Dependency.t -> Dep.t =
    let to_dune_path = Path.relative working_dir in
    function
    | File fn -> Dep.file (to_dune_path fn)
    | Directory dir ->
      let dir = to_dune_path dir in
      let selector = File_selector.of_glob ~dir Glob.universal in
      Dep.file_selector selector
    | Glob { path; glob } ->
      let dir = to_dune_path path in
      let glob = Glob.of_string_exn loc glob in
      let selector = File_selector.of_glob ~dir glob in
      Dep.file_selector selector
  in
  fun set ~loc ~working_dir ->
    Dependency.Set.to_list_map set ~f:(of_DAP_dep ~loc ~working_dir) |> Dep.Set.of_list
;;

type done_or_more_deps =
  | Done
  (* This code assumes that there can be at most one 'dynamic-run' within single
     action. [Dependency.t] stores relative paths so name clash would be
     possible if multiple 'dynamic-run' would be executed in different
     subdirectories that contains targets having the same name. *)
  | Need_more_deps of (Dependency.Set.t * Dep.Set.t)

let done_or_more_deps_union x y =
  match x, y with
  | Done, Done -> Done
  | Done, Need_more_deps x | Need_more_deps x, Done -> Need_more_deps x
  | Need_more_deps (deps1, dyn_deps1), Need_more_deps (deps2, dyn_deps2) ->
    Need_more_deps (Dependency.Set.union deps1 deps2, Dep.Set.union dyn_deps1 dyn_deps2)
;;

open Action_intf.Exec

let exec ~display ~(ectx : context) ~(eenv : env) prog args =
  let open Fiber.O in
  let* () = Rpc.ensure_ready () in
  let run_arguments_fn = Temp.create File ~prefix:"dune" ~suffix:"run" in
  let response_fn = Temp.create File ~prefix:"dune" ~suffix:"response" in
  let run_arguments =
    let targets =
      match ectx.targets with
      | None -> String.Set.empty
      | Some targets ->
        if not (Filename.Set.is_empty targets.dirs)
        then
          User_error.raise
            ~loc:ectx.rule_loc
            [ Pp.text "Directory targets are not compatible with dynamic actions" ];
        Filename.Set.to_list_map targets.files ~f:(fun target ->
          Path.Build.relative targets.root target
          |> Path.build
          |> Path.reach ~from:eenv.working_dir)
        |> String.Set.of_list
    in
    { DAP.Run_arguments.prepared_dependencies = eenv.prepared_dependencies; targets }
  in
  DAP.Run_arguments.to_sexp run_arguments
  |> Csexp.to_string
  |> Io.write_file run_arguments_fn;
  let env =
    let value =
      DAP.Greeting.(
        to_sexp
          { run_arguments_fn = Path.to_absolute_filename run_arguments_fn
          ; response_fn = Path.to_absolute_filename response_fn
          })
      |> Csexp.to_string
    in
    Env.add eenv.env ~var:DAP.run_by_dune_env_variable ~value
  in
  let+ () =
    Process.run
      ~display
      Strict
      ~dir:eenv.working_dir
      ~env
      ~stderr_to:eenv.stderr_to
      ~stdin_from:eenv.stdin_from
      ~metadata:ectx.metadata
      prog
      args
  in
  let response_raw = Io.read_file response_fn in
  Temp.destroy File run_arguments_fn;
  Temp.destroy File response_fn;
  let response =
    match Csexp.parse_string response_raw with
    | Ok s -> DAP.Response.of_sexp s
    | Error _ -> Error DAP.Error.Parse_error
  in
  let prog_name = Path.reach ~from:eenv.working_dir prog in
  match response with
  | Error _ when String.is_empty response_raw ->
    User_error.raise
      ~loc:ectx.rule_loc
      [ Pp.textf
          "Executable '%s' declared as using dune-action-plugin (declared with \
           'dynamic-run' tag) failed to respond to dune."
          prog_name
      ; Pp.nop
      ; Pp.text
          "If you don't use dynamic dependency discovery in your executable you may \
           consider changing 'dynamic-run' to 'run' in your rule definition."
      ]
  | Error Parse_error ->
    User_error.raise
      ~loc:ectx.rule_loc
      [ Pp.textf
          "Executable '%s' declared as using dune-action-plugin (declared with \
           'dynamic-run' tag) responded with invalid message."
          prog_name
      ]
  | Error (Version_mismatch _) ->
    User_error.raise
      ~loc:ectx.rule_loc
      [ Pp.textf
          "Executable '%s' is linked against a version of dune-action-plugin library \
           that is incompatible with this version of dune."
          prog_name
      ]
  | Ok Done -> Done
  | Ok (Need_more_deps deps) ->
    Need_more_deps
      (deps, to_dune_dep_set deps ~loc:ectx.rule_loc ~working_dir:eenv.working_dir)
;;
