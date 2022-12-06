open Import
open Fiber.O
module DAP = Dune_action_plugin.Private.Protocol

(** A version of [Dune_action_plugin.Private.Protocol.Dependency] where all
    relative paths are replaced by [Path.t]. (except the protocol doesn't
    support Globs yet) *)
module Dynamic_dep = struct
  module T = struct
    type t =
      | File of Path.t
      | Glob of Path.t * Glob.t

    let to_dep = function
      | File fn -> Dep.file fn
      | Glob (dir, glob) -> File_selector.of_glob ~dir glob |> Dep.file_selector

    let of_DAP_dep ~loc ~working_dir : DAP.Dependency.t -> t =
      let to_dune_path = Path.relative working_dir in
      function
      | File fn -> File (to_dune_path fn)
      | Directory dir -> Glob (to_dune_path dir, Glob.universal)
      | Glob { path; glob } ->
        Glob (to_dune_path path, Glob.of_string_exn loc glob)

    let compare x y =
      match (x, y) with
      | File x, File y -> Path.compare x y
      | File _, _ -> Lt
      | _, File _ -> Gt
      | Glob (dir1, glob1), Glob (dir2, glob2) ->
        let open Ordering.O in
        let= () = Path.compare dir1 dir2 in
        Glob.compare glob1 glob2

    let to_dyn =
      let open Dyn in
      function
      | File fn -> variant "File" [ Path.to_dyn fn ]
      | Glob (dir, glob) -> variant "Glob" [ Path.to_dyn dir; Glob.to_dyn glob ]
  end

  include T
  module O = Comparable.Make (T)
  module Map = O.Map

  module Set = struct
    include O.Set

    let to_dep_set t = to_list_map t ~f:to_dep |> Dep.Set.of_list

    let of_DAP_dep_set t ~loc ~working_dir =
      t |> DAP.Dependency.Set.to_list
      |> of_list_map ~f:(of_DAP_dep ~loc ~working_dir)
  end
end

module Exec_result = struct
  type t = { dynamic_deps_stages : (Dynamic_dep.Set.t * Dep.Facts.t) List.t }
end

type done_or_more_deps =
  | Done
  (* This code assumes that there can be at most one 'dynamic-run' within single
     action. [DAP.Dependency.t] stores relative paths so name clash would be
     possible if multiple 'dynamic-run' would be executed in different
     subdirectories that contains targets having the same name. *)
  | Need_more_deps of (DAP.Dependency.Set.t * Dynamic_dep.Set.t)

type exec_context =
  { targets : Targets.Validated.t option
  ; context : Build_context.t option
  ; metadata : Process.metadata
  ; rule_loc : Loc.t
  ; build_deps : Dep.Set.t -> Dep.Facts.t Fiber.t
  }

type exec_environment =
  { working_dir : Path.t
  ; env : Env.t
  ; stdout_to : Process.Io.output Process.Io.t
  ; stderr_to : Process.Io.output Process.Io.t
  ; stdin_from : Process.Io.input Process.Io.t
  ; prepared_dependencies : DAP.Dependency.Set.t
  ; exit_codes : int Predicate.t
  }

let validate_context_and_prog ectx prog =
  match ectx.context with
  | None | Some { Build_context.host = None; _ } -> ()
  | Some ({ Build_context.host = Some host; _ } as target) ->
    let target_name = Context_name.to_string target.name in
    let invalid_prefix prefix =
      match Path.descendant prog ~of_:prefix with
      | None -> ()
      | Some _ ->
        User_error.raise ~loc:ectx.rule_loc
          [ Pp.textf "Context %s has a host %s." target_name
              (Context_name.to_string host)
          ; Pp.textf "It's not possible to execute binary %s in it."
              (Path.to_string_maybe_quoted prog)
          ; Pp.nop
          ; Pp.text "This is a bug and should be reported upstream."
          ]
    in
    invalid_prefix (Path.relative Path.build_dir target_name);
    invalid_prefix (Path.relative Path.build_dir ("install/" ^ target_name))

let exec_run ~ectx ~eenv prog args =
  validate_context_and_prog ectx prog;
  let+ (_ : (unit, int) result) =
    Process.run (Accept eenv.exit_codes) ~dir:eenv.working_dir ~env:eenv.env
      ~stdout_to:eenv.stdout_to ~stderr_to:eenv.stderr_to
      ~stdin_from:eenv.stdin_from ~metadata:ectx.metadata prog args
  in
  ()

let exec_run_dynamic_client ~ectx ~eenv prog args =
  validate_context_and_prog ectx prog;
  let run_arguments_fn = Temp.create File ~prefix:"dune" ~suffix:"run" in
  let response_fn = Temp.create File ~prefix:"dune" ~suffix:"response" in
  let run_arguments =
    let targets =
      match ectx.targets with
      | None -> String.Set.empty
      | Some targets ->
        if not (Path.Build.Set.is_empty targets.dirs) then
          User_error.raise ~loc:ectx.rule_loc
            [ Pp.text
                "Directory targets are not compatible with dynamic actions"
            ];
        Path.Build.Set.to_list_map targets.files ~f:(fun target ->
            Path.reach (Path.build target) ~from:eenv.working_dir)
        |> String.Set.of_list
    in
    { DAP.Run_arguments.prepared_dependencies = eenv.prepared_dependencies
    ; targets
    }
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
    Process.run Strict ~dir:eenv.working_dir ~env ~stdout_to:eenv.stdout_to
      ~stderr_to:eenv.stderr_to ~stdin_from:eenv.stdin_from
      ~metadata:ectx.metadata prog args
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
    User_error.raise ~loc:ectx.rule_loc
      [ Pp.textf
          "Executable '%s' declared as using dune-action-plugin (declared with \
           'dynamic-run' tag) failed to respond to dune."
          prog_name
      ; Pp.nop
      ; Pp.text
          "If you don't use dynamic dependency discovery in your executable \
           you may consider changing 'dynamic-run' to 'run' in your rule \
           definition."
      ]
  | Error Parse_error ->
    User_error.raise ~loc:ectx.rule_loc
      [ Pp.textf
          "Executable '%s' declared as using dune-action-plugin (declared with \
           'dynamic-run' tag) responded with invalid message."
          prog_name
      ]
  | Error (Version_mismatch _) ->
    User_error.raise ~loc:ectx.rule_loc
      [ Pp.textf
          "Executable '%s' is linked against a version of dune-action-plugin \
           library that is incompatible with this version of dune."
          prog_name
      ]
  | Ok Done -> Done
  | Ok (Need_more_deps deps) ->
    Need_more_deps
      ( deps
      , Dynamic_dep.Set.of_DAP_dep_set deps ~loc:ectx.rule_loc
          ~working_dir:eenv.working_dir )

let exec_echo stdout_to str =
  Fiber.return (output_string (Process.Io.out_channel stdout_to) str)

let bash_exn =
  let bin = lazy (Bin.which ~path:(Env.path Env.initial) "bash") in
  fun ~loc ~needed_to ->
    match Lazy.force bin with
    | Some path -> path
    | None ->
      User_error.raise ~loc
        [ Pp.textf "I need bash to %s but I couldn't find it :(" needed_to ]

(* When passing these to an extension, they shouldn't need to know about any
   kind of dynamic build dependency functions or prepped dependencies, etc,
   which should be handled here instead. *)
let restrict_ctx { targets; context; metadata; rule_loc; build_deps = _ } =
  { Action.Ext.targets; context; purpose = metadata.purpose; rule_loc }

let restrict_env
    { working_dir
    ; env
    ; stdout_to
    ; stderr_to
    ; stdin_from
    ; exit_codes
    ; prepared_dependencies = _
    } =
  { Action.Ext.working_dir; env; stdout_to; stderr_to; stdin_from; exit_codes }

let compare_files = function
  | Diff.Mode.Binary -> Io.compare_files
  | Text -> Io.compare_text_files

let diff_eq_files { Diff.optional; mode; file1; file2 } =
  let file1 = if Path.Untracked.exists file1 then file1 else Config.dev_null in
  let file2 = Path.build file2 in
  (optional && not (Path.Untracked.exists file2))
  || compare_files mode file1 file2 = Eq

let rec exec t ~ectx ~eenv =
  match (t : Action.t) with
  | Run (Error e, _) -> Action.Prog.Not_found.raise e
  | Run (Ok prog, args) ->
    let+ () = exec_run ~ectx ~eenv prog args in
    Done
  | With_accepted_exit_codes (exit_codes, t) ->
    let eenv =
      let standard = Predicate_lang.Element (Predicate.create (Int.equal 0)) in
      let exit_codes =
        Predicate_lang.map exit_codes ~f:(fun i ->
            Predicate.create (Int.equal i))
        |> Predicate_lang.to_predicate ~standard
      in
      { eenv with exit_codes }
    in
    exec t ~ectx ~eenv
  | Dynamic_run (Error e, _) -> Action.Prog.Not_found.raise e
  | Dynamic_run (Ok prog, args) -> exec_run_dynamic_client ~ectx ~eenv prog args
  | Chdir (dir, t) -> exec t ~ectx ~eenv:{ eenv with working_dir = dir }
  | Setenv (var, value, t) ->
    exec t ~ectx ~eenv:{ eenv with env = Env.add eenv.env ~var ~value }
  | Redirect_out (Stdout, fn, perm, Echo s) ->
    let perm = Action.File_perm.to_unix_perm perm in
    Io.write_file (Path.build fn) (String.concat s ~sep:" ") ~perm;
    Fiber.return Done
  | Redirect_out (outputs, fn, perm, t) ->
    let fn = Path.build fn in
    redirect_out t ~ectx ~eenv outputs ~perm fn
  | Redirect_in (inputs, fn, t) -> redirect_in t ~ectx ~eenv inputs fn
  | Ignore (outputs, t) ->
    redirect_out t ~ectx ~eenv ~perm:Normal outputs Config.dev_null
  | Progn ts -> exec_list ts ~ectx ~eenv
  | Echo strs ->
    let+ () = exec_echo eenv.stdout_to (String.concat strs ~sep:" ") in
    Done
  | Cat xs ->
    List.iter xs ~f:(fun fn ->
        Io.with_file_in fn ~f:(fun ic ->
            Io.copy_channels ic (Process.Io.out_channel eenv.stdout_to)));
    Fiber.return Done
  | Copy (src, dst) ->
    let dst = Path.build dst in
    Io.copy_file ~src ~dst ();
    Fiber.return Done
  | Symlink (src, dst) ->
    Io.portable_symlink ~src ~dst:(Path.build dst);
    Fiber.return Done
  | Hardlink (src, dst) ->
    Io.portable_hardlink ~src ~dst:(Path.build dst);
    Fiber.return Done
  | System cmd ->
    let path, arg =
      Utils.system_shell_exn ~needed_to:"interpret (system ...) actions"
    in
    let+ () = exec_run ~ectx ~eenv path [ arg; cmd ] in
    Done
  | Bash cmd ->
    let+ () =
      exec_run ~ectx ~eenv
        (bash_exn ~loc:ectx.rule_loc ~needed_to:"interpret (bash ...) actions")
        [ "-e"; "-u"; "-o"; "pipefail"; "-c"; cmd ]
    in
    Done
  | Write_file (fn, perm, s) ->
    let perm = Action.File_perm.to_unix_perm perm in
    Io.write_file (Path.build fn) s ~perm;
    Fiber.return Done
  | Rename (src, dst) ->
    Unix.rename (Path.Build.to_string src) (Path.Build.to_string dst);
    Fiber.return Done
  | Remove_tree path ->
    Path.rm_rf (Path.build path);
    Fiber.return Done
  | Mkdir path ->
    Path.mkdir_p (Path.build path);
    Fiber.return Done
  | Diff ({ optional; file1; file2; mode } as diff) ->
    let remove_intermediate_file () =
      if optional then
        try Path.unlink (Path.build file2)
        with Unix.Unix_error (ENOENT, _, _) -> ()
    in
    if diff_eq_files diff then (
      remove_intermediate_file ();
      Fiber.return Done)
    else
      let is_copied_from_source_tree file =
        match Path.extract_build_context_dir_maybe_sandboxed file with
        | None -> false
        | Some (_, file) -> Path.Untracked.exists (Path.source file)
      in
      let+ () =
        let in_source_or_target =
          is_copied_from_source_tree file1 || not (Path.Untracked.exists file1)
        in
        let source_file =
          snd
            (Option.value_exn
               (Path.extract_build_context_dir_maybe_sandboxed file1))
        in
        Fiber.finalize
          (fun () ->
            let annots =
              User_message.Annots.singleton Diff_promotion.Annot.annot
                { Diff_promotion.Annot.in_source = source_file
                ; in_build =
                    (if optional && in_source_or_target then
                     Diff_promotion.File.in_staging_area source_file
                    else file2)
                }
            in
            if mode = Binary then
              User_error.raise ~annots ~loc:ectx.rule_loc
                [ Pp.textf "Files %s and %s differ."
                    (Path.to_string_maybe_quoted file1)
                    (Path.to_string_maybe_quoted (Path.build file2))
                ]
            else
              Print_diff.print annots file1 (Path.build file2)
                ~skip_trailing_cr:(mode = Text && Sys.win32))
          ~finally:(fun () ->
            (match optional with
            | false ->
              (* Promote if in the source tree or not a target. The second case
                 means that the diffing have been done with the empty file *)
              if
                in_source_or_target
                && not (is_copied_from_source_tree (Path.build file2))
              then
                Diff_promotion.File.register_dep ~source_file
                  ~correction_file:file2
            | true ->
              if in_source_or_target then
                Diff_promotion.File.register_intermediate ~source_file
                  ~correction_file:file2
              else remove_intermediate_file ());
            Fiber.return ())
      in
      Done
  | Merge_files_into (sources, extras, target) ->
    let lines =
      List.fold_left sources ~init:(String.Set.of_list extras)
        ~f:(fun set source_path ->
          Io.lines_of_file source_path
          |> String.Set.of_list |> String.Set.union set)
    in
    let target = Path.build target in
    Io.write_lines target (String.Set.to_list lines);
    Fiber.return Done
  | Pipe (outputs, l) -> exec_pipe ~ectx ~eenv outputs l
  | Extension (module A) ->
    let* () =
      A.Spec.action A.v ~ectx:(restrict_ctx ectx) ~eenv:(restrict_env eenv)
    in
    Fiber.return Done

and redirect_out t ~ectx ~eenv ~perm outputs fn =
  redirect t ~ectx ~eenv ~out:(outputs, fn, perm) ()

and redirect_in t ~ectx ~eenv inputs fn =
  redirect t ~ectx ~eenv ~in_:(inputs, fn) ()

and redirect t ~ectx ~eenv ?in_ ?out () =
  let stdin_from, release_in =
    match in_ with
    | None -> (eenv.stdin_from, ignore)
    | Some (Stdin, fn) ->
      let in_ = Process.Io.file fn Process.Io.In in
      (in_, fun () -> Process.Io.release in_)
  in
  let stdout_to, stderr_to, release_out =
    match out with
    | None -> (eenv.stdout_to, eenv.stderr_to, ignore)
    | Some (outputs, fn, perm) ->
      let out =
        Process.Io.file fn Process.Io.Out
          ~perm:(Action.File_perm.to_unix_perm perm)
      in
      let stdout_to, stderr_to =
        match outputs with
        | Stdout -> (out, eenv.stderr_to)
        | Stderr -> (eenv.stdout_to, out)
        | Outputs -> (out, out)
      in
      (stdout_to, stderr_to, fun () -> Process.Io.release out)
  in
  let+ result =
    exec t ~ectx ~eenv:{ eenv with stdin_from; stdout_to; stderr_to }
  in
  release_in ();
  release_out ();
  result

and exec_list ts ~ectx ~eenv =
  match ts with
  | [] -> Fiber.return Done
  | [ t ] -> exec t ~ectx ~eenv
  | t :: rest -> (
    let* done_or_deps =
      let stdout_to = Process.Io.multi_use eenv.stdout_to in
      let stderr_to = Process.Io.multi_use eenv.stderr_to in
      let stdin_from = Process.Io.multi_use eenv.stdin_from in
      exec t ~ectx ~eenv:{ eenv with stdout_to; stderr_to; stdin_from }
    in
    match done_or_deps with
    | Need_more_deps _ as need -> Fiber.return need
    | Done -> exec_list rest ~ectx ~eenv)

and exec_pipe outputs ts ~ectx ~eenv =
  let tmp_file () =
    Dtemp.file ~prefix:"dune-pipe-action-"
      ~suffix:("." ^ Action.Outputs.to_string outputs)
  in
  let rec loop ~in_ ts =
    match ts with
    | [] -> assert false
    | [ last_t ] ->
      let eenv =
        match outputs with
        | Stderr ->
          { eenv with stdout_to = Process.Io.multi_use eenv.stderr_to }
        | _ -> eenv
      in
      let+ result = redirect_in last_t ~ectx ~eenv Stdin in_ in
      Dtemp.destroy File in_;
      result
    | t :: ts -> (
      let out = tmp_file () in
      let* done_or_deps =
        let eenv =
          { eenv with stderr_to = Process.Io.multi_use eenv.stderr_to }
        in
        redirect t ~ectx ~eenv ~in_:(Stdin, in_) ~out:(Stdout, out, Normal) ()
      in
      Dtemp.destroy File in_;
      match done_or_deps with
      | Need_more_deps _ as need -> Fiber.return need
      | Done -> loop ~in_:out ts)
  in
  match ts with
  | [] -> assert false
  | t1 :: ts -> (
    let out = tmp_file () in
    let eenv =
      match outputs with
      | Outputs -> eenv
      | Stdout -> { eenv with stderr_to = Process.Io.multi_use eenv.stderr_to }
      | Stderr -> { eenv with stdout_to = Process.Io.multi_use eenv.stdout_to }
    in
    let* done_or_deps = redirect_out t1 ~ectx ~eenv ~perm:Normal outputs out in
    match done_or_deps with
    | Need_more_deps _ as need -> Fiber.return need
    | Done -> loop ~in_:out ts)

let exec_until_all_deps_ready ~ectx ~eenv t =
  let rec loop ~eenv stages =
    let* result = exec ~ectx ~eenv t in
    match result with
    | Done -> Fiber.return stages
    | Need_more_deps (relative_deps, deps_to_build) ->
      let* fact_map =
        ectx.build_deps (Dynamic_dep.Set.to_dep_set deps_to_build)
      in
      let stages = (deps_to_build, fact_map) :: stages in
      let eenv =
        { eenv with
          prepared_dependencies =
            DAP.Dependency.Set.union eenv.prepared_dependencies relative_deps
        }
      in
      loop ~eenv stages
  in
  let+ stages = loop ~eenv [] in
  { Exec_result.dynamic_deps_stages = List.rev stages }

let exec ~targets ~root ~context ~env ~rule_loc ~build_deps
    ~execution_parameters t =
  let ectx =
    let metadata = Process.create_metadata ~purpose:(Build_job targets) () in
    { targets; metadata; context; rule_loc; build_deps }
  and eenv =
    let env =
      match
        Execution_parameters.add_workspace_root_to_build_path_prefix_map
          execution_parameters
      with
      | false -> env
      | true ->
        Dune_util.Build_path_prefix_map.extend_build_path_prefix_map env
          `New_rules_have_precedence
          [ Some
              { source = Path.to_absolute_filename root
              ; target = "/workspace_root"
              }
          ]
    in
    { working_dir = Path.root
    ; env
    ; stdout_to =
        Process.Io.make_stdout
          (Execution_parameters.action_stdout_on_success execution_parameters)
    ; stderr_to =
        Process.Io.make_stderr
          (Execution_parameters.action_stderr_on_success execution_parameters)
    ; stdin_from = Process.Io.null In
    ; prepared_dependencies = DAP.Dependency.Set.empty
    ; exit_codes = Predicate.create (Int.equal 0)
    }
  in
  exec_until_all_deps_ready t ~ectx ~eenv
