open Import
open Stdune.Action_types
open Action_intf.Exec
open Done_or_more_deps
module Dependency = Dune_action_plugin.Private.Protocol.Dependency

let maybe_async =
  let maybe_async =
    lazy
      (match Config.(get background_actions) with
       | `Enabled -> Scheduler.async_exn
       | `Disabled -> fun f -> Fiber.return (f ()))
  in
  fun f -> (Lazy.force maybe_async) f
;;

module Exec_result = struct
  module Error = struct
    type t =
      | User of User_message.t
      | Code of Code_error.t
      | Sys of string
      | Unix of Unix.error * string * string
      | Nonreproducible_build_cancelled

    (* We can't capture raw backtraces since they are not marshallable.
       We can convert those to marshallable backtrace slots, but we can't convert them
       back to re-raise exceptions with preserved backtraces. *)
    let of_exn (e : exn) =
      match e with
      | User_error.E msg -> User msg
      | Code_error.E err -> Code err
      | Sys_error msg -> Sys msg
      | Unix.Unix_error (err, call, args) -> Unix (err, call, args)
      | Memo.Non_reproducible Scheduler.Run.Build_cancelled ->
        Nonreproducible_build_cancelled
      | Memo.Cycle_error.E _ as e ->
        (* [Memo.Cycle_error.t] is hard to serialize and can only be raised during action
           execution with the dynamic dependencies plugin, which is not production-ready yet.
           For now, we just re-reraise it.
        *)
        reraise e
      | e ->
        Code
          { message = "unable to serialize exception"
          ; data = [ "exn", Exn.to_dyn e ]
          ; loc = None
          }
    ;;

    let to_exn (t : t) =
      match t with
      | User msg -> User_error.E msg
      | Code err -> Code_error.E err
      | Sys msg -> Sys_error msg
      | Unix (err, call, args) -> Unix.Unix_error (err, call, args)
      | Nonreproducible_build_cancelled ->
        Memo.Non_reproducible Scheduler.Run.Build_cancelled
    ;;
  end

  type ok = { dynamic_deps_stages : (Dep.Set.t * Dep.Facts.t) list }
  type t = (ok, Error.t list) Result.t

  let ok_exn (t : t) =
    match t with
    | Ok t -> Fiber.return t
    | Error errs ->
      Fiber.reraise_all
        (List.map errs ~f:(fun e -> Exn_with_backtrace.capture (Error.to_exn e)))
  ;;
end

open Fiber.O

type execution_mode =
  | Build
  | Shell_replay

exception Shell_replay_failed of Process.Failure_mode.raw_status

let exec_run ~(ectx : context) ~(eenv : env) ~mode ~can_run_in_action_runner prog args =
  let can_run_in_action_runner =
    match mode with
    | Build -> can_run_in_action_runner
    | Shell_replay -> false
  in
  let metadata = { ectx.metadata with can_run_in_action_runner } in
  match mode with
  | Build ->
    let+ (_ : (unit, int) result) =
      Process.run
        ~display:!Clflags.display
        (Accept eenv.exit_codes)
        ~dir:eenv.working_dir
        ~env:eenv.env
        ~stdout_to:eenv.stdout_to
        ~stderr_to:eenv.stderr_to
        ~stdin_from:eenv.stdin_from
        ~metadata
        prog
        args
    in
    ()
  | Shell_replay ->
    let+ (), status =
      Process.run
        ~display:Quiet
        Return_raw
        ~dir:eenv.working_dir
        ~env:eenv.env
        ~stdout_to:eenv.stdout_to
        ~stderr_to:eenv.stderr_to
        ~stdin_from:eenv.stdin_from
        ~metadata
        prog
        args
    in
    (match status with
     | Process.Failure_mode.Exited exit_code ->
       if not (Predicate.test eenv.exit_codes exit_code)
       then raise (Shell_replay_failed status)
     | Process.Failure_mode.Signaled _ -> raise (Shell_replay_failed status))
;;

let bash_exn =
  let bin = lazy (Bin.which ~path:(Env_path.path Env.initial) "bash") in
  fun ~loc ~needed_to ->
    match Lazy.force bin with
    | Some path -> path
    | None ->
      User_error.raise
        ~loc
        [ Pp.textf "I need bash to %s but I couldn't find it :(" needed_to ]
;;

let zero = Predicate_lang.element 0

let rec exec t ~ectx ~eenv ~mode : Done_or_more_deps.t Fiber.t =
  match (t : Action.t) with
  | Run { prog = Error e; args = _; can_run_in_action_runner = _ } ->
    Action.Prog.Not_found.raise e
  | Run { prog = Ok prog; args; can_run_in_action_runner } ->
    let+ () =
      exec_run
        ~ectx
        ~eenv
        ~mode
        ~can_run_in_action_runner
        prog
        (Appendable_list.to_list args)
    in
    Done
  | With_accepted_exit_codes (exit_codes, t) ->
    let eenv =
      let exit_codes =
        Predicate.create (Predicate_lang.test exit_codes ~test:Int.equal ~standard:zero)
      in
      { eenv with exit_codes }
    in
    exec t ~ectx ~eenv ~mode
  | Chdir (dir, t) -> exec t ~ectx ~eenv:{ eenv with working_dir = dir } ~mode
  | Setenv (var, value, t) ->
    exec t ~ectx ~eenv:{ eenv with env = Env.add eenv.env ~var ~value } ~mode
  | Redirect_out (Stdout, fn, perm, Echo s) ->
    let perm = File_perm.to_unix_perm perm in
    let+ () =
      maybe_async (fun () ->
        Io.write_file (Path.build fn) (String.concat s ~sep:" ") ~perm)
    in
    Done
  | Redirect_out (outputs, fn, perm, t) ->
    let fn = Path.build fn in
    redirect_out t ~ectx ~eenv ~mode outputs ~perm fn
  | Redirect_in (inputs, fn, t) -> redirect_in t ~ectx ~eenv ~mode inputs fn
  | Ignore (outputs, t) ->
    redirect_out t ~ectx ~eenv ~mode ~perm:Normal outputs Dev_null.path
  | Progn ts -> exec_list ts ~ectx ~eenv ~mode
  | Concurrent ts ->
    Fiber.parallel_map ts ~f:(exec ~ectx ~eenv ~mode)
    >>| List.fold_left ~f:Done_or_more_deps.union ~init:Done
  | Echo strs ->
    let () =
      String.concat strs ~sep:" " |> output_string (Process.Io.out_channel eenv.stdout_to)
    in
    Fiber.return Done
  | Cat xs ->
    let+ () =
      maybe_async (fun () ->
        List.iter xs ~f:(fun fn ->
          Io.with_file_in fn ~f:(fun ic ->
            Io.copy_channels ic (Process.Io.out_channel eenv.stdout_to))))
    in
    Done
  | Copy (src, dst) ->
    let dst = Path.build dst in
    let copy_file ~src ~dst =
      Path.parent dst |> Option.iter ~f:Path.mkdir_p;
      Io.copy_file ~src ~dst ()
    in
    let mkdir ~src:_ ~dst = Path.mkdir_p dst in
    let on_unsupported ~src kind =
      User_error.raise
        [ Pp.textf
            "Failed to copy %s of kind %S while executing a copy action"
            (Path.to_string_maybe_quoted src)
            (File_kind.to_string_hum kind)
        ]
    in
    let+ () =
      maybe_async (fun () ->
        (* NOTE(anmonteiro): we may reconsider relaxing the directory target
           constraint (see [test/blackbox-tests/test-cases/pkg/source-with-directory-symlink.t]).

           [Copy] stays file-oriented by default. We only use recursive copying
           when [dst] is a directory target. *)
        match ectx.targets with
        | Some { dirs; _ } when Filename.Set.mem dirs (Path.basename dst) ->
          Tree_copy.copy ~src ~dst ~copy_file ~mkdir ~on_unsupported ()
        | _ -> copy_file ~src ~dst)
    in
    Done
  | Symlink (src, dst) ->
    let+ () = maybe_async (fun () -> Io.portable_symlink ~src ~dst:(Path.build dst)) in
    Done
  | Hardlink (src, dst) ->
    let+ () = maybe_async (fun () -> Io.portable_hardlink ~src ~dst:(Path.build dst)) in
    Done
  | System command ->
    let prog, arg =
      Env_path.system_shell_exn ~needed_to:"interpret (system ...) actions"
    in
    let+ () =
      exec_run ~ectx ~eenv ~mode ~can_run_in_action_runner:true prog [ arg; command ]
    in
    Done
  | Bash { script; can_run_in_action_runner } ->
    let+ () =
      exec_run
        ~ectx
        ~eenv
        ~mode
        ~can_run_in_action_runner
        (bash_exn ~loc:ectx.rule_loc ~needed_to:"interpret (bash ...) actions")
        [ "-e"; "-u"; "-o"; "pipefail"; "-c"; script ]
    in
    Done
  | Write_file (fn, perm, s) ->
    let start = Time.now () in
    let fn = Path.build fn in
    let* () =
      maybe_async (fun () ->
        let perm = File_perm.to_unix_perm perm in
        Io.write_file fn s ~perm)
    in
    let finish = Time.now () in
    Dune_trace.emit ~buffered:true Action (fun () ->
      Dune_trace.Event.Action.write_file ~start ~finish ~file:fn ~size:(String.length s));
    Fiber.return Done
  | Rename (src, dst) ->
    let src = Path.Build.to_string src in
    let dst = Path.Build.to_string dst in
    let+ () = maybe_async (fun () -> Unix.rename src dst) in
    Done
  | Remove_tree path ->
    let+ () = maybe_async (fun () -> Path.rm_rf (Path.build path)) in
    Done
  | Mkdir path ->
    let+ () = maybe_async (fun () -> Path.mkdir_p (Path.build path)) in
    Done
  | Pipe (outputs, l) -> exec_pipe ~ectx ~eenv ~mode outputs l
  | Diff diff ->
    let+ () =
      match mode with
      | Build -> Diff_action.exec ~patch_back:None ectx.rule_loc diff
      | Shell_replay -> Diff_action.exec_without_promotion ectx.rule_loc diff
    in
    Done
  | Extension (module A) ->
    let metadata =
      { ectx.metadata with can_run_in_action_runner = A.Spec.can_run_in_action_runner }
    in
    A.Spec.action A.v ~ectx:{ ectx with metadata } ~eenv

and redirect_out t ~ectx ~eenv ~mode ~perm outputs fn =
  redirect t ~ectx ~eenv ~mode ~out:(outputs, fn, perm) ()

and redirect_in t ~ectx ~eenv ~mode inputs fn =
  redirect t ~ectx ~eenv ~mode ~in_:(inputs, fn) ()

and redirect t ~ectx ~eenv ~mode ?in_ ?out () =
  let stdin_from, release_in =
    match in_ with
    | None -> eenv.stdin_from, ignore
    | Some (Stdin, fn) ->
      let in_ = Process.Io.file fn Process.Io.In in
      in_, fun () -> Process.Io.release in_
  in
  let stdout_to, stderr_to, release_out =
    match out with
    | None -> eenv.stdout_to, eenv.stderr_to, ignore
    | Some (outputs, fn, perm) ->
      let out = Process.Io.file fn Process.Io.Out ~perm:(File_perm.to_unix_perm perm) in
      let stdout_to, stderr_to =
        match outputs with
        | Stdout -> out, eenv.stderr_to
        | Stderr -> eenv.stdout_to, out
        | Outputs -> out, out
      in
      stdout_to, stderr_to, fun () -> Process.Io.release out
  in
  let+ result = exec t ~ectx ~eenv:{ eenv with stdin_from; stdout_to; stderr_to } ~mode in
  release_in ();
  release_out ();
  result

and exec_list ts ~ectx ~eenv ~mode : Done_or_more_deps.t Fiber.t =
  match ts with
  | [] -> Fiber.return Done
  | [ t ] -> exec t ~ectx ~eenv ~mode
  | t :: rest ->
    (let stdout_to = Process.Io.multi_use eenv.stdout_to in
     let stderr_to = Process.Io.multi_use eenv.stderr_to in
     let stdin_from = Process.Io.multi_use eenv.stdin_from in
     exec t ~ectx ~eenv:{ eenv with stdout_to; stderr_to; stdin_from } ~mode)
    >>= (function
     | Need_more_deps _ as need -> Fiber.return need
     | Done -> exec_list rest ~ectx ~eenv ~mode)

and exec_pipe outputs ts ~ectx ~eenv ~mode : Done_or_more_deps.t Fiber.t =
  let tmp_file () =
    Dtemp.file ~prefix:"dune-pipe-action-" ~suffix:("." ^ Outputs.to_string outputs)
  in
  let rec loop ~in_ ts =
    match ts with
    | [] -> assert false
    | [ last_t ] ->
      let+ result =
        let eenv =
          match outputs with
          | Stderr -> { eenv with stdout_to = Process.Io.multi_use eenv.stderr_to }
          | _ -> eenv
        in
        redirect_in last_t ~ectx ~eenv ~mode Stdin in_
      in
      Dtemp.destroy File in_;
      result
    | t :: ts ->
      let out = tmp_file () in
      let* done_or_deps =
        let eenv = { eenv with stderr_to = Process.Io.multi_use eenv.stderr_to } in
        redirect t ~ectx ~eenv ~mode ~in_:(Stdin, in_) ~out:(Stdout, out, Normal) ()
      in
      Dtemp.destroy File in_;
      (match done_or_deps with
       | Need_more_deps _ as need -> Fiber.return need
       | Done -> loop ~in_:out ts)
  in
  match ts with
  | [] -> assert false
  | t1 :: ts ->
    let out = tmp_file () in
    let eenv =
      match outputs with
      | Outputs -> eenv
      | Stdout -> { eenv with stderr_to = Process.Io.multi_use eenv.stderr_to }
      | Stderr -> { eenv with stdout_to = Process.Io.multi_use eenv.stdout_to }
    in
    redirect_out t1 ~ectx ~eenv ~mode ~perm:Normal outputs out
    >>= (function
     | Need_more_deps _ as need -> Fiber.return need
     | Done -> loop ~in_:out ts)
;;

let exec_until_all_deps_ready ~ectx ~eenv ~mode t =
  let rec loop ~eenv stages =
    let* result = exec ~ectx ~eenv ~mode t in
    match result with
    | Done -> Fiber.return stages
    | Need_more_deps (relative_deps, deps_to_build) ->
      let* stages =
        let+ fact_map = ectx.build_deps deps_to_build in
        (deps_to_build, fact_map) :: stages
      in
      let eenv =
        { eenv with
          prepared_dependencies =
            Dependency.Set.union eenv.prepared_dependencies relative_deps
        }
      in
      loop ~eenv stages
  in
  let+ stages = loop ~eenv [] in
  { Exec_result.dynamic_deps_stages = List.rev stages }
;;

type input =
  { targets : Targets.Validated.t option (* Some Jane Street actions use [None] *)
  ; root : Path.t
  ; context : Build_context.t option
  ; env : Env.t
  ; rule_loc : Loc.t
  ; execution_parameters : Execution_parameters.t
  ; action : Action.t
  }

let prepare_env ~root ~env execution_parameters =
  let env =
    match
      Execution_parameters.workspace_root_to_build_path_prefix_map execution_parameters
    with
    | Unset -> env
    | Set target ->
      Dune_util.Build_path_prefix_map.extend_build_path_prefix_map
        env
        `New_rules_have_precedence
        (* TODO generify *)
        [ Some { source = Path.to_absolute_filename root; target } ]
  in
  let var = "DUNE_PROJECT_ROOT" in
  match Execution_parameters.action_project_root execution_parameters with
  | None -> Env.remove env ~var
  | Some project_root ->
    (match Path.as_in_build_dir root with
     | None -> env
     | Some root ->
       let project_root = Path.Build.append_source root project_root in
       Env.add env ~var ~value:(Path.to_absolute_filename (Path.build project_root)))
;;

let prepare_chdirs action =
  Action.chdirs action
  |> Path.Build.Set.iter ~f:(fun path -> Path.mkdir_p (Path.build path));
  Fiber.return ()
;;

let exec
      { targets; root; context; env; rule_loc; execution_parameters; action = t }
      ~build_deps
  =
  let ectx =
    let metadata =
      Process_metadata.create ~purpose:(Process_metadata.Build_job targets) ()
    in
    { targets; metadata; context; rule_loc; build_deps }
  and eenv =
    let env = prepare_env ~root ~env execution_parameters in
    { working_dir = Path.root
    ; env
    ; stdout_to =
        Process.Io.make_stdout
          ~output_on_success:
            (Execution_parameters.action_stdout_on_success execution_parameters)
          ~output_limit:(Execution_parameters.action_stdout_limit execution_parameters)
    ; stderr_to =
        Process.Io.make_stderr
          ~output_on_success:
            (Execution_parameters.action_stderr_on_success execution_parameters)
          ~output_limit:(Execution_parameters.action_stderr_limit execution_parameters)
    ; stdin_from = Process.Io.null In
    ; prepared_dependencies = Dependency.Set.empty
    ; exit_codes = Predicate.create (Int.equal 0)
    }
  in
  let open Fiber.O in
  Fiber.collect_errors (fun () -> exec_until_all_deps_ready t ~ectx ~eenv ~mode:Build)
  >>| function
  | Ok res -> Ok res
  | Error exns ->
    Error
      (List.map exns ~f:(fun (e : Exn_with_backtrace.t) -> Exec_result.Error.of_exn e.exn))
;;

type replay_input =
  { targets : Targets.Validated.t
  ; dir : Path.t
  ; env : Env.t
  ; rule_loc : Loc.t
  ; action : Action.t
  ; temp_dir : Path.t
  }

let replay { targets; dir; env; rule_loc; action; temp_dir } =
  if Action.contains_concurrent action
  then Code_error.raise "concurrent action passed to dune shell replay" [];
  (match Action.find_extension_name action with
   | None -> ()
   | Some name ->
     Code_error.raise
       "action extension passed to dune shell replay"
       [ "extension", Dyn.string name ]);
  Dtemp.with_temp_dir_for_shell temp_dir ~f:(fun () ->
    let build_deps (_ : Dep.Set.t) =
      Code_error.raise "dynamic dependencies in a static dune shell replay" []
    in
    let ectx =
      let metadata =
        Process_metadata.create ~purpose:(Process_metadata.Build_job (Some targets)) ()
      in
      { targets = Some targets; metadata; context = None; rule_loc; build_deps }
    in
    let eenv =
      { working_dir = dir
      ; env
      ; stdout_to = Process.Io.inherit_stdout
      ; stderr_to = Process.Io.inherit_stderr
      ; stdin_from = Process.Io.null In
      ; prepared_dependencies = Dependency.Set.empty
      ; exit_codes = Predicate.create (Int.equal 0)
      }
    in
    let open Fiber.O in
    let* () = prepare_chdirs action in
    Fiber.collect_errors (fun () ->
      exec_until_all_deps_ready action ~ectx ~eenv ~mode:Shell_replay)
    >>= function
    | Ok _ -> Fiber.return 0
    | Error [ { Exn_with_backtrace.exn = Shell_replay_failed status; _ } ] ->
      Fiber.return (Process.Failure_mode.exit_code_of_raw_status status)
    | Error errors -> Fiber.reraise_all errors)
;;
