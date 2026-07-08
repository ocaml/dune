open Import

let name = Action_runner_name.of_string "action-runner"
let dune_prog () = Util.resolve_program_path Sys.executable_name

module Backend = struct
  type t =
    | Bwrap
    | Landlock

  let equal a b =
    match a, b with
    | Bwrap, Bwrap | Landlock, Landlock -> true
    | Bwrap, Landlock | Landlock, Bwrap -> false
  ;;

  let all = [ Bwrap; Landlock ]

  let to_string = function
    | Bwrap -> "bwrap"
    | Landlock -> "landlock"
  ;;

  let parse = function
    | "bwrap" -> Some Bwrap
    | "landlock" -> Some Landlock
    | _ -> None
  ;;

  let conv =
    let parser s =
      match parse s with
      | Some b -> Ok b
      | None -> Error (`Msg (Printf.sprintf "unknown sandbox backend %S" s))
    in
    let printer fmt b = Format.pp_print_string fmt (to_string b) in
    Arg.conv (parser, printer)
  ;;

  let available = function
    | Bwrap -> Bwrap.available ()
    | Landlock -> Landlock.available ()
  ;;

  let unavailable_reason = function
    | Bwrap -> Bwrap.unavailable_reason ()
    | Landlock ->
      [ Pp.text
          "Landlock is unavailable to this Dune binary: requires Dune to be built with \
           Landlock headers and run on a Linux kernel with the Landlock LSM enabled and \
           a sufficient ABI version"
      ]
  ;;
end

module Backends = struct
  (* Canonicalise into chain order (outermost first), deduplicated. The chosen
     layering is bwrap outer / landlock inner regardless of CLI order. *)
  let canonicalise bs =
    List.filter Backend.all ~f:(fun b -> List.mem ~equal:Backend.equal bs b)
  ;;

  let resolve ~sandbox_actions ~requested =
    if not sandbox_actions
    then (
      match requested with
      | [] -> []
      | _ :: _ ->
        Code_error.raise
          "sandbox backends were requested without sandbox_actions"
          [ "requested", Dyn.list (fun b -> Dyn.string (Backend.to_string b)) requested ])
    else (
      match requested with
      | _ :: _ as bs ->
        List.iter bs ~f:(fun b ->
          if not (Backend.available b)
          then
            User_error.raise
              ([ Pp.textf
                   "Sandbox backend %s requested via --sandbox-actions-backend but \
                    unavailable."
                   (Backend.to_string b)
               ]
               @ Backend.unavailable_reason b));
        canonicalise bs
      | [] ->
        (* With the current cache-only policy, bwrap is the least surprising
           default when it works: it preserves the incumbent namespace isolation
           without Landlock's ancestor-directory write denial. If a later
           per-action policy needs Landlock's stronger semantics, revisit this
           ordering. *)
        (match Backend.available Landlock, Backend.available Bwrap with
         | _, true -> [ Bwrap ]
         | true, false -> [ Landlock ]
         | false, false ->
           User_error.raise
             [ Pp.text
                 "--sandbox-actions was requested but no sandbox backend is available on \
                  this system. Use --sandbox-actions-backend to request a specific \
                  backend and see the diagnostic, or omit --sandbox-actions."
             ]))
  ;;

  let landlock_wrap ~dune_prog argv =
    let dune = Path.to_string dune_prog in
    let argv = dune :: "internal" :: "with-landlock" :: "--" :: argv in
    dune, argv
  ;;

  let bwrap_wrap argv =
    let { Bwrap.prog; argv } =
      Bwrap.wrap ~cwd:(Path.to_absolute_filename Path.root) argv
    in
    prog, argv
  ;;

  let compose t ~dune_prog ~worker_argv =
    (* Fold from innermost to outermost: reverse the chain, then wrap step by
       step so the first element of [t] ends up as the outermost. Each wrapper
       must preserve [argv.(0)] as the program that it execs. *)
    List.fold_right
      t
      ~init:(Path.to_string dune_prog, worker_argv)
      ~f:(fun b (_, argv) ->
        match b with
        | Backend.Landlock -> landlock_wrap ~dune_prog argv
        | Backend.Bwrap -> bwrap_wrap argv)
  ;;
end

let create ~where ~config ~sandbox_actions ~sandbox_actions_backends =
  let backends = Backends.resolve ~sandbox_actions ~requested:sandbox_actions_backends in
  let pid =
    let env =
      let jobs =
        let scheduler_config =
          Dune_config.for_scheduler config ~watch_exclusions:[] ~print_ctrl_c_warning:true
        in
        Int.to_string scheduler_config.concurrency
      in
      Env.add Env.initial ~var:"DUNE_JOBS" ~value:jobs
      |> Env.add ~var:"DUNE_BUILD_DIR" ~value:(Path.Build.to_string Path.Build.root)
      |> Env.to_unix
      |> Spawn.Env.of_list
    in
    let trace_fd = Dune_trace.duplicate_global_fd () in
    let dune_prog = dune_prog () in
    let worker_argv =
      [ Path.to_string dune_prog
      ; "internal"
      ; "action-runner"
      ; "start"
      ; Action_runner_name.to_string name
      ]
    in
    let prog, argv = Backends.compose backends ~dune_prog ~worker_argv in
    let argv =
      let where = Dune_rpc.Where.to_string where in
      argv
      @ [ where ]
      @
      match trace_fd with
      | Some fd -> [ "--trace-fd"; Int.to_string (Fd.unsafe_to_int fd) ]
      | None -> []
    in
    Exn.protect
      ~f:(fun () -> Spawn.spawn ~env ~prog ~argv ())
      ~finally:(fun () -> Option.iter trace_fd ~f:Fd.close)
    |> Pid.of_int_exn
  in
  Dune_engine.Action_runner.create name pid
;;

let parse_where where =
  match
    Dune_rpc.Conv.of_sexp
      Dune_rpc.Where.sexp
      ~version:Dune_rpc.Version.latest
      (Sexp.Atom where)
  with
  | Ok where -> where
  | Error err ->
    User_error.raise
      [ Pp.textf "invalid action runner RPC address %S" where
      ; Pp.text (Dyn.to_string (Dune_rpc.Conv.dyn_of_error err))
      ]
;;

let inherit_trace_fd ~name trace_fd =
  Option.iter trace_fd ~f:(fun fd ->
    if Sys.win32 then Code_error.raise "trace fd handoff is not supported on Windows" [];
    Dune_trace.set_global_inherited_fd
      ~common_args:[ "action_runner", Sexp.Atom (Action_runner_name.to_string name) ]
      (Fd.unsafe_of_int (Int.of_string_exn fd)))
;;

let start_worker ~name ~where ~trace_fd =
  let name = Action_runner_name.parse_string_exn (Loc.none, name) in
  inherit_trace_fd ~name trace_fd;
  let where = parse_where where in
  Dune_engine.Action_runner_worker.start ~name ~where
;;
