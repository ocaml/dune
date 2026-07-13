open Import
open Memo.O

(* Preparation of a rule action for an interactive [dune shell] session. The
   machinery here reuses the ordinary rule execution pipeline ([dune_engine]
   [Build_system]): it evaluates the rule's action, selects the sandbox mode,
   removes the declared targets, and prepares the sandbox and locks. The
   action itself is not executed; instead, [f] is run in the prepared
   environment. *)

let validate_action ~loc (action : Action.Full.t) =
  if Action.is_dynamic action.action
  then
    User_error.raise
      ~loc
      [ Pp.text "dune shell does not support dynamic actions."
      ; Pp.text
          "The action can discover dependencies while it runs, after the shell \
           environment has already been prepared."
      ];
  match Action.find_extension_name action.action with
  | None -> ()
  | Some name ->
    User_error.raise
      ~loc
      [ Pp.textf "dune shell does not support the action extension %S." name
      ; Pp.text "Use an ordinary build, or rewrite the rule with standard actions."
      ]
;;

let map_targets sandbox (targets : Targets.Validated.t) =
  let files, dirs =
    Targets.Validated.fold
      targets
      ~init:(Path.Build.Set.empty, Path.Build.Set.empty)
      ~file:(fun path (files, dirs) ->
        Path.Build.Set.add files (Sandbox.map_path sandbox path), dirs)
      ~dir:(fun path (files, dirs) ->
        files, Path.Build.Set.add dirs (Sandbox.map_path sandbox path))
  in
  match Targets.create ~files ~dirs |> Targets.validate with
  | Valid targets -> targets
  | No_targets | Inconsistent_parent_dir | File_and_directory_target_with_the_same_name _
    -> Code_error.raise "mapping dune shell targets produced invalid targets" []
;;

(* The leading wrappers the action interpreter executes before the action's
   "body": [Chdir] and [Setenv] determine the working directory and
   environment the body runs in, while the remaining wrappers only affect the
   body's streams or failure handling. Single-element [Progn]s are
   transparent. Returns the body together with the directory and environment
   in effect at that point. *)
let rec leading_context action ~dir ~env =
  match action with
  | Action.Chdir (dir, action) -> leading_context action ~dir ~env
  | Setenv (var, value, action) ->
    leading_context action ~dir ~env:(Env.add env ~var ~value)
  | With_accepted_exit_codes (_, action)
  | Redirect_out (_, _, _, action)
  | Redirect_in (_, _, action)
  | Ignore (_, action)
  | Progn [ action ] -> leading_context action ~dir ~env
  | _ -> action, dir, env
;;

type t =
  { dir : Path.t
  ; shell_env : Env.t
  ; replay_env : Env.t
  ; sandbox_dir : Path.Build.t option
  ; sandbox_mode : Sandbox_mode.some option
  ; action : Action.t
  ; targets : Targets.Validated.t
  ; rule_digest : Digest.t
  }

type direct_process =
  { program : Path.t
  ; args : string list
  ; dir : Path.t
  ; env : Env.t
  }

let bash = lazy (Bin.which ~path:(Env_path.path Env.initial) "bash")

(* If the prepared action is a single process invocation modulo its leading
   wrappers, return that direct process: its program, arguments, and the
   working directory and environment the action interpreter would run it
   with. *)
let direct_process (shell : t) =
  let body, dir, env =
    leading_context shell.action ~dir:shell.dir ~env:shell.replay_env
  in
  match body with
  | Run { prog = Ok program; args; can_run_in_action_runner = _ } ->
    Some { program; args = Appendable_list.to_list args; dir; env }
  | Run { prog = Error _; _ } -> None
  | Bash { script; can_run_in_action_runner = _ } ->
    Option.map (Lazy.force bash) ~f:(fun program ->
      { program; args = [ "-e"; "-u"; "-o"; "pipefail"; "-c"; script ]; dir; env })
  | _ -> None
;;

let with_ (rule : Rule.t) ~f =
  let { Rule.targets = original_targets; loc; _ } = rule in
  let* full_action, facts, execution_parameters =
    Build_system.evaluate_rule_action rule
  in
  validate_action ~loc full_action;
  let sandbox_mode =
    Build_system.select_sandbox_mode
      ~loc
      full_action.sandbox
      ~sandboxing_preference:(Build_config.get ()).sandboxing_preference
  in
  (match sandbox_mode with
   | Some Patch_back_source_tree ->
     User_error.raise
       ~loc
       [ Pp.text "dune shell does not support patch-back-source-tree rules."
       ; Pp.text
           "That mode performs implicit source-tree updates, whose shell semantics are \
            not defined."
       ]
   | None | Some (Copy | Symlink | Hardlink) -> ());
  let rule_digest =
    Build_system.compute_rule_digest
      rule
      ~facts
      ~action:full_action
      ~sandbox_mode
      ~execution_parameters
  in
  Memo.of_non_reproducible_fiber
    (let open Fiber.O in
     Path.mkdir_p (Path.build original_targets.root);
     let* () = Build_system.remove_rule_targets original_targets in
     Build_system.with_prepared_action_for_rule
       ~rule_digest
       ~action:full_action
       ~facts
       ~loc
       ~execution_parameters
       ~sandbox_mode
       ~targets:original_targets
       ~f:(fun { sandbox; action; root; env; _ } ->
         let targets = map_targets sandbox original_targets in
         let base_env = Action_exec.prepare_env ~root ~env execution_parameters in
         let replay_env = Dtemp.add_to_env base_env in
         let sandbox_dir =
           Option.some_if
             (Sandbox.is_sandboxed sandbox)
             (Sandbox.map_path sandbox Path.Build.root)
         in
         let _, dir, shell_env =
           leading_context
             action
             ~dir:(Path.build (Sandbox.map_path sandbox original_targets.root))
             ~env:base_env
         in
         let shell_env = Dtemp.add_to_env shell_env in
         f
           { dir
           ; shell_env
           ; replay_env
           ; sandbox_dir
           ; sandbox_mode
           ; action
           ; targets
           ; rule_digest
           }))
;;
