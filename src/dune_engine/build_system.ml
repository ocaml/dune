open Import
open Memo.O
module Error = Build_system_error

module Progress = struct
  type t =
    { number_of_rules_discovered : int
    ; number_of_rules_executed : int
    ; number_of_rules_failed : int
    }

  let equal
    { number_of_rules_discovered; number_of_rules_executed; number_of_rules_failed }
    t
    =
    Int.equal number_of_rules_discovered t.number_of_rules_discovered
    && Int.equal number_of_rules_executed t.number_of_rules_executed
    && Int.equal number_of_rules_failed t.number_of_rules_failed
  ;;

  let init =
    { number_of_rules_discovered = 0
    ; number_of_rules_executed = 0
    ; number_of_rules_failed = 0
    }
  ;;
end

module State = struct
  module Svar = Fiber.Svar

  type t =
    | Initializing
    | Building of Progress.t
    | Restarting_current_build
    | Build_succeeded__now_waiting_for_changes
    | Build_failed__now_waiting_for_changes

  let equal x y =
    match x, y with
    | Building x, Building y -> Progress.equal x y
    | Initializing, Initializing
    | Restarting_current_build, Restarting_current_build
    | Build_succeeded__now_waiting_for_changes, Build_succeeded__now_waiting_for_changes
    | Build_failed__now_waiting_for_changes, Build_failed__now_waiting_for_changes -> true
    | Building _, _
    | Initializing, _
    | Restarting_current_build, _
    | Build_succeeded__now_waiting_for_changes, _
    | Build_failed__now_waiting_for_changes, _ -> false
  ;;

  let t = Fiber.Svar.create Initializing

  (* This mutable table is safe: it maps paths to lazily created mutexes. *)
  let locks : (Path.t, Fiber.Mutex.t) Table.t = Table.create (module Path) 32

  (* This mutex ensures that at most one [run] is running in parallel. *)
  let build_mutex = Fiber.Mutex.create ()
  let reset_progress () = Svar.write t (Building Progress.init)
  let set what = Svar.write t what

  let update_build_progress_exn ~f =
    let current = Svar.read t in
    match current with
    | Building current -> Svar.write t @@ Building (f current)
    | _ -> assert false
  ;;

  let incr_rule_done_exn () =
    update_build_progress_exn ~f:(fun p ->
      { p with number_of_rules_executed = p.number_of_rules_executed + 1 })
  ;;

  let start_rule_exn () =
    update_build_progress_exn ~f:(fun p ->
      { p with number_of_rules_discovered = p.number_of_rules_discovered + 1 })
  ;;

  let errors = Svar.create Error.Set.empty
  let reset_errors () = Svar.write errors Error.Set.empty

  let add_errors error_list =
    let open Fiber.O in
    let* () =
      update_build_progress_exn ~f:(fun p ->
        { p with number_of_rules_failed = p.number_of_rules_failed + 1 })
    in
    List.fold_left error_list ~init:(Svar.read errors) ~f:Error.Set.add
    |> Svar.write errors
  ;;
end

let rec with_locks ~f = function
  | [] -> f ()
  | m :: mutexes ->
    Fiber.Mutex.with_lock
      (Table.find_or_add State.locks m ~f:(fun _ -> Fiber.Mutex.create ()))
      ~f:(fun () -> with_locks ~f mutexes)
;;

module Pending_targets = struct
  (* All file and directory targets of non-sandboxed actions that are currently
     being executed. On exit, we need to delete them as they might contain
     garbage. *)

  let t = ref Targets.empty
  let remove targets = t := Targets.diff !t (Targets.Validated.unvalidate targets)
  let add targets = t := Targets.combine !t (Targets.Validated.unvalidate targets)

  let () =
    Hooks.End_of_build.always (fun () ->
      let targets = !t in
      t := Targets.empty;
      Targets.iter targets ~file:Path.Build.unlink_no_err ~dir:(fun p ->
        Path.rm_rf (Path.build p)))
  ;;
end

let () = Hooks.End_of_build.always Metrics.reset

type rule_execution_result =
  { facts : Dep.Fact.t Dep.Map.t
  ; targets : Digest.t Targets.Produced.t
  }

module type Rec = sig
  (** Build all the transitive dependencies of the alias and return the alias
      expansion. *)
  val build_alias : Alias.t -> Dep.Fact.Files.t Memo.t

  val build_file : Path.t -> Digest.t Memo.t
  val build_dir : Path.t -> Digest.t Targets.Produced.t Memo.t
  val build_dep : Dep.t -> Dep.Fact.t Memo.t
  val build_deps : Dep.Set.t -> Dep.Facts.t Memo.t
  val execute_rule : Rule.t -> rule_execution_result Memo.t

  val execute_action
    :  observing_facts:Dep.Facts.t
    -> Rule.Anonymous_action.t
    -> unit Memo.t

  val execute_action_stdout
    :  observing_facts:Dep.Facts.t
    -> Rule.Anonymous_action.t
    -> string Memo.t

  val eval_pred : File_selector.t -> Filename_set.t Memo.t
end

(* Separation between [Used_recursively] and [Exported] is necessary because at
   least one module in the recursive module group must be pure (i.e. only expose
   functions). *)
module rec Used_recursively : Rec = Exported

and Exported : sig
  include Rec

  val execute_rule : Rule.t -> rule_execution_result Memo.t

  type target_kind =
    | File_target
    | Dir_target of { targets : Digest.t Targets.Produced.t }

  (* The below two definitions are useless, but if we remove them we get an
     "Undefined_recursive_module" exception. *)

  val build_file_memo : (Path.t, Digest.t * target_kind) Memo.Table.t Lazy.t
  [@@warning "-32"]

  val build_alias_memo : (Alias.t, Dep.Fact.Files.t) Memo.Table.t [@@warning "-32"]
  val dep_on_alias_definition : Rules.Dir_rules.Alias_spec.item -> unit Action_builder.t
end = struct
  open Used_recursively

  let file_selector_stack_frame_description file_selector =
    Pp.concat [ Pp.text (File_selector.to_dyn file_selector |> Dyn.to_string) ]
  ;;

  let build_file_selector : File_selector.t -> Dep.Fact.t Memo.t =
    let impl file_selector =
      let* files = eval_pred file_selector in
      let+ fact = Dep.Fact.Files.create files ~build_file in
      (* Fact: [file_selector] expands to the set of [files] whose digests are captured
         via [build_file]; also, the [File_selector.dir] exists (though it may be empty) *)
      Dep.Fact.file_selector file_selector fact
    in
    let memo =
      Memo.create
        "build_file_selector"
        ~input:(module File_selector)
        ~cutoff:Dep.Fact.equal
          (* CR-someday amokhov: Pass [file_selector_stack_frame_description] here to
             include globs into stack traces. *)
        ?human_readable_description:None
        impl
    in
    fun file_selector -> Memo.exec memo file_selector
  ;;

  (* [build_dep] turns a [Dep.t] which is a description of a dependency into a
     fact about the world. To do that, it needs to do some building. *)
  let build_dep : Dep.t -> Dep.Fact.t Memo.t = function
    | Alias a ->
      let+ digests = build_alias a in
      (* Fact: alias [a] expands to the set of file-digest pairs [digests] *)
      Dep.Fact.alias a digests
    | File f ->
      let+ digest = build_file f in
      (* Fact: file [f] has digest [digest] *)
      Dep.Fact.file f digest
    | File_selector file_selector -> build_file_selector file_selector
    | Universe | Env _ ->
      (* Facts about these dependencies are constructed in
         [Dep.Facts.digest]. *)
      Memo.return Dep.Fact.nothing
  ;;

  let build_deps deps = Dep.Map.parallel_map deps ~f:(fun dep () -> build_dep dep)

  let select_sandbox_mode (config : Sandbox_config.t) ~loc ~sandboxing_preference =
    (* Rules with (mode patch-back-source-tree) are special and are not affected
       by sandboxing preferences. *)
    match Sandbox_mode.Set.is_patch_back_source_tree_only config with
    | true -> Some Sandbox_mode.Patch_back_source_tree
    | false ->
      let evaluate_sandboxing_preference preference =
        match Sandbox_mode.Set.mem config preference with
        | false -> None
        | true -> Some preference
      in
      (match List.find_map sandboxing_preference ~f:evaluate_sandboxing_preference with
       | Some choice -> choice
       | None ->
         (* This is not trivial to reach because the user rules are checked at
            parse time and [sandboxing_preference] always includes all possible
            modes. However, it can still be reached if multiple sandbox config
            specs are combined into an unsatisfiable one. *)
         User_error.raise
           ~loc
           [ Pp.text
               "This rule forbids all sandboxing modes (but it also requires sandboxing)"
           ])
  ;;

  (* The current version of the rule digest scheme. We should increment it when
     making any changes to the scheme, to avoid collisions. *)
  let rule_digest_version = 21

  let compute_rule_digest
    (rule : Rule.t)
    ~facts
    ~action
    ~sandbox_mode
    ~execution_parameters
    =
    let { Action.Full.action
        ; env
        ; locks
        ; can_go_in_shared_cache
        ; sandbox = _ (* already taken into account in [sandbox_mode] *)
        }
      =
      action
    in
    let target_paths names =
      Filename.Set.to_list_map names ~f:(fun name ->
        Path.Build.relative rule.targets.root name |> Path.Build.to_string)
    in
    let trace =
      ( rule_digest_version (* Update when changing the rule digest scheme. *)
      , sandbox_mode
      , Dep.Facts.digest facts ~env
      , target_paths rule.targets.files @ target_paths rule.targets.dirs
      , Action.for_shell action
      , can_go_in_shared_cache
      , List.map locks ~f:Path.to_string
      , Execution_parameters.action_stdout_on_success execution_parameters
      , Execution_parameters.action_stderr_on_success execution_parameters
      , Execution_parameters.workspace_root_to_build_path_prefix_map execution_parameters
      , Execution_parameters.expand_aliases_in_sandbox execution_parameters )
    in
    Digest.generic trace
  ;;

  let report_evaluated_rule_exn (t : Build_config.t) =
    Option.iter t.stats ~f:(fun stats ->
      let module Event = Chrome_trace.Event in
      let event =
        let rule_total =
          match Fiber.Svar.read State.t with
          | Building progress -> progress.number_of_rules_discovered
          | _ -> assert false
        in
        let args = [ "value", `Int rule_total ] in
        let ts = Event.Timestamp.of_float_seconds (Unix.gettimeofday ()) in
        let common = Event.common_fields ~name:"evaluated_rules" ~ts () in
        Event.counter common args
      in
      Dune_stats.emit stats event)
  ;;

  module Exec_result = struct
    type t =
      { produced_targets : unit Targets.Produced.t
      ; action_exec_result : Action_exec.Exec_result.ok
      }
  end

  type rule_kind =
    | Normal_rule
    | Anonymous_action of
        { stamp_file : Path.Build.t
        ; capture_stdout : bool
        ; attached_to_alias : bool
        }

  let maybe_async_rule_file_op f =
    (* It would be nice to do this check only once and return a function, but the
       type of this function would need to be polymorphic which is forbidden by
       the relaxed value restriction. *)
    match Config.(get background_file_system_operations_in_rule_execution) with
    | `Enabled -> Scheduler.async_exn f
    | `Disabled -> Fiber.return (f ())
  ;;

  let execute_action_for_rule
    ~rule_kind
    ~rule_digest
    ~action
    ~facts
    ~loc
    ~execution_parameters
    ~sandbox_mode
    ~(targets : Targets.Validated.t)
    : Exec_result.t Fiber.t
    =
    let open Fiber.O in
    let { Action.Full.action; env; locks; can_go_in_shared_cache = _; sandbox = _ } =
      action
    in
    let* dune_stats = Scheduler.stats () in
    let deps =
      Dep.Facts.paths
        ~expand_aliases:
          (Execution_parameters.expand_aliases_in_sandbox execution_parameters)
        facts
    in
    let* sandbox =
      match sandbox_mode with
      | Some mode ->
        let+ sandbox =
          Sandbox.create
            ~mode
            ~dirs:(Dep.Facts.necessary_dirs_for_sandboxing facts)
            ~deps
            ~rule_dir:targets.root
            ~rule_loc:loc
            ~rule_digest
            ~dune_stats
        in
        Some sandbox
      | None ->
        (* If the action is not sandboxed, we use [pending_file_targets] to
           clean up the build directory if the action is interrupted. *)
        Pending_targets.add targets;
        Fiber.return None
    in
    let action =
      match sandbox with
      | None -> action
      | Some sandbox -> Action.sandbox action sandbox
    in
    let action =
      (* We must add the creation of the stamp file after sandboxing it, as
         otherwise the stamp file would end up inside the sandbox. This is
         especially a problem for the [Patch_back_source_tree] sandboxing
         mode. *)
      match rule_kind with
      | Normal_rule -> action
      | Anonymous_action { stamp_file; capture_stdout; _ } ->
        if capture_stdout
        then Action.with_stdout_to stamp_file action
        else Action.progn [ action; Action.write_file stamp_file "" ]
    in
    let* () =
      maybe_async_rule_file_op (fun () ->
        Action.chdirs action
        |> Path.Build.Set.iter ~f:(fun p -> Path.mkdir_p (Path.build p)))
    in
    let context = Build_context.of_build_path targets.root in
    let root =
      match context with
      | None -> Path.Build.root
      | Some context -> context.build_dir
    in
    let root =
      Path.build
        (match sandbox with
         | None -> root
         | Some sandbox -> Sandbox.map_path sandbox root)
    in
    Fiber.finalize
      ~finally:(fun () ->
        match sandbox with
        | Some sandbox -> Sandbox.destroy sandbox
        | None ->
          Pending_targets.remove targets;
          Fiber.return ())
      (fun () ->
        with_locks locks ~f:(fun () ->
          let* action_exec_result =
            let input =
              { Action_exec.root
              ; context (* can be derived from the root *)
              ; env
              ; targets = Some targets
              ; rule_loc = loc
              ; execution_parameters
              ; action
              }
            in
            match (Build_config.get ()).action_runner input with
            | None ->
              let build_deps deps = Memo.run (build_deps deps) in
              Action_exec.exec input ~build_deps
            | Some runner -> Action_runner.exec_action runner input
          in
          let* action_exec_result = Action_exec.Exec_result.ok_exn action_exec_result in
          let* () =
            match sandbox with
            | None -> Fiber.return ()
            | Some sandbox ->
              (* The stamp file for anonymous actions is always created outside
                 the sandbox, so we can't move it. *)
              let should_be_skipped =
                match rule_kind with
                | Normal_rule -> fun (_ : Path.Build.t) -> false
                | Anonymous_action { stamp_file; _ } -> Path.Build.equal stamp_file
              in
              Sandbox.move_targets_to_build_dir sandbox ~should_be_skipped ~targets
          in
          let+ produced_targets =
            maybe_async_rule_file_op (fun () -> Targets.Produced.of_validated targets)
          in
          match produced_targets with
          | Ok produced_targets -> { Exec_result.produced_targets; action_exec_result }
          | Error error -> User_error.raise ~loc (Targets.Produced.Error.message error)))
  ;;

  let promote_targets ~rule_mode ~targets ~promote_source =
    match rule_mode, !Clflags.promote with
    | (Rule.Mode.Standard | Fallback | Ignore_source_files), _ | Promote _, Some Never ->
      Fiber.return ()
    | Promote promote, (Some Automatically | None) ->
      Target_promotion.promote ~targets ~promote ~promote_source
  ;;

  let execute_rule_impl ~rule_kind rule =
    let { Rule.id = _; targets; mode; action; info = _; loc } = rule in
    (* We run [State.start_rule_exn ()] entirely for its side effect, so one
       might be tempted to use [Memo.of_non_reproducible_fiber] here but that is
       wrong, because that would force us to rerun [execute_rule_impl] on every
       incremental build. *)
    let* () = Memo.of_reproducible_fiber (State.start_rule_exn ()) in
    let head_target = Targets.Validated.head targets in
    let* execution_parameters =
      match Dpath.Target_dir.of_target targets.root with
      | Regular (With_context (_, _)) | Anonymous_action (With_context (_, _)) ->
        (Build_config.get ()).execution_parameters ~dir:targets.root
      | Anonymous_action Root | Regular Root | Invalid _ ->
        Code_error.raise
          "invalid dir for rule execution"
          [ "dir", Path.Build.to_dyn targets.root ]
    in
    (* Note: we do not run the below in parallel with the above: if we fail to
       compute action execution parameters, we have no use for the action and
       might as well fail early, skipping unnecessary dependencies. The
       function [(Build_config.get ()).execution_parameters] is likely
       memoized, and the result is not expected to change often, so we do not
       sacrifice too much performance here by executing it sequentially. *)
    let* action, facts = Action_builder.evaluate_and_collect_facts action in
    let wrap_fiber f =
      Memo.of_reproducible_fiber
        (if Loc.is_none loc
         then f ()
         else
           Fiber.with_error_handler f ~on_error:(fun exn ->
             match exn.exn with
             | User_error.E msg when not (User_message.has_location msg) ->
               let msg = { msg with loc = Some loc } in
               Exn_with_backtrace.reraise { exn with exn = User_error.E msg }
             | _ -> Exn_with_backtrace.reraise exn))
    in
    let config = Build_config.get () in
    wrap_fiber (fun () ->
      let open Fiber.O in
      report_evaluated_rule_exn config;
      let* () =
        maybe_async_rule_file_op (fun () -> Path.mkdir_p (Path.build targets.root))
      in
      let is_action_dynamic = Action.is_dynamic action.action in
      let sandbox_mode =
        select_sandbox_mode
          ~loc
          action.sandbox
          ~sandboxing_preference:config.sandboxing_preference
      in
      (* CR-someday amokhov: More [always_rerun] and [can_go_in_shared_cache]
         to [Rule_cache] too. *)
      let always_rerun =
        let is_test =
          (* jeremiedimino: what about:

             {v (rule (alias runtest) (targets x) (action ...)) v}

             These will be treated as [Normal_rule], and the below match means
             that [--force] will have no effect on them. Is that what we want?

             The doc says:

             -f, --force Force actions associated to aliases to be re-executed
             even if their dependencies haven't changed.

             So it seems to me that such rules should be re-executed. TBC *)
          match rule_kind with
          | Normal_rule -> false
          | Anonymous_action a -> a.attached_to_alias
        in
        let force_rerun = !Clflags.force && is_test in
        force_rerun || Dep.Map.has_universe facts
      in
      let rule_digest =
        compute_rule_digest rule ~facts ~action ~sandbox_mode ~execution_parameters
      in
      let can_go_in_shared_cache =
        action.can_go_in_shared_cache
        && (not
              (always_rerun
               || is_action_dynamic
               || Action.is_useful_to_memoize action.action = Clearly_not))
        &&
        match sandbox_mode with
        | Some Patch_back_source_tree ->
          (* Action in this mode cannot go in the shared cache *)
          false
        | _ -> true
      in
      let* (produced_targets : Digest.t Targets.Produced.t) =
        (* Step I. Check if the workspace-local cache is up to date. *)
        Rule_cache.Workspace_local.lookup
          ~always_rerun
          ~rule_digest
          ~targets
          ~env:action.env
          ~build_deps
        >>= function
        | Some produced_targets -> Fiber.return produced_targets
        | None ->
          (* Step II. Remove stale targets both from the digest table and from
             the build directory. *)
          let () =
            Targets.Validated.iter
              targets
              ~file:Cached_digest.remove
              ~dir:Cached_digest.remove
          in
          let* () =
            maybe_async_rule_file_op (fun () ->
              let remove_target_dir dir = Path.rm_rf (Path.build dir) in
              let remove_target_file path =
                match Path.Build.unlink path with
                | Success -> ()
                | Does_not_exist -> ()
                | Is_a_directory ->
                  (* If target changed from a directory to a file, delete
                     in anyway. *)
                  remove_target_dir path
                | Error exn ->
                  Log.info
                    [ Pp.textf
                        "Error while removing target %s: %s"
                        (Path.Build.to_string path)
                        (Printexc.to_string exn)
                    ]
              in
              Targets.Validated.iter
                targets
                ~file:remove_target_file
                ~dir:remove_target_dir)
          in
          let* produced_targets, dynamic_deps_stages =
            (* Step III. Try to restore artifacts from the shared cache. *)
            Rule_cache.Shared.lookup ~can_go_in_shared_cache ~rule_digest ~targets
            >>= function
            | Some produced_targets ->
              (* Rules with dynamic deps can't be stored to the shared cache
                 (see the [is_action_dynamic] check above), so we know this is
                 not a dynamic action, so returning an empty list is correct.
                 The lack of information to fill in [dynamic_deps_stages] here
                 is precisely the reason why we don't store dynamic actions in
                 the shared cache. *)
              let dynamic_deps_stages = [] in
              Fiber.return (produced_targets, dynamic_deps_stages)
            | None ->
              (* Step IV. Execute the build action. *)
              let* exec_result =
                execute_action_for_rule
                  ~rule_kind
                  ~rule_digest
                  ~action
                  ~facts
                  ~loc
                  ~execution_parameters
                  ~sandbox_mode
                  ~targets
              in
              (* Step V. Examine produced targets and store them to the shared
                 cache if needed. *)
              let* produced_targets =
                Rule_cache.Shared.examine_targets_and_store
                  ~can_go_in_shared_cache
                  ~loc
                  ~rule_digest
                  ~execution_parameters
                  ~produced_targets:exec_result.produced_targets
                  ~action:action.action
              in
              let dynamic_deps_stages =
                List.map
                  exec_result.action_exec_result.dynamic_deps_stages
                  ~f:(fun (deps, fact_map) ->
                    deps, Dep.Facts.digest fact_map ~env:action.env)
              in
              Fiber.return (produced_targets, dynamic_deps_stages)
          in
          (* We do not include target names into [targets_digest] because they
             are already included into the rule digest. *)
          Rule_cache.Workspace_local.store
            ~head_target
            ~rule_digest
            ~dynamic_deps_stages
            ~targets_digest:(Targets.Produced.digest produced_targets);
          Fiber.return produced_targets
      in
      let* () =
        promote_targets
          ~rule_mode:mode
          ~targets:produced_targets
          ~promote_source:config.promote_source
      in
      let+ () = State.incr_rule_done_exn () in
      produced_targets)
    (* jeremidimino: We need to include the dependencies discovered while
       running the action here. Otherwise, package dependencies are broken in
       the presence of dynamic actions. *)
    >>| fun produced_targets -> { facts; targets = produced_targets }
  ;;

  module Anonymous_action = struct
    type t =
      { action : Rule.Anonymous_action.t
      ; deps : Dep.Set.t
      ; capture_stdout : bool
      ; digest : Digest.t
      }

    let equal a b = Digest.equal a.digest b.digest
    let hash t = Digest.hash t.digest

    let to_dyn t : Dyn.t =
      Record [ "digest", Digest.to_dyn t.digest; "loc", Loc.to_dyn t.action.loc ]
    ;;
  end

  (* Returns the action's stdout or the empty string if [capture_stdout = false]. *)
  let execute_action_generic_stage2_impl
    { Anonymous_action.action = act; deps; capture_stdout; digest }
    =
    let target =
      let dir =
        Path.Build.append_local
          Dpath.Build.anonymous_actions_dir
          (Path.Build.local act.dir)
      in
      let d = Digest.to_string digest in
      let basename =
        match act.alias with
        | None -> d
        | Some a -> Alias.Name.to_string a ^ "-" ^ d
      in
      Path.Build.relative dir basename
    in
    let rule =
      let { Rule.Anonymous_action.action = _; loc; dir = _; alias = _ } = act in
      Rule.make
        ~info:(if Loc.is_none loc then Internal else From_dune_file loc)
        ~targets:(Targets.File.create target)
        ~mode:Standard
        (Action_builder.record act.action deps ~f:build_dep)
    in
    let+ { facts = _; targets = _ } =
      execute_rule_impl
        rule
        ~rule_kind:
          (Anonymous_action
             { attached_to_alias = Option.is_some act.alias
             ; capture_stdout
             ; stamp_file = target
             })
    in
    if capture_stdout then Io.read_file (Path.build target) else ""
  ;;

  let execute_action_generic_stage2_memo =
    Memo.create
      "execute-action"
      ~input:(module Anonymous_action)
      ~cutoff:String.equal
      execute_action_generic_stage2_impl
  ;;

  (* The current version of the action digest scheme. We should increment it when
     making any changes to the scheme, to avoid collisions. *)
  let action_digest_version = 1

  let execute_action_generic
    ~observing_facts
    (act : Rule.Anonymous_action.t)
    ~capture_stdout
    =
    (* We memoize the execution of anonymous actions, both via the persistent
       mechanism for not re-running build rules between invocations of [dune
       build] and via [Memo]. The former is done by producing a normal build
       rule on the fly for the anonymous action.

       Memoizing such actions via [Memo] doesn't feel super useful given that we
       expect a given anonymous action to be executed only once in a given
       build. And so persistent mechanism should be enough.

       However, if it does happen that two code paths try to execute the same
       anonymous action, then we need to be sure it is not executed twice. This
       is because the build rule we produce on the fly creates a file whose name
       only depend on the action. If the two execution could run concurrently,
       then they would both try to create the same file. So in this regard, we
       use [Memo] mostly for synchronisation purposes. *)
    (* Here we "forget" the facts about the world. We do that to make the input
       of the memoized function smaller. If we passed the whole [original_facts]
       as input, then we would end up memoizing one entry per set of facts. This
       could use a lot of memory. For instance, if we used [action_stdout] for
       the calls to [ocamldep], then Dune would remember the whole history of
       calls to [ocamldep] for each OCaml source file. *)
    let deps = Dep.Map.map observing_facts ~f:ignore in
    (* Shadow [observing_facts] to make sure we don't use it again. *)
    let observing_facts = () in
    ignore observing_facts;
    let digest =
      let { Rule.Anonymous_action.action =
              { action; env; locks; can_go_in_shared_cache; sandbox }
          ; loc = _
          ; dir
          ; alias
          }
        =
        act
      in
      let env =
        (* Here we restrict the environment to only the variables we depend on,
           so that we don't re-execute all actions when some irrelevant
           environment variable changes.

           Ideally, we would pass this restricted environment to the external
           command, however that might be tedious to do in practice. See this
           ticket for a longer discussion about the management of the
           environment: https://github.com/ocaml/dune/issues/4382 *)
        Dep.Set.fold deps ~init:Env.Map.empty ~f:(fun dep acc ->
          match dep with
          | Env var -> Env.Map.set acc var (Env.get env var)
          | _ -> acc)
        |> Env.Map.to_list
      in
      Digest.generic
        ( action_digest_version
        , env
        , Dep.Set.digest deps
        , Action.for_shell action
        , List.map locks ~f:Path.to_string
        , dir
        , alias
        , capture_stdout
        , can_go_in_shared_cache
        , sandbox )
    in
    (* It might seem superfluous to memoize the execution here, given that a
       given anonymous action will typically only appear once during a given
       build. However, it is possible that two code paths try to execute the
       exact same anonymous action, and so would end up trying to create the
       same file. Using [Memo.create] serves as a synchronisation point to share
       the execution and avoid such a race condition. *)
    Memo.exec
      execute_action_generic_stage2_memo
      { action = act; deps; capture_stdout; digest }
  ;;

  let execute_action ~observing_facts act =
    let+ (_empty_string : string) =
      execute_action_generic ~observing_facts act ~capture_stdout:false
    in
    ()
  ;;

  let execute_action_stdout ~observing_facts act =
    execute_action_generic ~observing_facts act ~capture_stdout:true
  ;;

  (* CR-soon amokhov: Instead of wrapping the result into a variant, [build_file_impl]
     could always return [targets : Digest.t Targets.Produced.t], and the latter could
     provide a way to conveniently check if a specific [path] is a file or a directory,
     as well as extract its digest when needed. *)
  type target_kind =
    | File_target
    | Dir_target of
        { targets :
            (* All targets of the rule which produced the directory target in question. *)
            Digest.t Targets.Produced.t
        }

  let target_kind_equal a b =
    match a, b with
    | File_target, File_target -> true
    | Dir_target { targets = a }, Dir_target { targets = b } ->
      Targets.Produced.equal a b ~equal:Digest.equal
    | File_target, Dir_target _ | Dir_target _, File_target -> false
  ;;

  (* A rule can have multiple targets but calls to [execute_rule] are memoized,
     so the rule will be executed only once. *)
  let build_file_impl path =
    Load_rules.get_rule_or_source path
    >>= function
    | Source digest -> Memo.return (digest, File_target)
    | Rule (path, rule) ->
      let+ { facts = _; targets } =
        Memo.push_stack_frame
          (fun () -> execute_rule rule)
          ~human_readable_description:(fun () ->
            Pp.text (Path.to_string_maybe_quoted (Path.build path)))
      in
      (match Targets.Produced.find targets path with
       | Some digest -> digest, File_target
       | None ->
         (match
            Path.Local.Map.find
              targets.dirs
              (Path.Build.drop_build_context_exn path |> Path.Source.to_local)
          with
          | Some _ -> Targets.Produced.digest targets, Dir_target { targets }
          | None ->
            (* CR-someday amokhov: The most important reason we end up here is
               [No_such_file]. I think some of the outcomes above are impossible
               but some others will benefit from a better error. To be refined. *)
            let target =
              Path.Build.drop_build_context_exn path |> Path.Source.to_string_maybe_quoted
            in
            let matching_dirs =
              Filename.Set.to_list_map rule.targets.dirs ~f:(fun dir ->
                (* CR-someday rleshchinskiy: This test can probably be simplified. *)
                let dir = Path.Build.relative rule.targets.root dir in
                match Path.Build.is_descendant path ~of_:dir with
                | true -> [ dir ]
                | false -> [])
              |> List.concat
            in
            let matching_target =
              match matching_dirs with
              | [ dir ] ->
                Path.Build.drop_build_context_exn dir
                |> Path.Source.to_string_maybe_quoted
              | [] | _ :: _ ->
                Code_error.raise
                  "Multiple matching directory targets"
                  [ "targets", Targets.Validated.to_dyn rule.targets ]
            in
            User_error.raise
              ~loc:rule.loc
              ~annots:
                (User_message.Annots.singleton User_message.Annots.needs_stack_trace ())
              [ Pp.textf
                  "This rule defines a directory target %S that matches the requested \
                   path %S but the rule's action didn't produce it"
                  matching_target
                  target
              ]))
  ;;

  let execute_anonymous_action action =
    let* action, facts = Action_builder.evaluate_and_collect_facts action in
    execute_action action ~observing_facts:facts
  ;;

  let dep_on_anonymous_action (action : Rule.Anonymous_action.t Action_builder.t)
    : unit Action_builder.t
    =
    Action_builder.record_success (Memo.of_thunk_apply execute_anonymous_action action)
  ;;

  let dep_on_alias_definition (definition : Rules.Dir_rules.Alias_spec.item) =
    match definition with
    | Deps x -> x
    | Action x -> dep_on_anonymous_action x
  ;;

  let build_alias_impl alias =
    let+ l =
      Load_rules.get_alias_definition alias
      >>= Memo.parallel_map ~f:(fun (loc, definition) ->
        Memo.push_stack_frame
          (fun () ->
            Action_builder.evaluate_and_collect_facts (dep_on_alias_definition definition)
            >>| snd)
          ~human_readable_description:(fun () -> Alias.describe alias ~loc))
    in
    Dep.Facts.group_paths_as_fact_files l
  ;;

  let eval_pred_impl g =
    let dir = File_selector.dir g in
    (* CR-soon amokhov: Change [Load_rules.load_dir] to return [Filename_set.t]s to save
       a bunch of set/list operations and reduce code duplication. *)
    Load_rules.load_dir ~dir
    >>= function
    | Source { filenames } | External { filenames } ->
      Filename_set.create ~dir ~filter:(File_selector.test_basename g) filenames
      |> Memo.return
    | Build { rules_here; _ } ->
      let only_generated_files = File_selector.only_generated_files g in
      (* We look only at [by_file_targets] because [File_selector] does not
         match directories. *)
      Path.Build.Map.foldi
        ~init:[]
        rules_here.by_file_targets
        ~f:(fun path { Rule.info; _ } acc ->
          match info with
          | Rule.Info.Source_file_copy _ when only_generated_files -> acc
          | _ ->
            let basename = Path.Build.basename path in
            if File_selector.test_basename g ~basename then basename :: acc else acc)
      |> Filename.Set.of_list
      |> Filename_set.create ~dir
      |> Memo.return
    | Build_under_directory_target { directory_target_ancestor = _ } ->
      (* To evaluate a glob in a generated directory, we have no choice but to build the
         whole directory and examine its contents. *)
      let+ path_map = build_dir dir in
      (match Targets.Produced.find_dir path_map (Path.as_in_build_dir_exn dir) with
       | Some files_and_digests ->
         Filename_set.create
           ~dir
           ~filter:(File_selector.test_basename g)
           (Filename.Set.of_keys files_and_digests)
       | None ->
         (* CR-soon amokhov: I think this case should be an error. If the directory target
            doesn't contain the requested dir, we will currently create an empty directory
            in the sandbox, as if it actually existed. *)
         Filename_set.empty ~dir)
  ;;

  let eval_pred_memo =
    Memo.create
      "eval-pred"
      ~human_readable_description:file_selector_stack_frame_description
      ~input:(module File_selector)
      ~cutoff:Filename_set.equal
      eval_pred_impl
  ;;

  let eval_pred = Memo.exec eval_pred_memo

  let build_file_memo =
    lazy
      (let cutoff =
         match Dune_config.Config.(get cutoffs_that_reduce_concurrency_in_watch_mode) with
         | `Disabled -> None
         | `Enabled -> Some (Tuple.T2.equal Digest.equal target_kind_equal)
       in
       Memo.create_with_store
         "build-file"
         ~store:(module Path.Table)
         ~input:(module Path)
         ?cutoff
         build_file_impl)
  ;;

  let build_file path = Memo.exec (Lazy.force build_file_memo) path >>| fst

  let build_dir path =
    let+ (_ : Digest.t), kind = Memo.exec (Lazy.force build_file_memo) path in
    match kind with
    | Dir_target { targets } -> targets
    | File_target ->
      Code_error.raise "build_dir called on a file target" [ "path", Path.to_dyn path ]
  ;;

  let build_alias_memo =
    Memo.create
      "build-alias"
      ~input:(module Alias)
      ~cutoff:Dep.Fact.Files.equal
      build_alias_impl
  ;;

  let build_alias = Memo.exec build_alias_memo

  let execute_rule_memo =
    Memo.create
      "execute-rule"
      ~input:(module Rule)
      (execute_rule_impl ~rule_kind:Normal_rule)
  ;;

  let execute_rule = Memo.exec execute_rule_memo

  let () =
    Load_rules.set_current_rule_loc (fun () ->
      let+ stack = Memo.get_call_stack () in
      List.find_map stack ~f:(fun frame ->
        match Memo.Stack_frame.as_instance_of frame ~of_:execute_rule_memo with
        | Some r -> Some (Rule.loc r)
        | None ->
          Option.bind
            (Memo.Stack_frame.as_instance_of
               frame
               ~of_:execute_action_generic_stage2_memo)
            ~f:(fun (x : Anonymous_action.t) ->
              Option.some_if (not @@ Loc.is_none x.action.loc) x.action.loc)))
  ;;
end

include Exported

(* Here we are doing a O(log |S|) lookup in a set S of files in the build
   directory [dir]. We could memoize these lookups, but it doesn't seem to be
   worth it, since we're unlikely to perform exactly the same lookup many times.
   As far as I can tell, each lookup will be done twice: when computing static
   dependencies of a [Action_builder.t] with [Action_builder.static_deps] and
   when executing the very same [Action_builder.t] with [Action_builder.exec] --
   the results of both [Action_builder.static_deps] and [Action_builder.exec]
   are cached. *)
let file_exists fn =
  Load_rules.load_dir ~dir:(Path.parent_exn fn)
  >>= function
  | Source { filenames } | External { filenames } ->
    Filename.Set.mem filenames (Path.basename fn) |> Memo.return
  | Build { rules_here; _ } ->
    Memo.return
      (Path.Build.Map.mem rules_here.by_file_targets (Path.as_in_build_dir_exn fn))
  | Build_under_directory_target { directory_target_ancestor } ->
    let+ path_map = build_dir (Path.build directory_target_ancestor) in
    Targets.Produced.mem path_map (Path.as_in_build_dir_exn fn)
;;

let files_of ~dir =
  Load_rules.load_dir ~dir
  >>= function
  | Source { filenames } | External { filenames } ->
    Memo.return (Filename_set.create ~dir filenames)
  | Build { rules_here; _ } ->
    Path.Build.Map.keys rules_here.by_file_targets
    |> Filename.Set.of_list_map ~f:Path.Build.basename
    |> Filename_set.create ~dir
    |> Memo.return
  | Build_under_directory_target { directory_target_ancestor } ->
    let+ path_map = build_dir (Path.build directory_target_ancestor) in
    let filenames =
      let dir = Path.as_in_build_dir_exn dir in
      match Targets.Produced.find_dir path_map dir with
      | Some files -> Filename.Set.of_keys files
      | None -> Filename.Set.empty
    in
    Filename_set.create ~dir filenames
;;

let caused_by_cancellation (exn : Exn_with_backtrace.t) =
  match exn.exn with
  | Scheduler.Run.Build_cancelled -> true
  | Memo.Error.E err ->
    (match Memo.Error.get err with
     | Scheduler.Run.Build_cancelled -> true
     | _ -> false)
  | _ -> false
;;

let report_early_exn exn =
  match caused_by_cancellation exn with
  | true -> Fiber.return ()
  | false ->
    let open Fiber.O in
    let errors = Error.of_exn exn in
    let+ () = State.add_errors errors
    and+ () =
      match !Clflags.stop_on_first_error with
      | true ->
        let* () =
          (Build_config.get ()).action_runners ()
          |> Fiber.parallel_iter ~f:Action_runner.cancel_build
        in
        Scheduler.stop_on_first_error ()
      | false -> Fiber.return ()
    in
    (match !Clflags.report_errors_config with
     | Early | Twice -> Dune_util.Report_error.report exn
     | Deterministic -> ())
;;

let handle_final_exns exns =
  match !Clflags.report_errors_config with
  | Early -> ()
  | Twice | Deterministic ->
    let report exn =
      if not (caused_by_cancellation exn) then Dune_util.Report_error.report exn
    in
    List.iter exns ~f:report
;;

let run f =
  let open Fiber.O in
  Hooks.End_of_build.once Diff_promotion.finalize;
  let* () = State.reset_progress () in
  let* () = State.reset_errors () in
  let f () =
    let* res =
      Fiber.collect_errors (fun () ->
        Memo.run_with_error_handler f ~handle_error_no_raise:report_early_exn)
    in
    let* () = (Build_config.get ()).write_error_summary (Fiber.Svar.read State.errors) in
    match res with
    | Ok res ->
      let+ () = State.set Build_succeeded__now_waiting_for_changes in
      Ok res
    | Error exns ->
      handle_final_exns exns;
      let final_status =
        if List.exists exns ~f:caused_by_cancellation
        then State.Restarting_current_build
        else Build_failed__now_waiting_for_changes
      in
      let+ () = State.set final_status in
      Error `Already_reported
  in
  Fiber.Mutex.with_lock State.build_mutex ~f
;;

let run_exn f =
  let open Fiber.O in
  let+ res = run f in
  match res with
  | Ok res -> res
  | Error `Already_reported -> raise Dune_util.Report_error.Already_reported
;;

let build_file p =
  let+ (_ : Digest.t) = build_file p in
  ()
;;

let with_file p ~f =
  let+ () = build_file p in
  f p
;;

(* CR-someday amokhov: Try running [Io.read_file] in a separate thread. *)
let read_file =
  Memo.exec
    (Memo.create_with_store
       "Build_system.read_file"
       ~store:(module Path.Table)
       ~input:(module Path)
       ~cutoff:String.equal
       (fun path -> with_file path ~f:Io.read_file))
;;

let state = State.t
let errors = State.errors
let record_deps (deps : Dep.Set.t) = Action_builder.record () deps ~f:build_dep
