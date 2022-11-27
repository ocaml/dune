open Import
open Memo.O
module Action_builder = Action_builder0

module Progress = struct
  type t =
    { number_of_rules_discovered : int
    ; number_of_rules_executed : int
    ; number_of_rules_failed : int
    }

  let equal
      { number_of_rules_discovered
      ; number_of_rules_executed
      ; number_of_rules_failed
      } t =
    Int.equal number_of_rules_discovered t.number_of_rules_discovered
    && Int.equal number_of_rules_executed t.number_of_rules_executed
    && Int.equal number_of_rules_failed t.number_of_rules_failed

  let init =
    { number_of_rules_discovered = 0
    ; number_of_rules_executed = 0
    ; number_of_rules_failed = 0
    }
end

module Error = struct
  module Id = Id.Make ()

  type t =
    { exn : Exn_with_backtrace.t
    ; id : Id.t
    }

  module Event = struct
    type nonrec t =
      | Add of t
      | Remove of t
  end

  let create ~exn = { exn; id = Id.gen () }

  let id t = t.id

  let promotion t =
    let e =
      match t.exn.exn with
      | Memo.Error.E e -> Memo.Error.get e
      | e -> e
    in
    match e with
    | User_error.E msg ->
      User_message.Annots.find msg.annots Diff_promotion.Annot.annot
    | _ -> None

  let info (t : t) =
    let e =
      match t.exn.exn with
      | Memo.Error.E e -> Memo.Error.get e
      | e -> e
    in
    match e with
    | User_error.E msg -> (
      let dir =
        User_message.Annots.find msg.annots Process.with_directory_annot
      in
      match User_message.Annots.find msg.annots Compound_user_error.annot with
      | None -> (msg, [], dir)
      | Some { main; related } -> (main, related, dir))
    | e ->
      (* CR-someday jeremiedimino: Use [Report_error.get_user_message] here. *)
      (User_message.make [ Pp.text (Printexc.to_string e) ], [], None)

  module Set : sig
    type error := t

    type nonrec t = private
      { current : t Id.Map.t
      ; stamp : int
      ; last_event : Event.t option
      }

    val add : t -> error -> t

    val one_event_diff : prev:t -> next:t -> Event.t option

    val equal : t -> t -> bool

    val current : t -> error Id.Map.t

    val empty : t
  end = struct
    type nonrec t =
      { current : t Id.Map.t
      ; stamp : int
      ; last_event : Event.t option
      }

    let add t error =
      let current = Id.Map.set t.current (id error) error in
      { current; stamp = t.stamp + 1; last_event = Some (Add error) }

    let equal t { current; stamp; last_event } =
      Int.equal t.stamp stamp
      &&
      match (t.last_event, last_event) with
      | None, None ->
        assert (Id.Map.is_empty t.current && Id.Map.is_empty current);
        true (* only possible when both sets are empty *)
      | Some x, Some y -> (
        match (x, y) with
        | Add x, Add y -> Id.equal x.id y.id
        | Add _, _ -> false
        | Remove x, Remove y -> Id.equal x.id y.id
        | Remove _, _ -> false)
      | Some _, None | None, Some _ -> false

    let one_event_diff ~prev ~next =
      if prev.stamp + 1 = next.stamp then next.last_event else None

    let current t = t.current

    let empty = { current = Id.Map.empty; stamp = 0; last_event = None }
  end
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
    match (x, y) with
    | Building x, Building y -> Progress.equal x y
    | Initializing, Initializing
    | Restarting_current_build, Restarting_current_build
    | ( Build_succeeded__now_waiting_for_changes
      , Build_succeeded__now_waiting_for_changes )
    | ( Build_failed__now_waiting_for_changes
      , Build_failed__now_waiting_for_changes ) -> true
    | Building _, _
    | Initializing, _
    | Restarting_current_build, _
    | Build_succeeded__now_waiting_for_changes, _
    | Build_failed__now_waiting_for_changes, _ -> false

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

  let incr_rule_done_exn () =
    update_build_progress_exn ~f:(fun p ->
        { p with number_of_rules_executed = p.number_of_rules_executed + 1 })

  let start_rule_exn () =
    update_build_progress_exn ~f:(fun p ->
        { p with number_of_rules_discovered = p.number_of_rules_discovered + 1 })

  let errors = Svar.create Error.Set.empty

  let reset_errors () = Svar.write errors Error.Set.empty

  let add_error error =
    let open Fiber.O in
    let* () =
      update_build_progress_exn ~f:(fun p ->
          { p with number_of_rules_failed = p.number_of_rules_failed + 1 })
    in
    Svar.write errors @@ Error.Set.add (Svar.read errors) error
end

let rec with_locks ~f = function
  | [] -> f ()
  | m :: mutexes ->
    Fiber.Mutex.with_lock
      (Table.find_or_add State.locks m ~f:(fun _ -> Fiber.Mutex.create ()))
      ~f:(fun () -> with_locks ~f mutexes)

(* All file targets of non-sandboxed actions that are currently being executed.
   On exit, we need to delete them as they might contain garbage. There is no
   [pending_dir_targets] since actions with directory targets are sandboxed. *)
let pending_file_targets = ref Path.Build.Set.empty

let () = Hooks.End_of_build.always Metrics.reset

let () =
  Hooks.End_of_build.always (fun () ->
      let fns = !pending_file_targets in
      pending_file_targets := Path.Build.Set.empty;
      Path.Build.Set.iter fns ~f:(fun p -> Path.Build.unlink_no_err p))

type rule_execution_result =
  { deps : Dep.Fact.t Dep.Map.t
  ; targets : Digest.t Path.Build.Map.t
  }

module type Rec = sig
  (** Build all the transitive dependencies of the alias and return the alias
      expansion. *)
  val build_alias : Alias.t -> Dep.Fact.Files.t Memo.t

  val build_file : Path.t -> Digest.t Memo.t

  val build_dir : Path.t -> (Digest.t * Digest.t Path.Build.Map.t) Memo.t

  val build_deps : Dep.Set.t -> Dep.Facts.t Memo.t

  val eval_deps :
    'a Action_builder.eval_mode -> Dep.Set.t -> 'a Dep.Map.t Memo.t

  val execute_rule : Rule.t -> rule_execution_result Memo.t

  val execute_action :
    observing_facts:Dep.Facts.t -> Rule.Anonymous_action.t -> unit Memo.t

  val execute_action_stdout :
    observing_facts:Dep.Facts.t -> Rule.Anonymous_action.t -> string Memo.t

  module Pred : sig
    val eval : File_selector.t -> Path.Set.t Memo.t

    val build : File_selector.t -> Dep.Fact.Files.t Memo.t
  end
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
    | Dir_target of { generated_file_digests : Digest.t Path.Build.Map.t }

  (* The below two definitions are useless, but if we remove them we get an
     "Undefined_recursive_module" exception. *)

  val build_file_memo : (Path.t, Digest.t * target_kind) Memo.Table.t
    [@@warning "-32"]

  val build_alias_memo : (Alias.t, Dep.Fact.Files.t) Memo.Table.t
    [@@warning "-32"]

  val dep_on_alias_definition :
    Rules.Dir_rules.Alias_spec.item -> unit Action_builder.t
end = struct
  open Used_recursively

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
    | File_selector g ->
      let+ digests = Pred.build g in
      (* Fact: file selector [g] expands to the set of file- and (possibly)
         dir-digest pairs [digests] *)
      Dep.Fact.file_selector g digests
    | Universe | Env _ ->
      (* Facts about these dependencies are constructed in
         [Dep.Facts.digest]. *)
      Memo.return Dep.Fact.nothing

  let build_deps deps =
    Dep.Map.parallel_map deps ~f:(fun dep () -> build_dep dep)

  let eval_deps :
      type a. a Action_builder.eval_mode -> Dep.Set.t -> a Dep.Map.t Memo.t =
   fun mode deps ->
    match mode with
    | Lazy -> Memo.return deps
    | Eager -> build_deps deps

  let select_sandbox_mode (config : Sandbox_config.t) ~loc
      ~sandboxing_preference =
    (* Rules with (mode patch-back-source-tree) are special and are not affected
       by sandboxing preferences. *)
    match Sandbox_mode.Set.is_patch_back_source_tree_only config with
    | true -> Some Sandbox_mode.Patch_back_source_tree
    | false -> (
      let evaluate_sandboxing_preference preference =
        match Sandbox_mode.Set.mem config preference with
        | false -> None
        | true -> Some preference
      in
      match
        List.find_map sandboxing_preference ~f:evaluate_sandboxing_preference
      with
      | Some choice -> choice
      | None ->
        (* This is not trivial to reach because the user rules are checked at
           parse time and [sandboxing_preference] always includes all possible
           modes. However, it can still be reached if multiple sandbox config
           specs are combined into an unsatisfiable one. *)
        User_error.raise ~loc
          [ Pp.text
              "This rule forbids all sandboxing modes (but it also requires \
               sandboxing)"
          ])

  (* The current version of the rule digest scheme. We should increment it when
     making any changes to the scheme, to avoid collisions. *)
  let rule_digest_version = 13

  let compute_rule_digest (rule : Rule.t) ~deps ~action ~sandbox_mode
      ~execution_parameters =
    let { Action.Full.action
        ; env
        ; locks
        ; can_go_in_shared_cache
        ; sandbox = _ (* already taken into account in [sandbox_mode] *)
        } =
      action
    in
    let file_targets =
      Path.Build.Set.to_list_map rule.targets.files ~f:Path.Build.to_string
    in
    let dir_targets =
      Path.Build.Set.to_list_map rule.targets.dirs ~f:Path.Build.to_string
    in
    let trace =
      ( rule_digest_version (* Update when changing the rule digest scheme. *)
      , sandbox_mode
      , Dep.Facts.digest deps ~env
      , file_targets @ dir_targets
      , Option.map rule.context ~f:(fun c -> Context_name.to_string c.name)
      , Action.for_shell action
      , can_go_in_shared_cache
      , List.map locks ~f:Path.to_string
      , Execution_parameters.action_stdout_on_success execution_parameters
      , Execution_parameters.action_stderr_on_success execution_parameters
      , Execution_parameters.add_workspace_root_to_build_path_prefix_map
          execution_parameters )
    in
    Digest.generic trace

  let report_evaluated_rule_exn (t : Build_config.t) =
    Option.iter t.stats ~f:(fun stats ->
        let module Event = Chrome_trace.Event in
        let event =
          let rule_total =
            match Fiber.Svar.read State.t with
            | Building progress -> progress.number_of_rules_discovered
            | _ -> assert false
          in
          let args = [ ("value", `Int rule_total) ] in
          let ts = Event.Timestamp.of_float_seconds (Unix.gettimeofday ()) in
          let common = Event.common_fields ~name:"evaluated_rules" ~ts () in
          Event.counter common args
        in
        Dune_stats.emit stats event)

  module Exec_result = struct
    type t =
      { produced_targets : unit Targets.Produced.t
      ; action_exec_result : Action_exec.Exec_result.t
      }
  end

  type rule_kind =
    | Normal_rule
    | Anonymous_action of
        { stamp_file : Path.Build.t
        ; capture_stdout : bool
        ; attached_to_alias : bool
        }

  let execute_action_for_rule ~rule_kind ~rule_digest ~action ~deps ~loc
      ~(context : Build_context.t option) ~execution_parameters ~sandbox_mode
      ~dir ~(targets : Targets.Validated.t) : Exec_result.t Fiber.t =
    let open Fiber.O in
    let { Action.Full.action
        ; env
        ; locks
        ; can_go_in_shared_cache = _
        ; sandbox = _
        } =
      action
    in
    let* dune_stats = Scheduler.stats () in
    let sandbox =
      match sandbox_mode with
      | Some mode ->
        Some
          (Sandbox.create ~mode ~deps ~rule_dir:dir ~rule_loc:loc ~rule_digest
             ~dune_stats
             ~expand_aliases:
               (Execution_parameters.expand_aliases_in_sandbox
                  execution_parameters))
      | None ->
        (* If the action is not sandboxed, we use [pending_file_targets] to
           clean up the build directory if the action is interrupted. *)
        pending_file_targets :=
          Path.Build.Set.union targets.files !pending_file_targets;
        None
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
        if capture_stdout then Action.with_stdout_to stamp_file action
        else Action.progn [ action; Action.write_file stamp_file "" ]
    in
    Action.chdirs action
    |> Path.Build.Set.iter ~f:(fun p -> Path.mkdir_p (Path.build p));
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
    let+ exec_result =
      with_locks locks ~f:(fun () ->
          let build_deps deps = Memo.run (build_deps deps) in
          let+ action_exec_result =
            Action_exec.exec ~root ~context ~env ~targets:(Some targets)
              ~rule_loc:loc ~build_deps ~execution_parameters action
          in
          let produced_targets =
            match sandbox with
            | None ->
              Targets.Produced.produced_after_rule_executed_exn ~loc targets
            | Some sandbox ->
              (* The stamp file for anonymous actions is always created outside
                 the sandbox, so we can't move it. *)
              let should_be_skipped =
                match rule_kind with
                | Normal_rule -> fun (_ : Path.Build.t) -> false
                | Anonymous_action { stamp_file; _ } ->
                  Path.Build.equal stamp_file
              in
              Sandbox.move_targets_to_build_dir sandbox ~loc ~should_be_skipped
                ~targets
          in
          { Exec_result.produced_targets; action_exec_result })
    in
    (match sandbox with
    | Some sandbox -> Sandbox.destroy sandbox
    | None ->
      (* All went well, these targets are no longer pending. *)
      pending_file_targets :=
        Path.Build.Set.diff !pending_file_targets targets.files);
    exec_result

  let promote_targets ~rule_mode ~dir ~targets ~promote_source =
    match (rule_mode, !Clflags.promote) with
    | (Rule.Mode.Standard | Fallback | Ignore_source_files), _
    | Promote _, Some Never -> Fiber.return ()
    | Promote promote, (Some Automatically | None) ->
      Target_promotion.promote ~dir ~targets ~promote ~promote_source

  let execution_parameters_of_dir =
    let f path =
      let+ dir = Source_tree.nearest_dir path
      and+ ep = Execution_parameters.default in
      Dune_project.update_execution_parameters (Source_tree.Dir.project dir) ep
    in
    let memo =
      Memo.create "execution-parameters-of-dir"
        ~input:(module Path.Source)
        ~cutoff:Execution_parameters.equal f
    in
    Memo.exec memo

  let execute_rule_impl ~rule_kind rule =
    let { Rule.id = _; targets; dir; context; mode; action; info = _; loc } =
      rule
    in
    (* We run [State.start_rule_exn ()] entirely for its side effect, so one
       might be tempted to use [Memo.of_non_reproducible_fiber] here but that is
       wrong, because that would force us to rerun [execute_rule_impl] on every
       incremental build. *)
    let* () = Memo.of_reproducible_fiber (State.start_rule_exn ()) in
    let head_target = Targets.Validated.head targets in
    let* execution_parameters =
      match Dpath.Target_dir.of_target dir with
      | Regular (With_context (_, dir))
      | Anonymous_action (With_context (_, dir)) ->
        execution_parameters_of_dir dir
      | _ -> Execution_parameters.default
    in
    (* Note: we do not run the below in parallel with the above: if we fail to
       compute action execution parameters, we have no use for the action and
       might as well fail early, skipping unnecessary dependencies. The function
       [Source_tree.execution_parameters_of_dir] is memoized, and the result is
       not expected to change often, so we do not sacrifice too much performance
       here by executing it sequentially. *)
    let* action, deps = Action_builder.run action Eager in
    let wrap_fiber f =
      Memo.of_reproducible_fiber
        (if Loc.is_none loc then f ()
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
        Path.mkdir_p (Path.build dir);
        let is_action_dynamic = Action.is_dynamic action.action in
        let sandbox_mode =
          match Action.is_useful_to_sandbox action.action with
          | Clearly_not ->
            if Sandbox_config.mem action.sandbox Sandbox_mode.none then
              Sandbox_mode.none
            else
              User_error.raise ~loc
                [ Pp.text
                    "Rule dependencies are configured to require sandboxing, \
                     but the rule has no actions that could potentially \
                     require sandboxing."
                ]
          | Maybe ->
            select_sandbox_mode ~loc action.sandbox
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
          force_rerun || Dep.Map.has_universe deps
        in
        let rule_digest =
          compute_rule_digest rule ~deps ~action ~sandbox_mode
            ~execution_parameters
        in
        (* CR-someday amokhov: Add support for rules with directory targets. *)
        let can_go_in_shared_cache =
          action.can_go_in_shared_cache
          && Path.Build.Set.is_empty targets.dirs
          && (not
                (always_rerun || is_action_dynamic
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
          Rule_cache.Workspace_local.lookup ~always_rerun ~rule_digest ~targets
            ~env:action.env ~build_deps
          >>= function
          | Some produced_targets -> Fiber.return produced_targets
          | None ->
            (* Step II. Remove stale targets both from the digest table and from
               the build directory. *)
            Path.Build.Set.iter targets.files ~f:(fun file ->
                Cached_digest.remove file;
                Path.Build.unlink_no_err file);
            Path.Build.Set.iter targets.dirs ~f:(fun dir ->
                Cached_digest.remove dir;
                Path.rm_rf (Path.build dir));
            let* produced_targets, dynamic_deps_stages =
              (* Step III. Try to restore artifacts from the shared cache. *)
              match
                Rule_cache.Shared.lookup ~can_go_in_shared_cache ~rule_digest
                  ~targets ~target_dir:rule.dir
              with
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
                  execute_action_for_rule ~rule_kind ~rule_digest ~action ~deps
                    ~loc ~context ~execution_parameters ~sandbox_mode ~dir
                    ~targets
                in
                (* Step V. Examine produced targets and store them to the shared
                   cache if needed. *)
                let* produced_targets =
                  Rule_cache.Shared.examine_targets_and_store
                    ~can_go_in_shared_cache ~loc ~rule_digest
                    ~execution_parameters
                    ~produced_targets:exec_result.produced_targets
                    ~action:action.action
                in
                let dynamic_deps_stages =
                  List.map exec_result.action_exec_result.dynamic_deps_stages
                    ~f:(fun (deps, fact_map) ->
                      (deps, Dep.Facts.digest fact_map ~env:action.env))
                in
                Fiber.return (produced_targets, dynamic_deps_stages)
            in
            (* We do not include target names into [targets_digest] because they
               are already included into the rule digest. *)
            Rule_cache.Workspace_local.store ~head_target ~rule_digest
              ~dynamic_deps_stages
              ~targets_digest:(Targets.Produced.digest produced_targets);
            Fiber.return produced_targets
        in
        let* () =
          promote_targets ~rule_mode:mode ~dir ~targets:produced_targets
            ~promote_source:(config.promote_source context)
        in
        let+ () = State.incr_rule_done_exn () in
        produced_targets)
    (* jeremidimino: We need to include the dependencies discovered while
       running the action here. Otherwise, package dependencies are broken in
       the presence of dynamic actions. *)
    >>|
    fun produced_targets ->
    { deps; targets = Targets.Produced.all_files produced_targets }

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
      Record
        [ ("digest", Digest.to_dyn t.digest)
        ; ("loc", Dyn.option Loc.to_dyn t.action.loc)
        ]
  end

  let execute_action_generic_stage2_impl
      { Anonymous_action.action = act; deps; capture_stdout; digest } =
    let target =
      let dir =
        Path.Build.append_local Dpath.Build.anonymous_actions_dir
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
      let { Rule.Anonymous_action.context; action = _; loc; dir = _; alias = _ }
          =
        act
      in
      Rule.make ~context
        ~info:
          (match loc with
          | Some loc -> From_dune_file loc
          | None -> Internal)
        ~targets:(Targets.File.create target)
        ~mode:Standard
        (Action_builder.of_thunk
           { f =
               (fun mode ->
                 let+ deps = eval_deps mode deps in
                 (act.action, deps))
           })
    in
    let+ { deps = _; targets = _ } =
      execute_rule_impl rule
        ~rule_kind:
          (Anonymous_action
             { attached_to_alias = Option.is_some act.alias
             ; capture_stdout
             ; stamp_file = target
             })
    in
    target

  let execute_action_generic_stage2_memo =
    Memo.create "execute-action"
      ~input:(module Anonymous_action)
      (* this memo doesn't need cutoff because the input's digests
         fully determines the returned build path and we already compare the
         input using this digest *)
      execute_action_generic_stage2_impl

  let execute_action_generic ~observing_facts (act : Rule.Anonymous_action.t)
      ~capture_stdout =
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
      let { Rule.Anonymous_action.context
          ; action = { action; env; locks; can_go_in_shared_cache; sandbox }
          ; loc
          ; dir
          ; alias
          } =
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
        ( Option.map context ~f:(fun c ->
              (* Only looking at the context name is fishy, but it is in line
                 with what we do for build rules. *)
              Context_name.to_string c.name)
        , env
        , Dep.Set.digest deps
        , Action.for_shell action
        , List.map locks ~f:Path.to_string
        , loc
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
    Memo.exec execute_action_generic_stage2_memo
      { action = act; deps; capture_stdout; digest }

  let execute_action ~observing_facts act =
    let+ _target =
      execute_action_generic ~observing_facts act ~capture_stdout:false
    in
    ()

  let execute_action_stdout ~observing_facts act =
    let+ target =
      execute_action_generic ~observing_facts act ~capture_stdout:true
    in
    Io.read_file (Path.build target)

  type target_kind =
    | File_target
    | Dir_target of { generated_file_digests : Digest.t Path.Build.Map.t }

  let target_kind_equal a b =
    match (a, b) with
    | File_target, File_target -> true
    | ( Dir_target { generated_file_digests = a }
      , Dir_target { generated_file_digests = b } ) ->
      Path.Build.Map.equal a b ~equal:Digest.equal
    | File_target, Dir_target _ | Dir_target _, File_target -> false

  (* A rule can have multiple targets but calls to [execute_rule] are memoized,
     so the rule will be executed only once. *)
  let build_file_impl path =
    Load_rules.get_rule_or_source path >>= function
    | Source digest -> Memo.return (digest, File_target)
    | Rule (path, rule) -> (
      let+ { deps = _; targets } =
        Memo.push_stack_frame
          (fun () -> execute_rule rule)
          ~human_readable_description:(fun () ->
            Pp.text (Path.to_string_maybe_quoted (Path.build path)))
      in
      match Path.Build.Map.find targets path with
      | Some digest -> (digest, File_target)
      | None -> (
        match Cached_digest.build_file ~allow_dirs:true path with
        | Ok digest ->
          (digest, Dir_target { generated_file_digests = targets })
          (* Must be a directory target *)
        | No_such_file
        | Broken_symlink
        | Cyclic_symlink
        | Unexpected_kind _
        | Unix_error _
        | Error _ ->
          (* CR-someday amokhov: The most important reason we end up here is
             [No_such_file]. I think some of the outcomes above are impossible
             but some others will benefit from a better error. To be refined. *)
          let target =
            Path.Build.drop_build_context_exn path
            |> Path.Source.to_string_maybe_quoted
          in
          let matching_dirs =
            Path.Build.Set.to_list_map rule.targets.dirs ~f:(fun dir ->
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
              Code_error.raise "Multiple matching directory targets"
                [ ("targets", Targets.Validated.to_dyn rule.targets) ]
          in
          User_error.raise ~loc:rule.loc
            ~annots:
              (User_message.Annots.singleton
                 User_message.Annots.needs_stack_trace ())
            [ Pp.textf
                "This rule defines a directory target %S that matches the \
                 requested path %S but the rule's action didn't produce it"
                matching_target target
            ]))

  let dep_on_anonymous_action (x : Rule.Anonymous_action.t Action_builder.t) :
      _ Action_builder.t =
    Action_builder.of_thunk
      { f =
          (fun (type m) (mode : m Action_builder.eval_mode) ->
            match mode with
            | Lazy -> Memo.return ((), Dep.Map.empty)
            | Eager ->
              let* action, facts = Action_builder.run x Eager in
              let+ () = execute_action action ~observing_facts:facts in
              ((), Dep.Map.empty))
      }

  let dep_on_alias_definition (definition : Rules.Dir_rules.Alias_spec.item) =
    match definition with
    | Deps x -> x
    | Action x -> dep_on_anonymous_action x

  let build_alias_impl alias =
    let+ l =
      Load_rules.get_alias_definition alias
      >>= Memo.parallel_map ~f:(fun (loc, definition) ->
              Memo.push_stack_frame
                (fun () ->
                  Action_builder.run (dep_on_alias_definition definition) Eager
                  >>| snd)
                ~human_readable_description:(fun () ->
                  Alias.describe alias ~loc))
    in
    Dep.Facts.group_paths_as_fact_files l

  module Pred = struct
    let build_impl g =
      let dir = File_selector.dir g in
      Load_rules.load_dir ~dir >>= function
      | External _ | Source _ | Build _ ->
        let* paths = Pred.eval g in
        let+ files =
          Memo.parallel_map (Path.Set.to_list paths) ~f:(fun p ->
              let+ d = build_file p in
              (p, d))
        in
        Dep.Fact.Files.make
          ~files:(Path.Map.of_list_exn files)
          ~dirs:Path.Map.empty
      | Build_under_directory_target _ ->
        let* digest, path_map = build_dir dir in
        let files =
          Path.Build.Map.foldi path_map ~init:Path.Map.empty
            ~f:(fun path digest acc ->
              let parent = Path.Build.parent_exn path |> Path.build in
              let path = Path.build path in
              match Path.equal parent dir && File_selector.test g path with
              | true -> Path.Map.add_exn acc path digest
              | false -> acc)
        in
        let dirs = Path.Map.singleton dir digest in
        Memo.return (Dep.Fact.Files.make ~files ~dirs)

    let eval_impl g =
      let dir = File_selector.dir g in
      Load_rules.load_dir ~dir >>= function
      | Source { files } ->
        Path.set_of_source_paths files
        |> Path.Set.filter ~f:(File_selector.test g)
        |> Memo.return
      | External { files } ->
        Path.set_of_external_paths files
        |> Path.Set.filter ~f:(File_selector.test g)
        |> Memo.return
      | Build { rules_here; _ } ->
        let only_generated_files = File_selector.only_generated_files g in
        (* We look only at [by_file_targets] because [File_selector] does not
           match directories. *)
        Path.Build.Map.foldi ~init:[] rules_here.by_file_targets
          ~f:(fun s { Rule.info; _ } acc ->
            match info with
            | Rule.Info.Source_file_copy _ when only_generated_files -> acc
            | _ ->
              let s = Path.build s in
              if File_selector.test g s then s :: acc else acc)
        |> Path.Set.of_list |> Memo.return
      | Build_under_directory_target _ ->
        (* To evaluate a glob in a generated directory, we have no choice but to
           build the whole directory, so we might as well build the
           predicate. *)
        let+ fact = Pred.build g in
        Dep.Fact.Files.paths fact |> Path.Set.of_keys

    let eval_memo =
      Memo.create "eval-pred"
        ~human_readable_description:(fun glob ->
          Pp.concat
            [ Pp.textf "Evaluating predicate in directory %s"
                (Path.to_string_maybe_quoted (File_selector.dir glob))
            ])
        ~input:(module File_selector)
        ~cutoff:Path.Set.equal eval_impl

    let eval = Memo.exec eval_memo

    let build =
      Memo.exec
        (Memo.create "build-pred"
           ~input:(module File_selector)
           ~cutoff:Dep.Fact.Files.equal build_impl)
  end

  let build_file_memo =
    let cutoff = Tuple.T2.equal Digest.equal target_kind_equal in
    Memo.create "build-file" ~input:(module Path) ~cutoff build_file_impl

  let build_file path = Memo.exec build_file_memo path >>| fst

  let build_dir path =
    let+ digest, kind = Memo.exec build_file_memo path in
    match kind with
    | Dir_target { generated_file_digests } -> (digest, generated_file_digests)
    | File_target ->
      Code_error.raise "build_dir called on a file target"
        [ ("path", Path.to_dyn path) ]

  let build_alias_memo =
    Memo.create "build-alias"
      ~input:(module Alias)
      ~cutoff:Dep.Fact.Files.equal build_alias_impl

  let build_alias = Memo.exec build_alias_memo

  let execute_rule_memo =
    Memo.create "execute-rule"
      ~input:(module Rule)
      (execute_rule_impl ~rule_kind:Normal_rule)

  let execute_rule = Memo.exec execute_rule_memo

  let () =
    Load_rules.set_current_rule_loc (fun () ->
        let+ stack = Memo.get_call_stack () in
        List.find_map stack ~f:(fun frame ->
            match
              Memo.Stack_frame.as_instance_of frame ~of_:execute_rule_memo
            with
            | Some r -> Some (Rule.loc r)
            | None ->
              Option.bind
                (Memo.Stack_frame.as_instance_of frame
                   ~of_:execute_action_generic_stage2_memo) ~f:(fun x ->
                  x.action.Rule.Anonymous_action.loc)))
end

include Exported

let eval_pred = Pred.eval

let build_pred = Pred.build

(* Here we are doing a O(log |S|) lookup in a set S of files in the build
   directory [dir]. We could memoize these lookups, but it doesn't seem to be
   worth it, since we're unlikely to perform exactly the same lookup many times.
   As far as I can tell, each lookup will be done twice: when computing static
   dependencies of a [Action_builder.t] with [Action_builder.static_deps] and
   when executing the very same [Action_builder.t] with [Action_builder.exec] --
   the results of both [Action_builder.static_deps] and [Action_builder.exec]
   are cached. *)
let file_exists fn =
  Load_rules.load_dir ~dir:(Path.parent_exn fn) >>= function
  | Source { files } ->
    Memo.return
      (match Path.as_in_source_tree fn with
      | None -> false
      | Some file -> Path.Source.Set.mem files file)
  | External { files } ->
    Memo.return
      (match Path.as_external fn with
      | None -> false
      | Some file -> Path.External.Set.mem files file)
  | Build { rules_here; _ } ->
    Memo.return
      (Path.Build.Map.mem rules_here.by_file_targets
         (Path.as_in_build_dir_exn fn))
  | Build_under_directory_target { directory_target_ancestor } ->
    let+ _digest, path_map = build_dir (Path.build directory_target_ancestor) in
    Path.Build.Map.mem path_map (Path.as_in_build_dir_exn fn)

let files_of ~dir =
  Load_rules.load_dir ~dir >>= function
  | Source { files } -> Memo.return (Path.set_of_source_paths files)
  | External { files } -> Memo.return (Path.set_of_external_paths files)
  | Build { rules_here; _ } ->
    Memo.return
      (Path.Build.Map.keys rules_here.by_file_targets
      |> Path.Set.of_list_map ~f:Path.build)
  | Build_under_directory_target { directory_target_ancestor } ->
    let+ _digest, path_map = build_dir (Path.build directory_target_ancestor) in
    let dir = Path.as_in_build_dir_exn dir in
    Path.Build.Map.foldi path_map ~init:Path.Set.empty
      ~f:(fun path _digest acc ->
        let parent = Path.Build.parent_exn path in
        match Path.Build.equal parent dir with
        | true -> Path.Set.add acc (Path.build path)
        | false -> acc)

let caused_by_cancellation (exn : Exn_with_backtrace.t) =
  match exn.exn with
  | Scheduler.Run.Build_cancelled -> true
  | Memo.Error.E err -> (
    match Memo.Error.get err with
    | Scheduler.Run.Build_cancelled -> true
    | _ -> false)
  | _ -> false

let report_early_exn exn =
  match caused_by_cancellation exn with
  | true -> Fiber.return ()
  | false -> (
    let open Fiber.O in
    let error = Error.create ~exn in
    let+ () = State.add_error error in
    match !Clflags.report_errors_config with
    | Early | Twice -> Dune_util.Report_error.report exn
    | Deterministic -> ())

let handle_final_exns exns =
  match !Clflags.report_errors_config with
  | Early -> ()
  | Twice | Deterministic ->
    let report exn =
      if not (caused_by_cancellation exn) then Dune_util.Report_error.report exn
    in
    List.iter exns ~f:report

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
    match res with
    | Ok res ->
      let+ () = State.set Build_succeeded__now_waiting_for_changes in
      Ok res
    | Error exns ->
      handle_final_exns exns;
      let final_status =
        if List.exists exns ~f:caused_by_cancellation then
          State.Restarting_current_build
        else Build_failed__now_waiting_for_changes
      in
      let+ () = State.set final_status in
      Error `Already_reported
  in
  Fiber.Mutex.with_lock State.build_mutex ~f

let run_exn f =
  let open Fiber.O in
  let+ res = run f in
  match res with
  | Ok res -> res
  | Error `Already_reported -> raise Dune_util.Report_error.Already_reported

let build_file p =
  let+ (_ : Digest.t) = build_file p in
  ()

let read_file p ~f =
  let+ () = build_file p in
  f p

let state = State.t

let errors = State.errors
